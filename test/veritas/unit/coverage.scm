;; veritas

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; veritas is free software: you can redistribute it and/or modify
;; it under the terms of the Lesser GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; veritas is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; Lesser GNU General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with veritas.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; These tests do not trust the coverage of the suite they belong to: they
;; compile a module of their own, run part of it under `collect-coverage', and
;; check the numbers that come back.  The lesson behind them is that Guile only
;; counts instructions on the debugging VM, so a run that forgets to switch the
;; engine still writes a report - one where every line is zero.  The fixture has
;; a branch that is never taken and a procedure that is never called, so a
;; report of all zeroes cannot pass by accident.

(define-module (veritas unit coverage)
  #:use-module (veritas veritas)
  #:use-module (veritas coverage)
  #:use-module (system vm vm)
  #:use-module (system base compile)
  #:use-module (json)
  #:use-module (sxml simple)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (spec))

;; ====== Assertion shorthand ======
;;
;; A case lambda is handed the validation client, so the checks below apply one
;; assertion to it.  Keeping that in a procedure keeps the nesting - and the
;; parentheses - out of the tests themselves.

(define (check-true vc value)
  "Assert that @var{value} is true, through @var{vc}."
  ((assert-true value) vc))

(define (check-false vc value)
  "Assert that @var{value} is false, through @var{vc}."
  ((assert-false value) vc))

(define* (check-equal vc got #:key (expect #f))
  "Assert that @var{got} equals @var{expect}, through @var{vc}."
  ((assert-equal #:expect expect #:got got) vc))

(define (contains? text needle)
  "Return @code{#t} when @var{text} holds @var{needle}.

@code{string-contains} returns the index of the match, which @code{assert-true}
rightly refuses to take for a boolean."
  (if (string-contains text needle) #t #f))

;; ====== Reading XML back ======
;;
;; The Cobertura and JaCoCo documents are checked by parsing them with SXML,
;; not by matching strings: what matters is the element the format requires,
;; in the place the format requires it.

(define (find-node name tree)
  "Return the first SXML node named @var{name} in @var{tree}, or @code{#f}."
  (cond
    ((not (pair? tree)) #f)
    ((and (symbol? (car tree)) (eq? name (car tree))) tree)
    (else (or (find-node name (car tree))
              (find-node name (cdr tree))))))

(define (all-nodes name tree)
  "Return every SXML node named @var{name} in @var{tree}."
  (cond
    ((not (pair? tree)) '())
    ((and (symbol? (car tree)) (eq? name (car tree))) (list tree))
    (else (append (all-nodes name (car tree))
                  (all-nodes name (cdr tree))))))

(define (attribute name node)
  "Return the @var{name} attribute of the SXML @var{node}, or @code{#f}."
  (let ((attributes (and (pair? node)
                         (pair? (cdr node))
                         (pair? (cadr node))
                         (eq? '@ (car (cadr node)))
                         (cdr (cadr node)))))
    (and attributes
         (let ((pair (assq name attributes)))
           (and pair (cadr pair))))))

(define (node-text tree)
  "Return the text content of the SXML @var{tree}, concatenated."
  (cond
    ((string? tree) tree)
    ((not (pair? tree)) "")
    (else (apply string-append
                 (map node-text
                      (if (and (pair? (cdr tree))
                               (pair? (cadr tree))
                               (eq? '@ (car (cadr tree))))
                          (cddr tree)
                          (cdr tree)))))))

;; ====== A fixture module of our own ======

(define fixture-lines
  '("(define-module (fixture-module) #:export (classify unseen))"
    "(define (classify x)"
    "  (if (> x 0)"
    "      'pos"
    "      'non-pos))"
    "(define (unseen)"
    "  'never)"))

;; The lines of the fixture above that these tests look at.
(define fixture-line-taken 4)
(define fixture-line-not-taken 5)
(define fixture-line-never-called 7)

(define (make-temp-dir)
  "Return a fresh directory under the system temporary directory."
  (mkdtemp "/tmp/veritas-coverage-XXXXXX"))

(define (write-lines path lines)
  "Write @var{lines} to @var{path}, one per line."
  (call-with-output-file path
    (lambda (port)
      (for-each (lambda (line)
                  (display line port)
                  (newline port))
                lines))))

(define (remove-tree path)
  "Delete @var{path} and everything under it, ignoring anything that goes wrong."
  (catch #t
         (lambda ()
           (for-each (lambda (entry)
                       (unless (member entry '("." ".."))
                         (let ((child (in-vicinity path entry)))
                           (if (eq? 'directory (stat:type (lstat child)))
                               (remove-tree child)
                               (delete-file child)))))
                     (scandir path))
           (rmdir path))
         (lambda (key . args) #f)))

(define (call-with-coverage-allowed thunk)
  "Call @var{thunk} with @code{VERITAS_COVERAGE} unset, restoring it afterwards.

The end-to-end test asks for measurement explicitly; this is the platform that
lets it do that even when the surrounding run refused coverage
(@code{--no-coverage}, or @code{maak test-fast}).  The value is put back on the
way out, on the exception path too, so the rest of the suite sees the
environment it started with."
  (let ((previous (getenv "VERITAS_COVERAGE")))
    (define (restore!)
      (if previous
          (setenv "VERITAS_COVERAGE" previous)
          (unsetenv "VERITAS_COVERAGE")))
    (unsetenv "VERITAS_COVERAGE")
    (catch #t
           (lambda ()
             (let ((result (thunk)))
               (restore!)
               result))
           (lambda (key . args)
             (restore!)
             (apply throw key args)))))

(define (call-with-temporary-directory thunk)
  "Call @var{thunk} with a fresh directory, removing it afterwards.

The directory goes away after @var{thunk} returns, so a test can end with its
last assertion - which is what a case lambda has to return - instead of with the
cleanup.

The cleanup is in the exception path rather than in a @code{dynamic-wind}
after-thunk because fibers unwinds and re-enters a @code{dynamic-wind} whenever
a fiber is suspended, and opening a file is enough to suspend one: the directory
would be deleted while the test was still running."
  (let ((directory (make-temp-dir)))
    (catch #t
           (lambda ()
             (let ((result (thunk directory)))
               (remove-tree directory)
               result))
           (lambda (key . args)
             (remove-tree directory)
             (apply throw key args)))))

(define fixture-directory (make-temp-dir))

(define fixture-module-name
  (string->symbol (format #f "veritas-coverage-fixture-~a" (getpid))))

(define fixture-source
  (in-vicinity fixture-directory (format #f "~a.scm" fixture-module-name)))

(define fixture-compiled
  (string-append fixture-source ".go"))

;; Compile the fixture and load it now, before the suites run, which is what a
;; project does: coverage counts what runs, not what was loaded.
(write-lines fixture-source
             (cons (format #f "(define-module (~a) #:export (classify unseen))"
                           fixture-module-name)
                   (cdr fixture-lines)))
(compile-file fixture-source #:output-file fixture-compiled)
;; Loading a compiled file runs the fixture's `define-module', which leaves the
;; loader's current module pointing at the fixture.  Every form after this line -
;; the suites below - would then be evaluated there, where `suite' means
;; nothing.  Saving the excursion keeps the rest of this file in its own module.
(save-module-excursion
 (lambda ()
   (load-compiled fixture-compiled)))

(define (fixture-classify x)
  "Call the fixture's @code{classify}, which the coverage tests measure."
  ((module-ref (resolve-module (list fixture-module-name)) 'classify) x))

(define (sample-report path lines)
  "Return a one-file report of @var{path} with @var{lines}."
  (make-coverage-report (list (make-coverage-file path lines))))

;; ====== The suites ======

(define measurement-suite
  (suite "Coverage measurement"
         #:shuffle? #f
         (test "lines that ran are counted, lines that did not are zero"
               (lambda (vc)
                 (call-with-values
                  (lambda ()
                    (collect-coverage (lambda ()
                                        (fixture-classify 1))
                                      #:include (list fixture-directory)))
                  (lambda (report result)
                    (check-equal vc (length (coverage-report-files report))
                                 #:expect 1)
                    (let ((file (car (coverage-report-files report))))
                      (check-equal vc (coverage-file-path file)
                                   #:expect fixture-source))
                    (check-true vc (> (coverage-report-line-count
                                       report fixture-source
                                       fixture-line-taken)
                                      0))
                    (check-equal vc (coverage-report-line-count
                                     report fixture-source
                                     fixture-line-not-taken)
                                 #:expect 0)
                    (check-equal vc (coverage-report-line-count
                                     report fixture-source
                                     fixture-line-never-called)
                                 #:expect 0)
                    (check-true vc (< (coverage-report-covered report)
                                      (coverage-report-instrumented report)))
                    (check-true vc (> (coverage-report-percentage report) 0))
                    (check-true vc (< (coverage-report-percentage report)
                                      100))))))
         (test "collecting coverage leaves the VM engine as it found it"
               (lambda (vc)
                 (let ((before (vm-engine))
                       (during #f))
                   (collect-coverage (lambda ()
                                       (set! during (vm-engine))
                                       'done))
                   (check-equal vc during #:expect 'debug)
                   (check-equal vc (vm-engine) #:expect before))))
         (test "an empty collection is an empty report, not a covered one"
               (lambda (vc)
                 (call-with-values
                  (lambda ()
                    (collect-coverage (lambda () 'nothing)
                                      #:include (list "/nonexistent/veritas")))
                  (lambda (report result)
                    (check-equal vc (length (coverage-report-files report))
                                 #:expect 0)
                    (check-equal vc (coverage-report-percentage report)
                                 #:expect 0.0)))))))

(define filtering-suite
  (suite "Coverage filtering"
         #:shuffle? #f
         (test "the include filter is what keeps dependencies out"
               (lambda (vc)
                 (check-true vc (coverage-path-included?
                                 "/project/src/cart.scm"
                                 #:include '("/project")))
                 (check-false vc (coverage-path-included?
                                  "/gnu/store/guile/fibers.scm"
                                  #:include '("/project")))
                 (check-false vc (coverage-path-included?
                                  "/project/test/cart.scm"
                                  #:include '("/project")
                                  #:exclude '("/project/test")))
                 (check-true vc (coverage-path-included?
                                 "/project/src/cart.scm"
                                 #:include '("/project")
                                 #:exclude '("/project/test")))
                 ;; A directory whose name merely starts with the excluded
                 ;; prefix is not inside it.
                 (check-true vc (coverage-path-included?
                                 "/project/testing/cart.scm"
                                 #:include '("/project")
                                 #:exclude '("/project/test")))
                 ;; No include means everything.
                 (check-true vc (coverage-path-included?
                                 "/anywhere/cart.scm"))))
         (test "an explicit refusal beats a file that asks for coverage"
               (lambda (vc)
                 ;; The environment value is passed in, so this test says what
                 ;; it means without changing the environment the rest of the
                 ;; suite runs in.
                 (check-true vc (coverage-enabled-by-environment?
                                 #:value "true"))
                 (check-true vc (coverage-enabled-by-environment?
                                 #:value "1"))
                 (check-false vc (coverage-enabled-by-environment?
                                  #:value "false"))
                 (check-false vc (coverage-enabled-by-environment?
                                  #:value #f))
                 (check-true vc (coverage-disabled-by-environment?
                                 #:value "false"))
                 (check-true vc (coverage-disabled-by-environment?
                                 #:value "0"))
                 (check-false vc (coverage-disabled-by-environment?
                                  #:value "true"))
                 (check-false vc (coverage-disabled-by-environment?
                                  #:value #f))))
         (test "a recorded path becomes an absolute one"
               (lambda (vc)
                 (check-equal vc (normalize-coverage-path "/tmp/veritas/x.scm")
                              #:expect "/tmp/veritas/x.scm")
                 (check-equal vc (normalize-coverage-path
                                  "sub/x.scm" #:base "/tmp/veritas")
                              #:expect "/tmp/veritas/sub/x.scm")
                 (check-equal vc (normalize-coverage-path
                                  "sub/../x.scm" #:base "/tmp/veritas")
                              #:expect "/tmp/veritas/x.scm")
                 (call-with-temporary-directory
                  (lambda (dir)
                    (let ((path (in-vicinity dir "resolved.scm")))
                      (write-lines path '(";; a file found on the load path"))
                      (dynamic-wind
                       (lambda () (set! %load-path (cons dir %load-path)))
                       (lambda ()
                         ;; The compiler records a module relative to the load
                         ;; path entry it was found in, not to the working
                         ;; directory.
                         (check-equal vc (normalize-coverage-path
                                          "resolved.scm"
                                          #:base "/does/not/exist")
                                      #:expect path))
                       (lambda ()
                         (set! %load-path
                               (remove (lambda (entry) (string=? entry dir))
                                       %load-path)))))))))))

(define report-suite
  (suite "Coverage reports"
         #:shuffle? #f
         (test "an LCOV file survives a round trip"
               (lambda (vc)
                 (call-with-temporary-directory
                  (lambda (dir)
                    (let* ((path (in-vicinity dir "round-trip.info"))
                           (report (make-coverage-report
                                    (list (make-coverage-file
                                           "/project/a.scm"
                                           '((1 . 2) (2 . 0)))
                                          (make-coverage-file
                                           "/project/b.scm"
                                           '((5 . 1))))))
                           (text (lcov-string report)))
                      (write-lcov-file report path)
                      (check-equal vc (lcov-string (read-lcov-file path))
                                   #:expect text)
                      (check-equal vc (length (coverage-report-files
                                               (read-lcov-file path)))
                                   #:expect 2)
                      ;; One record per file, with that file's own totals.
                      (check-true vc (contains? text "SF:/project/a.scm"))
                      (check-true vc (contains? text "SF:/project/b.scm"))
                      (check-true vc (contains? text "DA:2,0"))
                      (check-true vc (contains? text "LF:2"))
                      ;; A file that is not there is an empty report.
                      (check-equal vc (length (coverage-report-files
                                               (merge-lcov-files
                                                (list (in-vicinity
                                                       dir "missing.info")))))
                                   #:expect 0))))))
         (test "reports merge by taking the highest count"
               (lambda (vc)
                 (let* ((first (sample-report "/project/a.scm"
                                              '((1 . 2) (2 . 0))))
                        (second (sample-report "/project/a.scm"
                                               '((1 . 5) (3 . 1))))
                        (merged (merge-coverage-reports first second)))
                   (check-equal vc (coverage-report-line-count
                                    merged "/project/a.scm" 1)
                                #:expect 5)
                   (check-equal vc (coverage-report-line-count
                                    merged "/project/a.scm" 2)
                                #:expect 0)
                   (check-equal vc (coverage-report-line-count
                                    merged "/project/a.scm" 3)
                                #:expect 1)
                   (check-equal vc (coverage-report-line-count
                                    (merge-coverage-reports first first)
                                    "/project/a.scm" 1)
                                #:expect 2)
                   (check-equal vc (length (coverage-report-files
                                            (merge-coverage-reports
                                             first second)))
                                #:expect 1)
                   ;; A file seen by either report is in the merge, once.
                   (check-equal vc (length (coverage-report-files
                                            (merge-coverage-reports
                                             first
                                             (sample-report "/project/b.scm"
                                                            '((1 . 1))))))
                                #:expect 2))))
         (test "the HTML report is one page with every file in it"
               (lambda (vc)
                 (call-with-temporary-directory
                  (lambda (dir)
                    (let* ((first (in-vicinity dir "a.scm"))
                           (second (in-vicinity dir "nested/b.scm"))
                           (html (in-vicinity dir "html"))
                           (report (make-coverage-report
                                    (list (make-coverage-file
                                           first '((1 . 3) (2 . 0) (3 . 1)))
                                          (make-coverage-file
                                           second '((1 . 1) (2 . 0)))))))
                      (mkdir (in-vicinity dir "nested"))
                      (write-lines first '("(first line)"
                                           "(<second> line)"
                                           "(third line)"))
                      (write-lines second '("(nested first)"
                                            "(nested second)"))
                      (let ((index (write-html-report report
                                                      #:directory html)))
                        (check-equal vc index
                                     #:expect (in-vicinity html
                                                           "index.html"))
                        (check-true vc (file-exists? index))
                        ;; Everything is in that one file: no per-file pages
                        ;; are left behind.
                        (check-equal vc (length (scandir html))
                                     #:expect 3))
                      (let ((page (call-with-input-file
                                      (in-vicinity html "index.html")
                                    get-string-all)))
                        ;; Both files, and both sources, are on the page.
                        (check-true vc (contains? page first))
                        (check-true vc (contains? page second))
                        (check-true vc (contains? page "(third line)"))
                        (check-true vc (contains? page "(nested second)"))
                        ;; The summary, the filter and the toggle are there.
                        (check-true vc (contains? page "id=\"filter\""))
                        (check-true vc (contains? page "id=\"hide-covered\""))
                        (check-true vc (contains? page "60.0%"))
                        ;; The line that never ran is marked, the ones that
                        ;; did run are marked differently, and markup in the
                        ;; source is escaped instead of being taken for HTML.
                        (check-true vc (contains? page "class=\"miss\""))
                        (check-true vc (contains? page "class=\"hit\""))
                        (check-true vc (contains? page "&lt;second&gt;"))
                        (check-false vc (contains? page "<second>"))))))))
         (test "the Cobertura document describes packages, classes and lines"
               (lambda (vc)
                 (call-with-temporary-directory
                  (lambda (dir)
                    (let* ((source (in-vicinity dir "src/a.scm"))
                           (path (in-vicinity dir "cobertura.xml"))
                           (report (sample-report source '((1 . 2) (2 . 0)
                                                           (3 . 1)))))
                      (mkdir (in-vicinity dir "src"))
                      (write-lines source '("(define a 1)"
                                            "(define b 2)"
                                            "(+ a b)"))
                      (write-cobertura-xml report
                                           #:path path
                                           #:sources (list dir))
                      (let* ((document (xml->sxml
                                        (call-with-input-file path
                                          get-string-all)))
                             (coverage (find-node 'coverage document))
                             (sources (find-node 'sources document))
                             (package (find-node 'package document))
                             (class (find-node 'class document))
                             (lines (all-nodes 'line document)))
                        ;; The rate is a fraction, and the totals add up.
                        (check-equal vc (attribute 'line-rate coverage)
                                     #:expect "0.6667")
                        (check-equal vc (attribute 'lines-covered coverage)
                                     #:expect "2")
                        (check-equal vc (attribute 'lines-valid coverage)
                                     #:expect "3")
                        (check-equal vc (attribute 'branch-rate coverage)
                                     #:expect "0.0")
                        (check-true vc (contains? (node-text sources) dir))
                        (check-equal vc (attribute 'name package)
                                     #:expect "src")
                        (check-equal vc (attribute 'filename class)
                                     #:expect "src/a.scm")
                        (check-equal vc (length lines) #:expect 3)
                        (check-equal vc (attribute 'hits (car lines))
                                     #:expect "2")
                        (check-equal vc (attribute 'hits (cadr lines))
                                     #:expect "0")))))))
         (test "the JaCoCo document carries per-line and per-class counters"
               (lambda (vc)
                 (call-with-temporary-directory
                  (lambda (dir)
                    (let* ((source (in-vicinity dir "src/a.scm"))
                           (path (in-vicinity dir "jacoco.xml"))
                           (report (sample-report source '((1 . 2) (2 . 0)
                                                           (3 . 1)))))
                      (mkdir (in-vicinity dir "src"))
                      (write-lines source '("(define a 1)"
                                            "(define b 2)"
                                            "(+ a b)"))
                      (write-jacoco-xml report
                                        #:path path
                                        #:sources (list dir))
                      (let* ((document (xml->sxml
                                        (call-with-input-file path
                                          get-string-all)))
                             (report-node (find-node 'report document))
                             (package (find-node 'package document))
                             (class (find-node 'class document))
                             (sourcefile (find-node 'sourcefile document))
                             (lines (all-nodes 'line document))
                             (counters (all-nodes 'counter sourcefile)))
                        (check-equal vc (attribute 'name report-node)
                                     #:expect "veritas")
                        (check-equal vc (attribute 'name package)
                                     #:expect "src")
                        (check-equal vc (attribute 'name class)
                                     #:expect "src/a")
                        (check-equal vc (attribute 'sourcefilename class)
                                     #:expect "a.scm")
                        (check-equal vc (length lines) #:expect 3)
                        ;; mi is 1 for the line that never ran and 0 otherwise;
                        ;; ci is the other way around, and there are no
                        ;; branches to describe.
                        (check-equal vc (attribute 'mi (car lines))
                                     #:expect "0")
                        (check-equal vc (attribute 'ci (car lines))
                                     #:expect "1")
                        (check-equal vc (attribute 'mi (cadr lines))
                                     #:expect "1")
                        (check-equal vc (attribute 'cb (cadr lines))
                                     #:expect "0")
                        (let ((line-counter
                               (find (lambda (node)
                                       (equal? "LINE" (attribute 'type node)))
                                     counters)))
                          (check-equal vc (attribute 'missed line-counter)
                                       #:expect "1")
                          (check-equal vc (attribute 'covered line-counter)
                                       #:expect "2"))))))))
         (test "the JSON summary holds the totals and the uncovered lines"
               (lambda (vc)
                 (call-with-temporary-directory
                  (lambda (dir)
                    (let* ((path (in-vicinity dir "coverage-summary.json"))
                           (report (sample-report "/project/a.scm"
                                                  '((1 . 2) (2 . 0) (3 . 4)))))
                      (write-coverage-json report #:path path)
                      (check-true vc (file-exists? path))
                      (let* ((document (json-string->scm
                                        (call-with-input-file path
                                          get-string-all)))
                             (totals (assoc-ref document "totals"))
                             (file (vector-ref (assoc-ref document "files") 0)))
                        (check-equal vc (assoc-ref totals "lines") #:expect 3)
                        (check-equal vc (assoc-ref totals "covered") #:expect 2)
                        (check-equal vc (assoc-ref totals "percentage")
                                     #:expect 66.67)
                        (check-equal vc (assoc-ref file "path")
                                     #:expect "/project/a.scm")
                        (check-equal vc (assoc-ref file "uncovered")
                                     #:expect (vector 2))))))))
         (test "the reports of one run add up"
               (lambda (vc)
                 (call-with-temporary-directory
                  (lambda (dir)
                    (let ((cov-dir (in-vicinity dir "lcov"))
                          (html-dir (in-vicinity dir "html")))
                      (publish-coverage! (sample-report "/project/a.scm"
                                                        '((1 . 1) (2 . 0)))
                                         #:dir cov-dir
                                         #:html-dir html-dir)
                      (publish-coverage! (sample-report "/project/b.scm"
                                                        '((1 . 1)))
                                         #:dir cov-dir
                                         #:html-dir html-dir)
                      (let ((merged (read-lcov-file
                                     (in-vicinity cov-dir "veritas.info"))))
                        (check-equal vc (length (coverage-report-files
                                                 merged))
                                     #:expect 2)
                        ;; The counts of the first file are still there.
                        (check-equal vc (coverage-report-line-count
                                         merged "/project/a.scm" 1)
                                     #:expect 1))
                      (check-true vc (file-exists?
                                      (in-vicinity cov-dir
                                                   "coverage-summary.json")))
                      (check-true vc (file-exists?
                                      (in-vicinity html-dir
                                                   "index.html")))
                      (check-true vc (file-exists?
                                      (in-vicinity cov-dir
                                                   "cobertura.xml")))
                      (check-true vc (file-exists?
                                      (in-vicinity cov-dir
                                                   "jacoco.xml")))
                      ;; The XML documents carry the merged totals too.
                      (check-equal
                       vc
                       (attribute 'lines-valid
                                  (find-node
                                   'coverage
                                   (xml->sxml
                                    (call-with-input-file
                                        (in-vicinity cov-dir "cobertura.xml")
                                      get-string-all))))
                       #:expect "3"))))))))

(define end-to-end-suite
  (suite "Coverage from a run"
         #:shuffle? #f
         (test "veritas-run writes the coverage of what its tests exercise"
               (lambda (vc)
                 (call-with-temporary-directory
                  (lambda (dir)
                    (let ((cov-dir (in-vicinity dir "lcov"))
                          (html-dir (in-vicinity dir "html"))
                          (lcov (in-vicinity dir "lcov/veritas.info")))
                      ;; A nested run has to behave like a top-level one: the
                      ;; record of failing tests and the worker hand-over
                      ;; belong to the run around it, not to this one.  A
                      ;; failure inside it asks its process to exit, which here
                      ;; means this test: catch it and say what happened
                      ;; instead of letting a bare `exit' take the suite down.
                      (call-with-coverage-allowed
                       (lambda ()
                         (parameterize ((veritas-last-failed-path
                                         (in-vicinity dir "last-failed.scm"))
                                        (veritas-coverage-data-path #f))
                           (catch 'quit
                             (lambda ()
                               (veritas-run #:shuffle? #f
                                            #:concurrent? #f
                                            #:with-coverage? #t
                                            #:coverage-dir cov-dir
                                            #:coverage-html-dir html-dir
                                            #:coverage-include
                                            (list fixture-directory)
                                            (test "calls the fixture"
                                                  (assert-equal
                                                   #:expect 'pos
                                                   #:got
                                                   (fixture-classify 1)))))
                             (lambda (key status)
                               (error "the nested run exited with status ~a"
                                      status))))))
                      (check-true vc (file-exists? lcov))
                      (check-true vc (file-exists?
                                      (in-vicinity html-dir "index.html")))
                      ;; The Java-world formats are part of a run, not an
                      ;; afterthought: they are written by the same call.
                      (check-true vc (file-exists?
                                      (in-vicinity cov-dir "cobertura.xml")))
                      (check-true vc (file-exists?
                                      (in-vicinity cov-dir "jacoco.xml")))
                      ;; Read the trace file only when it is there: a report
                      ;; that was not written is already a failed check, and a
                      ;; missing file must not hide it behind a raised port
                      ;; error.
                      (let ((text (if (file-exists? lcov)
                                      (call-with-input-file lcov
                                        get-string-all)
                                      "")))
                        (check-true vc (contains? text fixture-source))
                        (check-true vc (contains?
                                        text
                                        (format #f "DA:~a,"
                                                fixture-line-taken)))
                        (check-true vc (contains?
                                        text
                                        (format #f "DA:~a,0"
                                                fixture-line-not-taken)))))))))))

(define (spec)
  (veritas-run #:debug? #f
               measurement-suite
               filtering-suite
               report-suite
               end-to-end-suite))
