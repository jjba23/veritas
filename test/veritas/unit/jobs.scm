;; veritas

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; veritas is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; veritas is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; Lesser GNU General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with veritas.  If not, see <https://www.gnu.org/licenses/>.

(define-module (veritas unit jobs)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (veritas veritas)
  #:use-module (veritas model)
  #:use-module (veritas reporter)
  #:use-module (veritas runner)
  #:export (spec))

;; `--jobs' runs each file in a worker process.  Forking a process from inside a
;; test would take the test suite with it, so what is checked here is the part
;; that can be checked in place: the option itself, and the merging of what the
;; workers leave behind - their record of failing names and their JUnit data.

(define vc (list (cons 'reporter 'monotone-reporter)))

(define scratch-dir "./tmp-jobs-suite")

(define (scratch path)
  (string-append scratch-dir "/" path))

(define (remove-tree! dir)
  "Delete @var{dir} with everything under it, when it is there."
  (catch #t
         (lambda ()
           (for-each (lambda (entry)
                       (unless (member entry '("." ".."))
                         (let ((path (string-append dir "/" entry)))
                           (if (file-is-directory? path)
                               (remove-tree! path)
                               (delete-file path)))))
                     (scandir dir))
           (rmdir dir))
         (lambda (key . args) #f)))

;; ====== Fixtures the runner loads ======
;;
;; The file loop is checked on real files, because what a failing file does -
;; exiting the process, or raising while it loads - is exactly what the loop has
;; to survive.

(define saved-load-path %load-path)

(define marker-path
  (string-append (getcwd) "/" scratch-dir "/ran.log"))

(define (make-dirs! path)
  "Create @var{path} and every directory in front of it."
  (let ((dir (dirname path)))
    (unless (string=? dir ".")
      (make-dirs! dir))
    (catch #t (lambda () (mkdir path)) (lambda (key . args) #f))))

(define (write-file! path text)
  "Write @var{text} to @var{path}."
  (call-with-output-file path
    (lambda (port) (display text port))))

(define (count-lines text)
  "Return how many lines of @var{text} end in a newline."
  (let loop ((i 0) (n 0))
    (cond ((= i (string-length text)) n)
          ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ n 1)))
          (else (loop (+ i 1) n)))))

(define (write-fixtures!)
  "Write the probe files the file loop is checked on and make them loadable."
  (make-dirs! (scratch "veritas/probe"))
  ;; A passing file leaves a mark, so the test can see that it ran after the
  ;; file which failed.
  (write-file! (scratch "veritas/probe/pass.scm")
               (format #f "(define-module (veritas probe pass) #:export (spec))
(define (spec)
  (let ((port (open-file ~s \"a\")))
    (display \"ran\\n\" port)
    (close-port port))
  #t)"
                       marker-path))
  ;; `veritas-run' reports a failing file by exiting, so this is the shape the
  ;; loop has to survive.
  (write-file! (scratch "veritas/probe/exit-one.scm")
               "(define-module (veritas probe exit-one) #:export (spec))
(define (spec) (exit 1))")
  (write-file! (scratch "veritas/probe/raise.scm")
               "(define-module (veritas probe raise) #:export (spec))
(define (spec) (error \"the fixture raised\"))")
  (set! %load-path (cons scratch-dir %load-path)))

;; Both hooks leave the tree absent and the load path as it was: the files under
;; test are the ones the code creates, and a suite that leaks its scratch
;; directory is a suite that fails for someone else later.
(define (clean-scratch!)
  (remove-tree! scratch-dir)
  (set! %load-path saved-load-path))

(define (junit-data tests failures)
  "Return JUnit data with @var{tests} cases of which @var{failures} failed."
  (list (cons 'tests tests)
        (cons 'failures failures)
        (cons 'errors 0)
        (cons 'skipped 0)
        (cons 'xml (format #f "<testsuite name=\"s~a\"/>\n" tests))))

(define jobs-suite
  (suite "worker processes"
         #:concurrent? #f
         #:shuffle? #f
         #:before-all (lambda (vc)
                        (clean-scratch!)
                        (write-fixtures!))
         #:after-all (lambda (vc) (clean-scratch!))
         (test "a job count is a whole number of processes"
               (lambda (vc)
                 ((assert-equal #:expect 4 #:got (parse-jobs "4")) vc)
                 ((assert-equal #:expect 1 #:got (parse-jobs "1")) vc)
                 ((assert-equal #:expect #f #:got (parse-jobs #f)) vc)
                 ;; Anything else is an error: a CI job that asked for
                 ;; parallelism and silently got none is a lie.
                 ((assert-true (catch #t (lambda () (parse-jobs "0") #f)
                                      (lambda (key . args) #t)))
                  vc)
                 ((assert-true (catch #t (lambda () (parse-jobs "two") #f)
                                      (lambda (key . args) #t)))
                  vc)
                 ((assert-true (catch #t (lambda () (parse-jobs "2.5") #f)
                                      (lambda (key . args) #t)))
                  vc)))
         (test "the records of several runs merge into one"
               (lambda (vc)
                 ;; Names are deduplicated, and the order they were found in is
                 ;; kept so a rerun reads the way the run did.
                 ((assert-equal #:expect '("a" "b")
                                #:got (merge-failed-names (list '("a")
                                                                '()
                                                                '("b" "a"))))
                  vc)
                 ;; Every run passing means nothing to rerun.
                 ((assert-equal #:expect '()
                                #:got (merge-failed-names (list '() '())))
                  vc)
                 ;; No record at all is no information, not a passing run.
                 ((assert-equal #:expect #f
                                #:got (merge-failed-names (list #f #f)))
                  vc)
                 ((assert-equal #:expect '("a")
                                #:got (merge-failed-names (list #f '("a"))))
                  vc)))
         (test "a record can be written where a worker puts it"
               (lambda (vc)
                 (let ((path (scratch "worker.last-failed.scm")))
                   ((assert-equal #:expect '("the failing one")
                                  #:got (write-last-failed-names! '("the failing one")
                                                                  #:path path))
                    vc)
                   ((assert-equal #:expect '("the failing one")
                                  #:got (read-last-failed #:path path))
                    vc)
                   ;; A path whose directory does not exist yet is made.
                   ((assert-equal #:expect '()
                                  #:got (write-last-failed-names! '()
                                                                  #:path (scratch "deep/made/up.scm")))
                    vc)
                   ((assert-true (file-exists? (scratch "deep/made/up.scm"))) vc))))
         (test "JUnit data merges by summing and concatenating"
               (lambda (vc)
                 (let ((merged (merge-junit-data (list (junit-data 2 1)
                                                       (junit-data 3 0)))))
                   ((assert-equal #:expect 5 #:got (assoc-ref merged 'tests)) vc)
                   ((assert-equal #:expect 1 #:got (assoc-ref merged 'failures)) vc)
                   ((assert-equal #:expect 0 #:got (assoc-ref merged 'errors)) vc)
                   ((assert-equal #:expect 0 #:got (assoc-ref merged 'skipped)) vc)
                   ((assert-equal #:expect
                                  (string-append "<testsuite name=\"s2\"/>\n"
                                                 "<testsuite name=\"s3\"/>\n")
                                  #:got (assoc-ref merged 'xml))
                    vc))))
         (test "JUnit data written by a worker can be read back and merged"
               (lambda (vc)
                 (let ((path (scratch "1.junit.scm")))
                   (write-junit-data (junit-data 1 1) #:path path)
                   ((assert-equal #:expect 1
                                  #:got (assoc-ref (read-junit-data path) 'tests))
                    vc)
                   ;; Missing or damaged data is nothing to merge, never an
                   ;; error that takes down the process doing the merging.
                   ((assert-false (read-junit-data (scratch "absent.junit.scm")))
                    vc)
                   (call-with-output-file (scratch "damaged.junit.scm")
                     (lambda (port) (display "(this is not junit data" port)))
                   ((assert-false (read-junit-data (scratch "damaged.junit.scm")))
                    vc))))
         (test "a JUnit document is written from merged data"
               (lambda (vc)
                 (let* ((path (scratch "merged.xml"))
                        (data (merge-junit-data (list (junit-data 2 1)
                                                      (junit-data 3 0)))))
                   (write-junit-document data #:seconds 0.5 #:path path)
                   (let ((text (call-with-input-file path get-string-all)))
                     ((assert-contains #:expect text
                                       #:got "tests=\"5\" failures=\"1\" errors=\"0\" skipped=\"0\" time=\"0.5\"")
                      vc)
                     ((assert-contains #:expect text
                                       #:got (assoc-ref data 'xml))
                      vc)))))
         (test "a file that passes is reported as passed"
               (lambda (vc)
                 ((assert-true (run-test-file-guarded #:test-file "pass.scm"
                                                      #:test-type "probe"
                                                      #:entrypoint-function
                                                      "spec"))
                  vc)))
         (test "a file that exits is reported as failed, not as the end of the run"
               (lambda (vc)
                 ((assert-false (run-test-file-guarded #:test-file
                                                       "exit-one.scm"
                                                       #:test-type "probe"
                                                       #:entrypoint-function
                                                       "spec"))
                  vc)))
         (test "a file that raises is reported as failed"
               (lambda (vc)
                 ((assert-false (run-test-file-guarded #:test-file "raise.scm"
                                                       #:test-type "probe"
                                                       #:entrypoint-function
                                                       "spec"))
                  vc)))
         (test "the files after a failing one still run"
               (lambda (vc)
                 (catch #t (lambda () (delete-file marker-path))
                        (lambda (key . args) #f))
                 (let* ((failed (run-test-files (list "pass.scm"
                                                      "exit-one.scm"
                                                      "pass.scm")
                                                #:test-type "probe"
                                                #:entrypoint-function "spec"))
                        (marks (call-with-input-file marker-path
                                 get-string-all)))
                   ((assert-equal #:expect '("exit-one.scm")
                                  #:got failed)
                    vc)
                   ;; Two marks: the passing file ran again after the one which
                   ;; exited, so its failure did not end the run.
                   ((assert-equal #:expect 2 #:got (count-lines marks)) vc))))))

;; The reports are a second subject: what a consumer outside the run reads.
;; Kept in a suite of its own so the fixtures above stay about the file loop,
;; and with a scratch directory of its own because the two suites run at the
;; same time and the file-loop fixtures clear theirs when they finish.
(define junit-scratch-dir
  "./tmp-junit-suite")

(define (junit-scratch path)
  (string-append junit-scratch-dir "/" path))

(define junit-suite
  (suite "JUnit reports and CI annotations"
         #:after-all (lambda (vc) (remove-tree! junit-scratch-dir))
         (test "the JUnit document places a test in its file and times it"
               (lambda (vc)
                 (let* ((results (list (make-suite-result
                                        "checkout"
                                        (list (make-test-result
                                               0 ""
                                               (make-pre-test-result
                                                "pays"
                                                (list (make-assertion-result
                                                       0 "" "paid" #t))
                                                #:file "test/cart.scm"
                                                #:duration 0.25))
                                              (make-test-result
                                               1 ""
                                               (make-pre-test-result
                                                "refuses <&>"
                                                (list (make-assertion-result
                                                       0 "<no>" "refused" #f))
                                                #:file "test/cart.scm"
                                                #:duration 0.5)))
                                        "")
                                    "boom"))
                        (data (junit-report-data results #:seconds 1))
                        (xml (assoc-ref data 'xml))
                        (expected (string-append
                                   (string-append "<testsuite name=\"test/cart.scm\" tests=\"2\""
                                                  " failures=\"1\" errors=\"0\" skipped=\"0\""
                                                  " time=\"0.750\">\n")
                                   (string-append "<testcase name=\"pays\" classname=\"test/cart.scm\""
                                                  " file=\"test/cart.scm\" suite=\"checkout\""
                                                  " time=\"0.250\"/>\n")
                                   (string-append "<testcase name=\"refuses &lt;&amp;&gt;\""
                                                  " classname=\"test/cart.scm\""
                                                  " file=\"test/cart.scm\" suite=\"checkout\""
                                                  " time=\"0.500\">\n")
                                   "<failure message=\"refused\">\n"
                                   "&lt;no&gt;\n"
                                   "</failure>\n"
                                   "</testcase>\n"
                                   "</testsuite>\n"
                                   (string-append "<testsuite name=\"veritas\" tests=\"1\""
                                                  " failures=\"0\" errors=\"1\" skipped=\"0\""
                                                  " time=\"0.000\">\n")
                                   "<testcase name=\"veritas\" classname=\"veritas\" time=\"0.000\">\n"
                                   "<error message=\"boom\"/>\n"
                                   "</testcase>\n"
                                   "</testsuite>\n")))
                   ((assert-equal #:expect expected #:got xml) vc)
                   ((assert-equal #:expect 3 #:got (assoc-ref data 'tests)) vc)
                   ((assert-equal #:expect 1 #:got (assoc-ref data 'failures)) vc)
                   ((assert-equal #:expect 1 #:got (assoc-ref data 'errors)) vc)
                   ;; The whole document, not only the suites in it, is what a
                   ;; consumer opens: the header has to agree with the cases.
                   (let ((path (junit-scratch "golden.junit.xml")))
                     (make-dirs! junit-scratch-dir)
                     (write-junit-document data #:seconds 1 #:path path)
                     (let ((document (call-with-input-file path get-string-all)))
                       ((assert-contains
                         #:expect document
                         #:got (string-append
                                "<testsuites name=\"veritas\" tests=\"3\""
                                " failures=\"1\" errors=\"1\" skipped=\"0\""
                                " time=\"1\">"))
                        vc)
                       ((assert-contains #:expect document #:got xml) vc))))))
         (test "GitHub annotations name the failing test and its file"
               (lambda (vc)
                 (let* ((results (list (make-suite-result
                                        "cart"
                                        (list (make-test-result
                                               0 ""
                                               (make-pre-test-result
                                                "pays, twice"
                                                (list (make-assertion-result
                                                       0 "" "totals differ" #f))
                                                #:file "test/cart.scm")))
                                        "")))
                        (text (with-output-to-string
                               (lambda ()
                                 (write-github-annotations results)))))
                   ((assert-equal
                     #:expect
                     "::error file=test/cart.scm,title=cart > pays%2C twice::totals differ\n"
                     #:got text)
                    vc))))
         (test "the step summary gets a row per file and one header"
               (lambda (vc)
                 (let ((path (junit-scratch "summary.md")))
                   (make-dirs! junit-scratch-dir)
                   (catch #t (lambda () (delete-file path))
                          (lambda (key . args) #f))
                   (append-github-summary-row '((files . 1) (suites . 2)
                                                (tests . 3) (assertions . 4))
                                              #:name "test/a.scm"
                                              #:summary-path path)
                   (append-github-summary-row '((files . 1) (suites . 0)
                                                (tests . 0) (assertions . 0))
                                              #:name "test/b.scm"
                                              #:summary-path path)
                   (let ((text (call-with-input-file path get-string-all)))
                     ((assert-contains #:expect text
                                       #:got "| test/a.scm | 2 | 3 | 4 |")
                      vc)
                     ((assert-contains #:expect text
                                       #:got "| test/b.scm | 0 | 0 | 0 |")
                      vc)
                     ((assert-equal #:expect 6 #:got (count-lines text)) vc)))))))

(define (spec)
  (veritas-run #:debug? #f
               #:shuffle? #f
               jobs-suite
               junit-suite))
