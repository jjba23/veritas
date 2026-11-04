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

;; You should have received a copy of the Lesser GNU General Public License
;; along with veritas.  If not, see <https://www.gnu.org/licenses/>.

(define-module (veritas reporter)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (json)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (veritas model)
  #:export (emoji-reporter log
                           ansi-color-escapes
                           json-reporter
                           monotone-reporter
                           junit-reporter
                           github-reporter
                           github-actions?
                           write-github-annotations
                           append-github-summary-row
                           veritas-clock
                           veritas-current-location
                           now
                           write-junit-report
                           junit-report-data
                           write-junit-document
                           merge-junit-data
                           write-junit-data
                           read-junit-data
                           ensure-parent-directories
                           log-debug
                           log-pass
                           log-fail
                           log-assert
                           log-test
                           log-suite
                           log-info
                           log-error
                           log-benchmark
                           dispatch-log))

(define ansi-color-escapes
  `((yellow . "\x1b[0;33m") (magenta . "\x1b[0;35m")
    (green . "\x1b[0;32m")
    (default . "\x1b[0;39m")
    (bold . "\x1b[1m")
    (red . "\x1b[0;31m")
    (blue . "\x1b[0;34m")
    (cyan . "\x1b[0;36m")
    (reset . "\x1b[0m")))

(define-syntax-rule (log msg)
  "Print @var{msg} on a line of its own.

@example
(log \"Hello World\")
;; Outputs: \\nHello World
@end example"
  (format #t "\n~a" msg))

(define (dispatch-log vc log-type msg)
  "Send @var{msg} to the reporter chosen in @var{vc}.

The client names one under the key @code{'reporter}; anything else falls back to
@code{emoji-reporter}.  @var{log-type} picks the entry to call
(@code{'log-pass}, @code{'log-fail}, and so on)."
  (let* ((reporter-sym (assoc-ref vc
                                  'reporter))
         (reporter-alist (cond
                           ((equal? 'monotone-reporter reporter-sym)
                            monotone-reporter)
                           ((equal? 'json-reporter reporter-sym)
                            json-reporter)
                           ((equal? 'junit-reporter reporter-sym)
                            junit-reporter)
                           ;; GitHub's annotations and summary only exist inside a
                           ;; workflow, so elsewhere the plain-text reporter
                           ;; takes over rather than a silent run.
                           ((equal? 'github-reporter reporter-sym)
                            (if (github-actions?)
                                github-reporter monotone-reporter))
                           (else emoji-reporter))))
    
    ;; Called directly rather than through `apply' on a one-element list: this
    ;; runs several times per assertion, which is the hottest path in a run.
    ((assoc-ref reporter-alist log-type) msg)))

(define-syntax-rule (log-pass vc msg)
  "Report a passing check."
  (dispatch-log vc
                'log-pass msg))

(define-syntax-rule (log-fail vc msg)
  "Report a failing check."
  (dispatch-log vc
                'log-fail msg))

(define-syntax-rule (log-error vc msg)
  "Report a runtime error."
  (dispatch-log vc
                'log-error msg))

(define-syntax-rule (log-assert vc msg)
  "Announce an assertion that is about to be checked, by name."
  (dispatch-log vc
                'log-assert msg))

(define-syntax-rule (log-test vc msg)
  "Announce a test by name."
  (dispatch-log vc
                'log-test msg))

(define-syntax-rule (log-suite vc msg)
  "Announce a suite by name."
  (dispatch-log vc
                'log-suite msg))

(define-syntax-rule (log-debug vc msg)
  "Log a message for debugging."
  (dispatch-log vc
                'log-debug msg))

(define-syntax-rule (log-info vc msg)
  "Log an informational message."
  (dispatch-log vc
                'log-info msg))

(define-syntax-rule (log-benchmark vc msg)
  "Report benchmark results, given a @code{benchmark-result} record."
  (dispatch-log vc
                'log-benchmark msg))

(define emoji-reporter
  `((reporter-name . emoji-reporter) (log-pass unquote
                                               (lambda (msg)
                                                 (log (format #f
                                                              "    ~a✅ ~a~a"
                                                              (assoc-ref
                                                               ansi-color-escapes
                                                               'green) msg
                                                              (assoc-ref
                                                               ansi-color-escapes
                                                               'reset)))))
    (log-fail unquote
              (lambda (msg)
                (log (format #f "    ~a❌ fail - ~a~a"
                             (assoc-ref ansi-color-escapes
                                        'red) msg
                             (assoc-ref ansi-color-escapes
                                        'reset)))))
    (log-debug unquote
               (lambda (msg)
                 (log (format #f "~a🔬 debug -~a ~a"
                              (assoc-ref ansi-color-escapes
                                         'bold)
                              (assoc-ref ansi-color-escapes
                                         'reset) msg))))
    (log-info unquote
              (lambda (msg)
                (log (format #f "~aℹ️ info - ~a~a"
                             (assoc-ref ansi-color-escapes
                                        'bold) msg
                             (assoc-ref ansi-color-escapes
                                        'reset)))))
    (log-error unquote
               (lambda (msg)
                 (log (format #f "\n~a❌ error - ~a~a\n"
                              (assoc-ref ansi-color-escapes
                                         'red) msg
                              (assoc-ref ansi-color-escapes
                                         'reset)))))
    (log-test unquote
              (lambda (msg)
                (log (format #f
                             "  [~a🧪 test~a] ~a~a~a"
                             (assoc-ref ansi-color-escapes
                                        'bold)
                             (assoc-ref ansi-color-escapes
                                        'reset)
                             (assoc-ref ansi-color-escapes
                                        'bold)
                             msg
                             (assoc-ref ansi-color-escapes
                                        'reset)))))
    (log-benchmark unquote
                   (lambda (msg)
                     (log "+----------------------------------------------+")
                     (log (format #f
                                  "[~a🚄 benchmark~a]\n~a~a\n\n~a~a"
                                  (assoc-ref ansi-color-escapes
                                             'bold)
                                  (assoc-ref ansi-color-escapes
                                             'reset)
                                  (assoc-ref ansi-color-escapes
                                             'bold)
                                  (benchmark-result-name msg)
                                  (assoc-ref ansi-color-escapes
                                             'reset)

                                  (with-output-to-string (lambda ()
                                                           (for-each (lambda (run)
                                                                       (log (format
                                                                             #f
                                                                             "[🏃 ~arun #~a~a]"
                                                                             (assoc-ref
                                                                              ansi-color-escapes
                                                                              'bold)
                                                                             (assoc-ref
                                                                              run
                                                                              'run-index)
                                                                             (assoc-ref
                                                                              ansi-color-escapes
                                                                              'reset)))
                                                                       (log (assoc-ref
                                                                             run
                                                                             'run-details)))
                                                                     (benchmark-result-results
                                                                      msg))))))))
    (log-suite unquote
               (lambda (msg)
                 (log "+----------------------------------------------+")
                 (log (format #f
                              "[~a📑 suite~a] ~a~a~a"
                              (assoc-ref ansi-color-escapes
                                         'bold)
                              (assoc-ref ansi-color-escapes
                                         'reset)
                              (assoc-ref ansi-color-escapes
                                         'bold)
                              msg
                              (assoc-ref ansi-color-escapes
                                         'reset)))))
    (log-assert unquote
                (lambda (msg)
                  (log (format #f
                               "    [~a⚙️ assert~a] ~a~a~a"
                               (assoc-ref ansi-color-escapes
                                          'bold)
                               (assoc-ref ansi-color-escapes
                                          'reset)
                               (assoc-ref ansi-color-escapes
                                          'bold)
                               msg
                               (assoc-ref ansi-color-escapes
                                          'reset)))))))

(define monotone-reporter
  `((reporter-name . monotone-reporter) (log-pass unquote
                                                  (lambda (msg)
                                                    (log (format #f
                                                          "    [pass] ~a" msg))))
    (log-fail unquote
              (lambda (msg)
                (log (format #f "    [fail] ~a"

                             msg))))
    (log-error unquote
               (lambda (msg)
                 (log (format #f "\n[error] ~a\n"

                              msg))))
    (log-test unquote
              (lambda (msg)
                (log (format #f "  [test] ~a"

                             msg))))
    (log-benchmark unquote
                   (lambda (msg)
                     (log "+----------------------------------------------+")

                     (log (format #f "[benchmark]\n~a\n\n~a"
                                  (benchmark-result-name msg)

                                  (with-output-to-string (lambda ()
                                                           (for-each (lambda (run)
                                                                       (log (format
                                                                             #f
                                                                             "[run #~a]"
                                                                             (assoc-ref
                                                                              run
                                                                              'run-index)))
                                                                       (log (assoc-ref
                                                                             run
                                                                             'run-details)))
                                                                     (benchmark-result-results
                                                                      msg))))))))
    (log-debug unquote
               (lambda (msg)
                 (log (format #f "[debug] ~a"

                              msg))))
    (log-info unquote
              (lambda (msg)
                (log (format #f "[info] ~a"

                             msg))))
    (log-suite unquote
               (lambda (msg)
                 (log "------------------------------------------")
                 (log (format #f "[suite] ~a"

                              msg))))
    (log-assert unquote
                (lambda (msg)
                  (log (format #f "    [assert] ~a" msg))))))

;; ====== Seams ======
;;
;; Two things a run cannot make deterministic on its own: the wall clock and the
;; place a failure happened.  Both are parameters, so a test can pin them and a
;; report can be compared with another run's; the defaults are the real clock
;; and no location.

(define veritas-clock
  (make-parameter (lambda ()
                    (strftime "%c" (localtime (current-time))))))

(define veritas-current-location
  ;; "file:line" of the test being run, or #f outside a test.
  (make-parameter #f))

(define (now)
  ((veritas-clock)))

(define json-reporter
  `((reporter-name . json-reporter) (log-pass unquote
                                              (lambda (msg)
                                                (log (scm->json-string `((timestamp
                                                                          unquote
                                                                          (now))
                                                                         (level . pass)
                                                                         (message
                                                                          unquote
                                                                          msg))))))
    (log-fail unquote
              (lambda (msg)
                (log (scm->json-string `((timestamp unquote
                                                    (now))
                                         (level . fail)
                                         (message unquote msg))))))
    (log-error unquote
               (lambda (msg)
                 (log (scm->json-string `((timestamp unquote
                                                     (now))
                                          (level . error)
                                          (message unquote msg))))))
    (log-test unquote
              (lambda (msg)
                (log (scm->json-string `((timestamp unquote
                                                    (now))
                                         (level . test)
                                         (message unquote msg))))))
    (log-benchmark unquote
                   (lambda (msg)
                     (log (scm->json-string `((timestamp unquote
                                                         (now))
                                              (level . benchmark)
                                              (message unquote
                                                       (benchmark-result-name
                                                        msg)))))
                     (for-each (lambda (run)
                                 
                                 (log (scm->json-string `((timestamp unquote
                                                                     (now))
                                                          (level . benchmark)
                                                          (message unquote
                                                                   (assoc-ref
                                                                    run
                                                                    'run-index)))))

                                 (log (scm->json-string `((timestamp unquote
                                                                     (now))
                                                          (level . benchmark)
                                                          (message unquote
                                                                   (assoc-ref
                                                                    run
                                                                    'run-details))))))

                               (benchmark-result-results msg))))
    (log-debug unquote
               (lambda (msg)
                 (log (scm->json-string `((timestamp unquote
                                                     (now))
                                          (level . debug)
                                          (message unquote msg))))))
    (log-info unquote
              (lambda (msg)
                (log (scm->json-string `((timestamp unquote
                                                    (now))
                                         (level . info)
                                         (message unquote msg))))))
    (log-suite unquote
               (lambda (msg)
                 (log (scm->json-string `((timestamp unquote
                                                     (now))
                                          (level . suite)
                                          (message unquote msg))))))
    (log-assert unquote
                (lambda (msg)
                  (log (scm->json-string `((timestamp unquote
                                                      (now))
                                           (level . assert)
                                           (message unquote msg))))))))

;; ====== JUnit XML ======
;;
;; The XML is a document about the whole run rather than a stream of lines, so
;; the reporter keeps quiet (failures and the summary aside) and the document is
;; written from the result tree once the run is over.  Walking the tree instead
;; of the log stream is what keeps suite names and nesting right; the price is
;; that per-test timing is not recorded, because the result records do not
;; carry it.

(define ansi-escape-regexp
  (make-regexp "\x1b\\[[0-9;]*[A-Za-z]"))

(define (strip-ansi s)
  "Return @var{s} without the terminal colour escapes a reporter writes."
  (regexp-substitute/global #f ansi-escape-regexp s 'pre "" 'post))

(define (xml-escape s)
  "Escape @var{s} for XML, dropping the characters XML forbids.

Captured test output can hold colour escapes and other control characters; they
are removed rather than escaped, because XML 1.0 has no representation for most
of them."
  ;; Runs of ordinary characters are copied in one substring instead of being
  ;; built a character at a time: a report escapes every attribute of every
  ;; test, and the per-character version spent most of a report's time making
  ;; one-character strings.
  (let loop ((start 0) (i 0) (out '()))
    (if (= i (string-length s))
        (apply string-append
               (reverse (if (= start i)
                            out
                            (cons (substring s start i) out))))
        (let* ((c (string-ref s i))
               (code (char->integer c))
               (piece (cond ((char=? c #\&) "&amp;")
                            ((char=? c #\<) "&lt;")
                            ((char=? c #\>) "&gt;")
                            ((char=? c #\") "&quot;")
                            ((char=? c #\') "&apos;")
                            ((or (= code 9)
                                 (= code 10)
                                 (= code 13)) #f)
                            ((< code 32) "")
                            (else #f))))
          (if piece
              (loop (+ i 1) (+ i 1)
                    (cons piece
                          (if (= start i)
                              out
                              (cons (substring s start i) out))))
              (loop start (+ i 1) out))))))

(define (ensure-parent-directories path)
  "Create every directory in front of @var{path}, ignoring ones that exist."
  (let* ((dir (dirname path))
         (absolute? (string-prefix? "/" dir))
         (parts (remove string-null? (string-split dir #\/))))
    (let loop ((ps parts)
               (so-far (if absolute? "/" "")))
      (unless (null? ps)
        (let ((next (cond ((string=? so-far "") (car ps))
                          ((string=? so-far "/") (string-append "/" (car ps)))
                          (else (string-append so-far "/" (car ps))))))
          (catch #t (lambda () (mkdir next)) (lambda (key . args) #f))
          (loop (cdr ps) next))))))

(define (join-with separator parts)
  "Join @var{parts} with @var{separator} between them."
  (cond ((null? parts) "")
        ((null? (cdr parts)) (car parts))
        (else (string-append (car parts)
                             separator
                             (join-with separator (cdr parts))))))

(define (junit-suite-name path)
  "Return the suite path @var{path} as one string, empty at the top level."
  (join-with " > " path))

(define (junit-classname path file)
  "Return the class name of a test reached through @var{path}, in @var{file}.

The file is the class name whenever a result knows it, because that is what an
importer, a CI action and GitHub's annotations use to attach a test to a source
file; the suite path goes into an attribute of its own so the hierarchy is not
lost with it.  A spec called directly, with no runner around it, has no file,
and then the suite path is all there is."
  (cond (file file)
        ((null? path) "veritas")
        (else (join-with " > " path))))

(define (case-failures c)
  (assoc-ref c 'failures))

(define (case-skipped? c)
  (assoc-ref c 'skipped?))

(define (junit-case name classname file line suite assertions output duration)
  "Return one test as a JUnit case alist, from its assertions and output.

@var{output} is what the test printed - including the reporter's own failure
lines - and becomes the body of the first failure, so a CI log shows why the
test failed without a rerun."
  (let* ((failing (filter (lambda (a)
                            (and (assertion-result? a)
                                 (not (assertion-result-successful a))))
                          assertions))
         (detail (strip-ansi (format #f "~a" output)))
         (failures (let loop ((fs failing)
                              (first? #t)
                              (acc '()))
                     (if (null? fs)
                         (reverse acc)
                         (let* ((a (car fs))
                                (own (strip-ansi (format #f "~a"
                                                         (assertion-result-output
                                                          a))))
                                (body (cond ((not (string=? own ""))
                                             (string-append own detail))
                                            (first? detail)
                                            (else ""))))
                           (loop (cdr fs) #f
                                 (cons (list (cons 'message
                                                   (or (assertion-result-name a)
                                                       "assertion failed"))
                                             (cons 'body body))
                                       acc)))))))
    (list (cons 'name name)
          (cons 'classname classname)
          (cons 'file file)
          (cons 'line line)
          (cons 'suite suite)
          (cons 'duration (or duration 0))
          (cons 'failures failures)
          (cons 'skipped? #f)
          (cons 'error? #f))))

(define (junit-cases result path output)
  "Flatten @var{result} into JUnit cases, under the suite names in @var{path}.

@var{output} travels down with the result, because a test's own output lives on
its @code{test-result} parent rather than on the @code{pre-test-result} itself."
  (cond
    ((suite-result? result)
     (append-map (lambda (child)
                   (junit-cases child
                                (append path
                                        (list (suite-result-name result))) ""))
                 (suite-result-test-results result)))
    ;; A test result wraps one child result, not a list of them.
    ((test-result? result)
     (junit-cases (test-result-results result)
                  path (test-result-output result)))
    ((pre-test-result? result)
     (let ((file (pre-test-result-file result)))
       (list (junit-case (pre-test-result-name result)
                         (junit-classname path file)
                         file
                         (pre-test-result-line result)
                         (junit-suite-name path)
                         (pre-test-result-results result)
                         output
                         (pre-test-result-duration result)))))
    ((benchmark-result? result)
     (list (list (cons 'name (benchmark-result-name result))
                 (cons 'classname (junit-classname path #f))
                 (cons 'file #f)
                 (cons 'line #f)
                 (cons 'suite (junit-suite-name path))
                 (cons 'duration 0)
                 (cons 'failures '())
                 (cons 'skipped? #t)
                 (cons 'error? #f)
                 (cons 'message "benchmark, not an assertion"))))
    ;; A task that raised inside a fiber arrives as the error string itself.
    ((string? result)
     (list (list                 (cons 'name (junit-classname path #f))
                 (cons 'classname (junit-classname path #f))
                 (cons 'file #f)
                 (cons 'line #f)
                 (cons 'suite (junit-suite-name path))
                 (cons 'duration 0)
                 ;; One failure, holding its message and its (empty) body: a
                 ;; raised task has no output of its own to quote.
                 (cons 'failures (list (list (cons 'message (strip-ansi result))
                                            (cons 'body ""))))
                 (cons 'skipped? #f)
                 (cons 'error? #t))))
    (else '())))

(define (write-junit-case port c)
  "Write one JUnit @code{testcase} element for the case alist @var{c}.

The case's file and the suite path it sat in are written as attributes along
with its duration, so an importer can place the test in the project's files and
in the hierarchy of suites at once."
  (format port "<testcase name=\"~a\" classname=\"~a\""
          (xml-escape (format #f "~a" (assoc-ref c 'name)))
          (xml-escape (format #f "~a" (assoc-ref c 'classname))))
  (let ((file (assoc-ref c 'file))
        (suite (assoc-ref c 'suite)))
    (when (and file (not (string-null? (format #f "~a" file))))
      (format port " file=\"~a\"" (xml-escape (format #f "~a" file))))
    (when (assoc-ref c 'line)
      (format port " line=\"~a\"" (assoc-ref c 'line)))
    (when (and suite (not (string-null? (format #f "~a" suite))))
      (format port " suite=\"~a\"" (xml-escape (format #f "~a" suite)))))
  (format port " time=\"~,3f\"" (exact->inexact (or (assoc-ref c 'duration) 0)))
  (cond
    ((case-skipped? c)
     (format port ">~%")
     (let ((message (assoc-ref c 'message)))
       (if message
           (format port "<skipped message=\"~a\"/>~%" (xml-escape message))
           (format port "<skipped/>~%")))
     (format port "</testcase>~%"))
    ((null? (case-failures c)) (format port "/>~%"))
    (else
     (format port ">~%")
     (let ((tag (if (assoc-ref c 'error?) "error" "failure")))
       (for-each (lambda (f)
                   (let ((message (xml-escape (format #f "~a" (assoc-ref f 'message))))
                         (body (or (assoc-ref f 'body) "")))
                     (if (string-null? body)
                         (format port "<~a message=\"~a\"/>~%" tag message)
                         (begin
                           (format port "<~a message=\"~a\">~%" tag message)
                           (format port "~a~%" (xml-escape body))
                           (format port "</~a>~%" tag)))))
                 (case-failures c)))
     (format port "</testcase>~%"))))

(define (write-junit-suite port name cases)
  "Write one JUnit @code{testsuite} element holding @var{cases}."
  (let* ((errors (length (filter (lambda (c) (assoc-ref c 'error?)) cases)))
         (skipped (length (filter case-skipped? cases)))
         (failures (length (filter (lambda (c)
                                     (and (not (case-skipped? c))
                                          (not (assoc-ref c 'error?))
                                          (not (null? (case-failures c)))))
                                   cases)))
         (seconds (apply + (map (lambda (c) (assoc-ref c 'duration)) cases))))
    (format port
            "<testsuite name=\"~a\" tests=\"~a\" failures=\"~a\" errors=\"~a\" skipped=\"~a\" time=\"~,3f\">~%"
            (xml-escape name) (length cases) failures errors skipped
            (exact->inexact seconds))
    (for-each (lambda (c) (write-junit-case port c)) cases)
    (format port "</testsuite>~%")))



(define (group-cases-by-class cases)
  "Return @code{(class . cases)} pairs, one per class, in first-seen order.

A hash table does the grouping and a list keeps the order, so the report is
deterministic without being quadratic."
  (let ((groups (make-hash-table))
        (order '()))
    (for-each (lambda (c)
                (let ((class (assoc-ref c 'classname)))
                  (unless (hash-ref groups class)
                    (hash-set! groups class '())
                    (set! order (cons class order)))
                  (hash-set! groups class (cons c (hash-ref groups class)))))
              cases)
    (map (lambda (class)
           (cons class (reverse (hash-ref groups class))))
         (reverse order))))

(define* (junit-report-data all-results #:key (seconds 0))
  "Return the JUnit report of @var{all-results} as data rather than a file.

The result is an alist holding the numbers a @code{testsuites} element needs -
@code{tests}, @code{failures}, @code{errors} and @code{skipped} - and @code{xml},
the @code{testsuite} elements themselves as a string.  Writing the file is
@code{write-junit-document}'s job; a separate process can be handed this alist
and its run joined to others with @code{merge-junit-data}, which is how one
document is made out of the parallel runs of @code{--jobs}."
  (let* ((cases (append-map (lambda (result)
                              (junit-cases result '() "")) all-results))
         ;; One pass groups the cases by class, keeping the order the classes
         ;; first appeared in.  Filtering the whole list once per class - what
         ;; this used to do - is quadratic in the size of the run, and a run of
         ;; a few thousand tests paid for it in seconds.
         (grouped (group-cases-by-class cases))
         (errors (length (filter (lambda (c) (assoc-ref c 'error?)) cases)))
         (skipped (length (filter case-skipped? cases)))
         (failures (length (filter (lambda (c)
                                     (and (not (case-skipped? c))
                                          (not (assoc-ref c 'error?))
                                          (not (null? (case-failures c)))))
                                   cases)))
         (xml (with-output-to-string (lambda ()
                                       (for-each (lambda (group)
                                                   (write-junit-suite (current-output-port)
                                                                      (car group)
                                                                      (cdr group)))
                                                 grouped)))))
    (list (cons 'tests (length cases))
          (cons 'failures failures)
          (cons 'errors errors)
          (cons 'skipped skipped)
          (cons 'xml xml))))

(define* (write-junit-document data
                               #:key (seconds 0)
                               (path "reports/junit/veritas-junit.xml"))
  "Write JUnit @var{data}, as made by @code{junit-report-data}, to @var{path}.

@var{seconds} is the wall clock of the run and becomes the document's total
time; for a run split over worker processes it is the wall clock of the whole
parallel run, not the sum of the workers.  Returns @var{path}.

Every @code{testcase} carries the file it came from, the suite path it sat in
and its own duration; each @code{testsuite} sums the durations of its cases."
  (ensure-parent-directories path)
  (call-with-output-file path
    (lambda (port)
      (format port "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
      (format port
              "<testsuites name=\"veritas\" tests=\"~a\" failures=\"~a\" errors=\"~a\" skipped=\"~a\" time=\"~a\">~%"
              (assoc-ref data 'tests) (assoc-ref data 'failures)
              (assoc-ref data 'errors) (assoc-ref data 'skipped) seconds)
      (display (assoc-ref data 'xml) port)
      (format port "</testsuites>~%")))
  path)

(define (merge-junit-data datas)
  "Combine the JUnit data of several runs into one.

The counts are summed and the @code{testsuite} elements are concatenated in the
order given, so the merged document keeps the order the runs were scheduled in."
  (list (cons 'tests (apply + (map (lambda (d) (assoc-ref d 'tests)) datas)))
        (cons 'failures (apply + (map (lambda (d) (assoc-ref d 'failures)) datas)))
        (cons 'errors (apply + (map (lambda (d) (assoc-ref d 'errors)) datas)))
        (cons 'skipped (apply + (map (lambda (d) (assoc-ref d 'skipped)) datas)))
        (cons 'xml (apply string-append
                          (map (lambda (d) (assoc-ref d 'xml)) datas)))))

(define* (write-junit-data data #:key (path "reports/junit/veritas-junit-data.scm"))
  "Write JUnit @var{data} to @var{path} so another process can read it back.

An alist is plain Scheme data, so @code{read} restores it exactly: this is how a
worker process hands its report to the process that merges them.  Returns
@var{path}."
  (ensure-parent-directories path)
  (call-with-output-file path
    (lambda (port)
      (write data port)
      (newline port)))
  path)

(define (read-junit-data path)
  "Return the JUnit data recorded at @var{path}, or @code{#f}.

@code{#f} means there is nothing to merge: the file is missing, unreadable or
not the alist @code{write-junit-data} writes."
  (if (file-exists? path)
      (catch #t
             (lambda ()
               (let ((data (call-with-input-file path read)))
                 (if (and (list? data)
                          (assoc-ref data 'xml))
                     data #f)))
             (lambda (key . args) #f))
      #f))

(define* (write-junit-report all-results
                             #:key (seconds 0)
                             (path "reports/junit/veritas-junit.xml"))
  "Write @var{all-results} to @var{path} as one JUnit XML document.

@var{seconds} is the wall clock of the run and becomes the document's total
time.  Every test becomes a @code{testcase} inside a @code{testsuite} named
after the suites it sits in; loose tests land in a suite called @code{veritas}.
A benchmark is reported as skipped, because it measures instead of asserting,
and a task that raised is reported as an @code{error} rather than a failure.
Returns @var{path}.

Every @code{testcase} carries the file it came from, the suite path it sat in
and its own duration; each @code{testsuite} sums the durations of its cases."
  (write-junit-document (junit-report-data all-results #:seconds seconds)
                        #:seconds seconds #:path path))

(define (junit-quiet msg)
  "Ignore a log message: the XML document is written at the end of the run."
  (if #f #f))

(define junit-reporter
  `((reporter-name . junit-reporter) (log-pass unquote junit-quiet)
    (log-assert unquote junit-quiet)
    (log-test unquote junit-quiet)
    (log-suite unquote junit-quiet)
    (log-debug unquote junit-quiet)
    (log-benchmark unquote junit-quiet)
    (log-fail unquote
              (lambda (msg)
                (log (format #f "    [fail] ~a" msg))))
    (log-error unquote
               (lambda (msg)
                 (log (format #f "\n[error] ~a\n" msg))))
    (log-info unquote
              (lambda (msg)
                (log (format #f "[info] ~a" msg))))))

;; ====== GitHub Actions ======
;;
;; GitHub parses no test results of its own: it offers line-anchored annotations
;; and a Markdown job summary, both written as workflow commands on stdout.  The
;; annotations are made from the same cases the JUnit document is built from, so
;; a failure reads the same whichever surface shows it, and the summary gets the
;; numbers the run prints even when it fails.

(define (github-actions?)
  "Return @code{#t} when the run is inside a GitHub Actions workflow."
  (let ((value (getenv "GITHUB_ACTIONS")))
    (and value (string=? (string-downcase value) "true"))))

(define (github-escape-property value)
  "Escape @var{value} for the property part of a workflow command."
  (let* ((s (string-replace-substring (format #f "~a" value) "%" "%25"))
         (s (string-replace-substring s "\r" "%0D"))
         (s (string-replace-substring s "\n" "%0A"))
         (s (string-replace-substring s ":" "%3A")))
    (string-replace-substring s "," "%2C")))

(define (github-escape-message value)
  "Escape @var{value} for the message part of a workflow command."
  (let* ((s (string-replace-substring (format #f "~a" value) "%" "%25"))
         (s (string-replace-substring s "\r" "%0D")))
    (string-replace-substring s "\n" "%0A")))

(define* (github-annotation-line c)
  "Return the workflow command annotating the failing case @var{c}, or @code{#f}.

The file attribute is what places an annotation on the file in a pull request;
a case that passed, or one with no file of its own, has nothing to annotate."
  (let* ((failures (case-failures c))
         (file (assoc-ref c 'file))
         (suite (assoc-ref c 'suite))
         (name (format #f "~a" (assoc-ref c 'name)))
         (title (if (and suite (not (string-null? (format #f "~a" suite))))
                    (string-append (format #f "~a" suite) " > " name)
                    name))
         (message (if (null? failures)
                      "test failed"
                      (format #f "~a" (assoc-ref (car failures) 'message)))))
    (and (pair? failures)
         (format #f "::error ~a::~a"
                 (if file
                     (format #f "file=~a,title=~a"
                             (github-escape-property file)
                             (github-escape-property title))
                     (format #f "title=~a" (github-escape-property title)))
                 (github-escape-message message)))))

(define* (write-github-annotations all-results #:key (limit 10))
  "Print workflow-command annotations for the failures in @var{all-results}.

GitHub keeps ten error annotations per step, so only the first @var{limit} are
written and a notice says how many were left out; the JUnit document carries
every one of them.  Returns how many annotations were written."
  (let* ((cases (append-map (lambda (result) (junit-cases result '() ""))
                            all-results))
         (failing (filter (lambda (c)
                            (and (not (case-skipped? c))
                                 (pair? (case-failures c))))
                          cases))
         (shown (if (> (length failing) limit)
                    (take failing limit)
                    failing)))
    (for-each (lambda (c)
                (let ((line (github-annotation-line c)))
                  (when line
                    (display line)
                    (newline))))
              shown)
    (when (> (length failing) limit)
      (format #t "::notice::~a more failing tests than annotations; see the JUnit report~%"
              (- (length failing) limit)))
    (length shown)))

(define* (append-github-summary-row counts
                                    #:key
                                    (name "run")
                                    (summary-path (getenv "GITHUB_STEP_SUMMARY")))
  "Append one row of @var{counts} to the GitHub Actions step summary.

The table header is written the first time, and every test file of the run adds
a row, so the summary shows a file beside its numbers and still says something
when the run is failing.  Returns @var{summary-path}, or @code{#f} when there is
no summary to append to."
  (if (and summary-path (not (string-null? summary-path)))
      (let* ((fresh (not (file-exists? summary-path)))
             (port (open-file summary-path "a")))
        (dynamic-wind
          (lambda () #t)
          (lambda ()
            (when fresh
              (format port "### veritas test results~%~%")
              (format port "| file | suites | tests | assertions |~%")
              (format port "| --- | --- | --- | --- |~%"))
            (format port "| ~a | ~a | ~a | ~a |~%"
                    name
                    (assoc-ref counts 'suites)
                    (assoc-ref counts 'tests)
                    (assoc-ref counts 'assertions)))
          (lambda () (close-port port)))
        summary-path)
      #f))

;; Quiet, because the annotations and the step summary are written from the
;; result tree at the end of each file instead of line by line as the log
;; scrolls.  Outside a workflow `dispatch-log' uses the monotone reporter.
(define github-reporter
  `((reporter-name . github-reporter) (log-pass unquote junit-quiet)
    (log-assert unquote junit-quiet)
    (log-test unquote junit-quiet)
    (log-suite unquote junit-quiet)
    (log-debug unquote junit-quiet)
    (log-benchmark unquote junit-quiet)
    (log-fail unquote
              (lambda (msg)
                (log (format #f "    [fail] ~a" msg))))
    (log-error unquote
               (lambda (msg)
                 (log (format #f "\n[error] ~a\n" msg))))
    (log-info unquote
              (lambda (msg)
                (log (format #f "[info] ~a" msg))))))
