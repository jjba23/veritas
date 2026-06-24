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
  #:use-module (veritas model)
  #:export (emoji-reporter log
                           ansi-color-escapes
                           json-reporter
                           monotone-reporter
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
  "Print @var{msg} preceded by a newline to the standard output port.

@example
(log \"Hello World\")
;; Outputs: \\nHello World
@end example"
  (format #t "\n~a" msg))

(define (dispatch-log vc log-type msg)
  "Route @var{msg} through the designated reporter strategy.

Extracts the preferred reporter strategy type from the configuration client
@var{vc} under the key @code{'reporter}, dynamically resolving it to one of:
@itemize
@item @code{monotone-reporter}
@item @code{json-reporter}
@item @code{emoji-reporter} (Default Fallback)
@end itemize

Once resolved, it executes the corresponding function mapping to @var{log-type}
inside the reporter association list using @var{msg} as its argument."
  (let* ((reporter-sym (assoc-ref vc
                                  'reporter))
         (reporter-alist (cond
                           ((equal? 'monotone-reporter reporter-sym)
                            monotone-reporter)
                           ((equal? 'json-reporter reporter-sym)
                            json-reporter)
                           (else emoji-reporter))))
    
    (apply (assoc-ref reporter-alist log-type)
           (list msg))))

(define-syntax-rule (log-pass vc msg)
  "Dispatch a successful validation log event with message @var{msg} via client @var{vc}."
  (dispatch-log vc
                'log-pass msg))

(define-syntax-rule (log-fail vc msg)
  "Dispatch a test failure log event with message @var{msg} via client @var{vc}."
  (dispatch-log vc
                'log-fail msg))

(define-syntax-rule (log-error vc msg)
  "Dispatch an execution runtime error log event with message @var{msg} via client @var{vc}."
  (dispatch-log vc
                'log-error msg))

(define-syntax-rule (log-assert vc msg)
  "Dispatch a generalized assertion framework log event with message @var{msg} via client @var{vc}."
  (dispatch-log vc
                'log-assert msg))

(define-syntax-rule (log-test vc msg)
  "Dispatch an isolated individual test block initialization log event via client @var{vc}."
  (dispatch-log vc
                'log-test msg))

(define-syntax-rule (log-suite vc msg)
  "Dispatch a high-level test suite grouping boundary initialization log event via client @var{vc}."
  (dispatch-log vc
                'log-suite msg))

(define-syntax-rule (log-debug vc msg)
  "Dispatch a developer troubleshooting diagnostic log event with message @var{msg} via client @var{vc}."
  (dispatch-log vc
                'log-debug msg))

(define-syntax-rule (log-info vc msg)
  "Dispatch an operational informative status log event with message @var{msg} via client @var{vc}."
  (dispatch-log vc
                'log-info msg))

(define-syntax-rule (log-benchmark vc msg)
  "Dispatch performance runtime evaluation metrics data payload @var{msg} via client @var{vc}."
  (dispatch-log vc
                'log-benchmark msg))

(define emoji-reporter
  `((reporter-name . emoji-reporter) (log-pass unquote
                                               (lambda (msg)
                                                 (log (format #f
                                                       "    ~a✅ pass - ~a~a"
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

(define (now)
  (strftime "%c"
            (localtime (current-time))))

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
