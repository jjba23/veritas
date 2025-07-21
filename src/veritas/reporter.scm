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
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:export (colorful-reporter log
                              ansi-color-escapes
                              monotone-reporter
                              log-debug
                              log-pass
                              log-fail
                              log-assert
                              log-test
                              log-suite
                              log-info
                              log-error))

(define ansi-color-escapes
  `((yellow . "\x1b[0;33m") (magenta . "\x1b[0;35m")
    (green . "\x1b[0;32m")
    (default . "\x1b[0;39m")
    (bold . "\x1b[1m")
    (red . "\x1b[0;31m")
    (blue . "\x1b[0;34m")
    (cyan . "\x1b[0;36m")
    (reset . "\x1b[0m")))

(define (log msg)
  (format #t "\n~a" msg))

(define (log-pass vc msg)
  ((assoc-ref (assoc-ref vc
                         'reporter)
              'log-pass)
   msg))

(define (log-fail vc msg)
  ((assoc-ref (assoc-ref vc
                         'reporter)
              'log-fail)
   msg))

(define (log-error vc msg)
  ((assoc-ref (assoc-ref vc
                         'reporter)
              'log-error)
   msg))

(define (log-assert vc msg)
  ((assoc-ref (assoc-ref vc
                         'reporter)
              'log-assert)
   msg))

(define (log-test vc msg)
  ((assoc-ref (assoc-ref vc
                         'reporter)
              'log-test)
   msg))

(define (log-suite vc msg)
  ((assoc-ref (assoc-ref vc
                         'reporter)
              'log-suite)
   msg))

(define (log-debug vc msg)
  ((assoc-ref (assoc-ref vc
                         'reporter)
              'log-debug)
   msg))

(define (log-info vc msg)
  ((assoc-ref (assoc-ref vc
                         'reporter)
              'log-info)
   msg))

(define colorful-reporter
  `((log-pass unquote
              (lambda (msg)
                (log (format #f "    ~a✅ pass - ~a~a"
                             (assoc-ref ansi-color-escapes
                                        'green) msg
                             (assoc-ref ansi-color-escapes
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
  `((log-pass unquote
              (lambda (msg)
                (log (format #f "    [pass] ~a" msg))))
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
