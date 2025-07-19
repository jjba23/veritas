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
  #:export (colorful-stdout-reporter log-stdout ansi-color-escapes
                                     monotone-stdout-reporter))

(define ansi-color-escapes
  `((yellow . "\x1b[0;33m") (magenta . "\x1b[0;35m")
    (green . "\x1b[0;32m")
    (default . "\x1b[0;39m")
    (bold . "\x1b[1m")
    (red . "\x1b[0;31m")
    (blue . "\x1b[0;34m")
    (cyan . "\x1b[0;36m")
    (reset . "\x1b[0m")))

(define (log-stdout msg)
  (display (format #f "\n~a" msg)))

(define colorful-stdout-reporter
  `((log-pass unquote
              (lambda (msg)
                (log-stdout (format #f "    ~a✅ pass - ~a~a"
                                    (assoc-ref ansi-color-escapes
                                               'green) msg
                                    (assoc-ref ansi-color-escapes
                                               'reset)))))
    (log-fail unquote
              (lambda (msg)
                (log-stdout (format #f "    ~a❌ fail - ~a~a"
                                    (assoc-ref ansi-color-escapes
                                               'red) msg
                                    (assoc-ref ansi-color-escapes
                                               'reset)))))
    (log-error unquote
               (lambda (msg)
                 (log-stdout (format #f "\n~a❌ error - ~a~a\n"
                                     (assoc-ref ansi-color-escapes
                                                'red) msg
                                     (assoc-ref ansi-color-escapes
                                                'reset)))))
    (log-test unquote
              (lambda (msg)
                (log-stdout (format #f
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
                 (log-stdout "------------------------------------------")
                 (log-stdout (format #f
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
                  (log-stdout (format #f
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

(define monotone-stdout-reporter
  `((log-pass unquote
              (lambda (msg)
                (log-stdout (format #f "    [pass] ~a" msg))))
    (log-fail unquote
              (lambda (msg)
                (log-stdout (format #f "    [fail] ~a"

                                    msg))))
    (log-error unquote
               (lambda (msg)
                 (log-stdout (format #f "\n[error] ~a\n"

                                     msg))))
    (log-test unquote
              (lambda (msg)
                (log-stdout (format #f "  [test] ~a"

                                    msg))))
    (log-suite unquote
               (lambda (msg)
                 (log-stdout "------------------------------------------")
                 (log-stdout (format #f "[suite] ~a"

                                     msg))))
    (log-assert unquote
                (lambda (msg)
                  (log-stdout (format #f "    [assert] ~a" msg))))))
