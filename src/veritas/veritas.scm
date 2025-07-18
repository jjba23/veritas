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

(define-module (veritas veritas)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:export (assert-equal assert-true assert-false test))

(define ansi-color-escapes
  `((yellow . "\x1b[0;33m") (magenta . "\x1b[0;35m")
    (green . "\x1b[0;32m")
    (default . "\x1b[0;39m")
    (red . "\x1b[0;31m")
    (blue . "\x1b[0;34m")
    (cyan . "\x1b[0;36m")
    (reset . "\x1b[0m")))

(define (log-message msg)
  (display (format #f "\n~a\n" msg)))

(define (log msg . args)
  (log-message (apply format
                      (append (list #f msg) args))))

(define (log-pass msg . args)
  (log-message (format #f
                       "[~aPASS~a] ~a~a~a"
                       (assoc-ref ansi-color-escapes
                                  'green)
                       (assoc-ref ansi-color-escapes
                                  'reset)
                       (assoc-ref ansi-color-escapes
                                  'green)
                       (apply format
                              (append (list #f msg) args))
                       (assoc-ref ansi-color-escapes
                                  'reset))))

(define (log-fail msg . args)
  (log-message (format #f
                       "[~aFAIL~a] ~a~a~a"
                       (assoc-ref ansi-color-escapes
                                  'red)
                       (assoc-ref ansi-color-escapes
                                  'reset)
                       (assoc-ref ansi-color-escapes
                                  'red)
                       (apply format
                              (append (list #f msg) args))
                       (assoc-ref ansi-color-escapes
                                  'reset))))

(define* (assert-equal left right
                       #:key (compare-f equal?))
  (if (compare-f left right)
      (begin
        (log-pass "assert-equal, function: ~s, left: ~a, right: ~a"
                  (procedure-name compare-f) left right) #t)
      (begin
        (log-fail
         "assert-equal did not match expectation, function: ~s, left: ~a, right: ~a"
         (procedure-name compare-f) left right) #f)))

(define* (assert-true x)
  (assert-equal x #t))

(define* (assert-false x)
  (assert-equal x #f))

;;(assert-equal 1 1)

(define (evaluate-expression-list expr-list)
  (map (lambda (expr) (eval expr (interaction-environment))) expr-list))

(define (test msg asserts)
  (log-message msg)
  (write asserts)
  (evaluate-expression-list asserts))

;; (test ""  '((assert-equal 1 1) (assert-equal 1 2)))
;;(test "" '(assert-equal 1 1))
