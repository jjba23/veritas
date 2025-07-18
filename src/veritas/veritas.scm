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
  #:export (assert-equal assert-true
                         assert-false
                         test
                         log-test
                         log-pass
                         log-suite
                         log-fail
                         suite
                         simple-check))

(define ansi-color-escapes
  `((yellow . "\x1b[0;33m") (magenta . "\x1b[0;35m")
    (green . "\x1b[0;32m")
    (default . "\x1b[0;39m")
    (bold . "\x1b[1m")
    (red . "\x1b[0;31m")
    (blue . "\x1b[0;34m")
    (cyan . "\x1b[0;36m")
    (reset . "\x1b[0m")))

;; (define-syntax-rule (when c e ...)
;;   (if c (begin e ...)))

(define (log-message msg)
  (display (format #f "\n~a" msg)))

(define (log msg)
  (log-message (apply format
                      (list #f msg))))

(define (log-pass msg)
  (log (format #f
               "[~apass~a] ~a~a~a"
               (assoc-ref ansi-color-escapes
                          'green)
               (assoc-ref ansi-color-escapes
                          'reset)
               (assoc-ref ansi-color-escapes
                          'green)
               (apply format
                      (list #f msg))
               (assoc-ref ansi-color-escapes
                          'reset))))

(define (log-fail msg)
  (log (format #f
               "[~afail~a] ~a~a~a"
               (assoc-ref ansi-color-escapes
                          'red)
               (assoc-ref ansi-color-escapes
                          'reset)
               (assoc-ref ansi-color-escapes
                          'red)
               (apply format
                      (list #f msg))
               (assoc-ref ansi-color-escapes
                          'reset))))

(define (log-test msg)
  (log (format #f
               "[~atest~a] ~a~a~a"
               (assoc-ref ansi-color-escapes
                          'bold)
               (assoc-ref ansi-color-escapes
                          'reset)
               (assoc-ref ansi-color-escapes
                          'bold)
               (apply format
                      (list #f msg))
               (assoc-ref ansi-color-escapes
                          'reset))))

(define (log-suite msg)
  (log (format #f
               "[~asuite~a] ~a~a~a"
               (assoc-ref ansi-color-escapes
                          'bold)
               (assoc-ref ansi-color-escapes
                          'reset)
               (assoc-ref ansi-color-escapes
                          'bold)
               (apply format
                      (list #f msg))
               (assoc-ref ansi-color-escapes
                          'reset))))

(define (assert-equal* x y compare-f)
  (begin
    (if (compare-f x y)
        (begin
          (write x)
          (write y)
          (log-pass (format #f "x: ~a, y: ~a" x y)) #t)
        (begin
          (write x)
          (write y)
          (log-fail (format #f "error matching x: ~a, y: ~a" x y)) #f))))

(define (assert-equal x y)
  (assert-equal* x y equal?))
(define (assert-eqv x y)
  (assert-equal* x y eqv?))
(define (assert-eq x y)
  (assert-equal* x y eq?))

(define (assert-true x)
  (assert-equal* x #t eq?))

(define (assert-false x)
  (assert-equal* x #f eq?))

(define (test name . tail)
  (log-test name)
  (let* ((idx -1)
         (asserts-i (map (lambda (a)
                           (set! idx
                                 (+ idx 1))
                           (list idx a)) tail)))
    `((test-name unquote name)
      (test-results unquote
                    (map (lambda (a)
                           `((assertion-index unquote
                                              (car a))
                             (expression unquote
                                         (car (cdr a)))
                             (result unquote
                                     (eval (car (cdr a))
                                           (interaction-environment)))))
                         asserts-i)))))

(define (suite name . tail)
  (log-suite name)
  (let* ((idx -1)
         (tests-i (map (lambda (t)
                         (set! idx
                               (+ idx 1))
                         (list idx t)) tail))
         (suite-results (map (lambda (t)
                               (write t)
                               (let* ((msg (format #f "~a"
                                                   (assoc-ref t
                                                              'test-name)))
                                      (cases (car (cdr t))))
                                 (test (append (list msg) cases)))) tests-i))
         (s `((suite-name unquote name)
              (suite-results unquote suite-results))))
    (newline)
    (write s) s))


