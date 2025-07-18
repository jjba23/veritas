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
                         log
                         log-message
                         run-suite
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

(define (log-message msg)
  (display (format #f "\n~a" msg)))

(define (log msg)
  (log-message msg))

(define (log-pass msg)
  (log (format #f
               "[~apass~a] ~a~a~a"
               (assoc-ref ansi-color-escapes
                          'green)
               (assoc-ref ansi-color-escapes
                          'reset)
               (assoc-ref ansi-color-escapes
                          'green)
               msg
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
               msg
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
               msg
               (assoc-ref ansi-color-escapes
                          'reset))))

(define (log-suite msg)
  (log "------------------------------------------")
  (log (format #f
               "[~asuite~a] ~a~a~a"
               (assoc-ref ansi-color-escapes
                          'bold)
               (assoc-ref ansi-color-escapes
                          'reset)
               (assoc-ref ansi-color-escapes
                          'bold)
               msg
               (assoc-ref ansi-color-escapes
                          'reset))))

(define* (assert-equal* #:key expected sut compare)
  (begin
    (if (compare expected sut)
        (begin
          (log-pass (format #f "== expected: ~a, sut: ~a" expected sut)) #t)
        (begin
          (log-fail (format #f "error != expected: ~a, sut: ~a" expected sut))
          #f))))

(define* (assert-equal #:key expected sut)
  (assert-equal* #:expected expected
                 #:sut sut
                 #:compare equal?))

(define* (assert-eqv #:key expected sut)
  (assert-equal* #:expected expected
                 #:sut sut
                 #:compare eqv?))

(define* (assert-eq #:key expected sut)
  (assert-equal* #:expected expected
                 #:sut sut
                 #:compare eq?))

(define (assert-true x)
  (assert-equal* #:sut x
                 #:expected #t
                 #:compare eq?))

(define (assert-false x)
  (assert-equal* #:sut x
                 #:expected #f
                 #:compare eq?))

(define* (test name
               #:key (debug? #f)
               (cases '()))
  (let* ((_ (log-test name))
         (idx -1)
         (asserts-i (map (lambda (a)
                           (set! idx
                                 (+ idx 1))
                           (list idx a)) cases)))
    (when debug?
      (newline)
      (write cases))

    `((test-name unquote name)
      (test-results unquote
                    (map (lambda (a)
                           (write a)
                           `((assertion-index unquote
                                              (car a))
                             (results unquote
                                      (if (list? (car (cdr a)))
                                          (map (lambda (ll)
                                                 (apply ll
                                                        '()))
                                               (car (cdr a)))
                                          (apply (car (cdr a))
                                                 '()))))) asserts-i)))))

(define* (suite name
                #:key debug?
                (tests '()))
  (let* ((_ (log-suite name))
         (idx -1)
         (tests-i (map (lambda (t)
                         (set! idx
                               (+ idx 1))
                         (list idx
                               (apply t
                                      '()))) tests))
         (s `((suite-name unquote name)
              (suite-results unquote tests-i))))
    (when debug?
      (newline)
      (write s)
      (newline)) s))

(define* (run-suite x
                    #:key (fail-fast? #t))
  (let* ((suite-result (apply x
                              '())))
    suite-result))
