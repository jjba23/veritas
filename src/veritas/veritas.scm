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
                         log-assert
                         suite
                         shuffle
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
               "    [~a✅ pass~a] ~a~a~a"
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
               "    [~a❌ fail~a] ~a~a~a"
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
               "  [~a🧪 test~a] ~a~a~a"
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
               "[~a📑 suite~a] ~a~a~a"
               (assoc-ref ansi-color-escapes
                          'bold)
               (assoc-ref ansi-color-escapes
                          'reset)
               (assoc-ref ansi-color-escapes
                          'bold)
               msg
               (assoc-ref ansi-color-escapes
                          'reset))))

(define (log-assert msg)
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

;; List shuffle which implements the modern Fisher-Yates shuffle algorithm.
;; It converts the list to a vector for efficient O(1) random access
;; and mutation, shuffles the vector in-place, and then converts it back to a list.
(define (shuffle lst)
  "Returns a randomly shuffled copy of LST."
  (set! *random-state*
        (random-state-from-platform))
  (if (or (null? lst)
          (null? (cdr lst)))
      ;; If the list has 0 or 1 elements, it's already "shuffled".
      lst
      (let* ((vec (list->vector lst))
             (len (vector-length vec)))
        ;; Loop from the last element down to the second element.
        (do ((i (- len 1)
                (- i 1)))
            ((< i 1))
            ;; Pick a random index j from 0 to i (inclusive).
            (let* ((j (random (1+ i)))
                   ;; Swap the elements at positions i and j.
                   (temp (vector-ref vec i)))
              (vector-set! vec i
                           (vector-ref vec j))
              (vector-set! vec j temp)))
        ;; Convert the shuffled vector back to a list.
        (vector->list vec))))

(define* (test name
               #:key (debug? #f)
               (shuffle? #t)
               #:rest cases)
  (let* ((_ (log-test name))
         (idx -1)
         (ccc (filter procedure? cases))
         (asserts-i (map (lambda (a)
                           (set! idx
                                 (+ idx 1))
                           (list idx a))
                         (if shuffle?
                             (shuffle ccc) ccc))))
    
    `((test-name unquote name)
      (test-results unquote
                    (map (lambda (a)
                           (let* ((proc-doc (procedure-documentation (car (cdr
                                                                           a)))))
                             (if proc-doc
                                 (log-assert proc-doc)
                                 (log-assert ""))
                             `((assertion-index unquote
                                                (car a))
                               (assertion-documentation unquote proc-doc)
                               (assertion-succesful unquote
                                                    (apply (car (cdr a))
                                                           '()))))) asserts-i)))))

(define* (suite name
                #:key debug?
                (shuffle? #t)
                #:rest tests)
  (let* ((_ (log-suite name))
         (idx -1)
         (ttt (filter procedure? tests))
         (tests-i (map (lambda (t)
                         (set! idx
                               (+ idx 1))
                         (list idx
                               (apply t
                                      '())))
                       (if shuffle?
                           (shuffle ttt) ttt)))
         (s `((suite-name unquote name)
              (suite-results unquote tests-i))))
    (when debug?
      (newline)
      (write s)
      (newline)) s))

(define* (run-suite #:key (fail-fast? #t)
                    #:rest suites)
  (let* ((ss (filter procedure? suites))
         (suite-results (map (lambda (s)
                               (apply s
                                      '())) ss)))
    suite-results))
