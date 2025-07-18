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
  #:use-module (veritas reporter)
  #:export (assert-equal assert-true
                         assert-false
                         test
                         test-reporter
                         run-suite
                         suite
                         shuffle
                         simple-check))

(define test-reporter
  (make-parameter colorful-stdout-reporter))

(define* (assert-equal* #:key expected sut compare)
  (if (compare expected sut)
      (begin
        (apply (assoc-ref (test-reporter)
                          'log-pass)
               (list (format #f "== expected: ~a, sut: ~a" expected sut))) #t)
      (begin
        (apply (assoc-ref (test-reporter)
                          'log-fail)
               (list (format #f "error != expected: ~a, sut: ~a" expected sut)))

        #f)))

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

(define* (assert-true x)
  (assert-equal* #:sut x
                 #:expected #t
                 #:compare eq?))

(define* (assert-false x)
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
  (let* ((_ (apply (assoc-ref (test-reporter)
                              'log-test)
                   (list name)))
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
                                 (apply (assoc-ref (test-reporter)
                                                   'log-assert)
                                        (list proc-doc))
                                 (apply (assoc-ref (test-reporter)
                                                   'log-assert)
                                        (list "")))
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
  (let* ((_ (apply (assoc-ref (test-reporter)
                              'log-suite)
                   (list name)))
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
