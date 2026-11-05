;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit numeric a num)
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:export (spec))

(define (add-two x)
  (+ 2 x))

(define basic-suite
  (suite "Basic Suite"
         (test "Basic Numeric Tests"
               (assert-equal #:expect 1
                             #:got (* 1 1 1 1 1))
               (assert-equal #:name "checks if addition works"
                             #:expect 4
                             #:got (+ 2 2))
               (assert-approximate #:name "arithmetic approximation"
                                   #:expect 1
                                   #:got 0.99999999999
                                   #:epsilon 1.0e-8)
               (assert-approximate #:name "arithmetic approximation"
                                   #:expect 1
                                   #:got 0.99999999999)
               (assert-not-approximate #:name "arithmetic approximation"
                                       #:expect 1
                                       #:got 0.9999999
                                       #:epsilon 1.0e-8)
               (assert-equal #:expect 0
                             #:got (* 1 1 1 1 0))
               (assert-equal #:expect 1
                             #:got (lazy
                                    (begin
                                      ;; lazy runs at test time, so slow
                                      ;; work could go here
                                      (+ 10
                                         (- 0 9))))))))

(define (is-even n)
  (= (modulo n 2) 0))

(define predicate-suite
  (suite "Predicate Tests"
         (test "Even Number Checks"
               (assert-true (is-even 4)
                            #:name "4 should be even")
               (assert-false (is-even 7)
                             #:name "7 should not be even")
               (assert-true (is-even 0)
                            #:name "0 should be even")
               (assert-false (is-even 1)
                             #:name "1 should not be even"))))

(define (spec)
  (veritas-run #:debug? #t predicate-suite basic-suite))
