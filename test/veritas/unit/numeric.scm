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

(define-module (veritas unit numeric)
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
               (assert-equal #:expect 0
                             #:got (* 1 1 1 1 0))
               (assert-equal #:expect 1
                             #:got `(comp (begin
                                            ;; computations can be done here
                                            ;; (sleep 5)
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
  (veritas-run #:reporter colorful-reporter
               #:debug? #f
               predicate-suite
               basic-suite))
