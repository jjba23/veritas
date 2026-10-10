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

(define-module (veritas unit shuffle)
  #:use-module (srfi srfi-1)
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:use-module (veritas shuffle)
  #:export (spec))

;; A minimal context, so assertions can be built and inspected from a test.
(define vc (list (cons 'reporter 'monotone-reporter)))

(define (ok? assertion)
  (cdr (assq 'assertion-successful (assertion vc))))

;; Suites are concurrent by default, and one of these tests changes the run's
;; seed, so this file keeps its tests in a fixed order.
(define seed-suite
  (suite "seeded shuffling"
         #:concurrent? #f
         #:shuffle? #f
         (test "the same seed gives the same order"
               (lambda (vc)
                 ((assert-equal #:expect
                                (fisher-yates-shuffle '(1 2 3 4 5)
                                                      #:seed 7)
                                #:got (fisher-yates-shuffle '(1 2 3 4 5)
                                                            #:seed 7))
                  vc)))
         (test "two different seeds are not forced to agree"
               (lambda (vc)
                 ;; They may coincide, which is why this only checks that both
                 ;; are permutations of the input.
                 ((assert-equal #:expect '(1 2 3 4 5)
                                #:got (sort (fisher-yates-shuffle '(1 2 3 4 5)
                                                                  #:seed 1)
                                            <))
                  vc)
                 ((assert-equal #:expect '(1 2 3 4 5)
                                #:got (sort (fisher-yates-shuffle '(1 2 3 4 5)
                                                                  #:seed 2)
                                            <))
                  vc)))
         (test "short lists come back untouched"
               (lambda (vc)
                 ((assert-equal #:expect '() #:got (fisher-yates-shuffle '()
                                                                         #:seed 7))
                  vc)
                 ((assert-equal #:expect '(1) #:got (fisher-yates-shuffle '(1)
                                                                          #:seed 7))
                  vc)))
         (test "a fresh seed is a usable seed"
               (lambda (vc)
                 ((assert-true (integer? (fresh-seed))) vc)
                 ((assert-equal #:expect '(1 2 3)
                                #:got (sort (fisher-yates-shuffle '(1 2 3)
                                                                  #:seed (fresh-seed))
                                            <))
                  vc)))
         (test "an explicit seed is used and remembered"
               (lambda (vc)
                 ((assert-equal #:expect 7 #:got (current-seed #:seed 7)) vc)
                 ((assert-equal #:expect 7 #:got (current-seed)) vc)))
         (test "seeded-shuffle follows the run's seed"
               (lambda (vc)
                 (current-seed #:seed 11)
                 ((assert-equal #:expect (seeded-shuffle '(1 2 3 4))
                                #:got (fisher-yates-shuffle '(1 2 3 4)
                                                            #:seed 11))
                  vc)))))

(define (spec)
  (veritas-run #:debug? #f
               #:shuffle? #f
               seed-suite))
