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

(define-module (veritas unit matchers)
  #:use-module (ice-9 regex)
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:export (spec))

;; A minimal context so that assertions can be run directly from a test and
;; their result inspected, without affecting the runner's own bookkeeping.
(define vc (list (cons 'reporter 'monotone-reporter)))

(define (assertion-succeeded? result)
  (cdr (assq 'assertion-successful result)))

(define passing-suite
  (suite "passing matchers"
         (test "containment"
               (assert-contains #:expect '(a b c) #:got 'b)
               (assert-contains #:expect "hello world" #:got "world")
               (assert-contains #:expect #(1 2 3) #:got 2)
               (assert-not-contains #:expect '(a b) #:got 'z)
               (assert-not-contains #:expect "abc" #:got "z"))
         (test "regular expressions"
               (assert-matches #:pattern "^h.*o$" #:got "hello")
               (assert-matches #:pattern "b" #:got "abc")
               (assert-not-matches #:pattern "^x" #:got "hello"))
         (test "emptiness and length"
               (assert-empty #:got '())
               (assert-empty #:got "")
               (assert-empty #:got #())
               (assert-not-empty #:got '(1))
               (assert-not-empty #:got "a")
               (assert-length #:expect 3 #:got '(1 2 3))
               (assert-length #:expect 5 #:got "hello"))
         (test "ranges and membership"
               (assert-in-range #:min 1 #:max 10 #:got 5)
               (assert-in-range #:min 0 #:max 0 #:got 0)
               (assert-one-of #:expect '(red green blue) #:got 'green))
         (test "raising errors"
               (assert-raises (lambda () (+ 1 'a)) #:on 'wrong-type-arg)
               (assert-raises (lambda () (error "boom")))
               (assert-raises (lambda () (error "boom"))
                              #:predicate (lambda (key args) (equal? key 'misc-error))))))

(define wiring-suite
  (suite "matcher result wiring"
         (test "failing matchers are reported as failures"
               (assert-equal #:expect #t
                             #:got (assertion-succeeded?
                                    ((assert-contains #:expect '(1 2) #:got 2) vc)))
               (assert-equal #:expect #f
                             #:got (assertion-succeeded?
                                    ((assert-contains #:expect '(1) #:got 2) vc))))
         (test "assert-raises fails when nothing is raised"
               (assert-equal #:expect #f
                             #:got (assertion-succeeded?
                                    ((assert-raises (lambda () 42)) vc))))))

(define edge-hash (make-hash-table))
(hash-set! edge-hash 'a 1)

(define edge-suite
  (suite "matcher edge cases and failures"
         (test "hash tables are containers too"
               (assert-true (assertion-succeeded?
                             ((assert-contains #:expect edge-hash #:got 'a) vc)))
               (assert-false (assertion-succeeded?
                              ((assert-contains #:expect edge-hash #:got 'b) vc))))
         (test "vector containment"
               (assert-true (assertion-succeeded?
                             ((assert-contains #:expect #(1 2 3) #:got 2) vc)))
               (assert-false (assertion-succeeded?
                              ((assert-contains #:expect #(1 2 3) #:got 9) vc))))
         (test "not-contains"
               (assert-true (assertion-succeeded?
                             ((assert-not-contains #:expect '(1 2) #:got 9) vc)))
               (assert-false (assertion-succeeded?
                              ((assert-not-contains #:expect '(1 2) #:got 2) vc))))
         (test "matches accepts a compiled regexp"
               (assert-true (assertion-succeeded?
                             ((assert-matches #:pattern (make-regexp "^h.*o$")
                                              #:got "hello") vc)))
               (assert-false (assertion-succeeded?
                              ((assert-matches #:pattern "z" #:got "hello") vc))))
         (test "not-matches"
               (assert-true (assertion-succeeded?
                             ((assert-not-matches #:pattern "^x" #:got "hello") vc)))
               (assert-false (assertion-succeeded?
                              ((assert-not-matches #:pattern "e" #:got "hello") vc))))
         (test "in-range boundaries are inclusive"
               (assert-true (assertion-succeeded?
                             ((assert-in-range #:min 1 #:max 10 #:got 1) vc)))
               (assert-true (assertion-succeeded?
                             ((assert-in-range #:min 1 #:max 10 #:got 10) vc)))
               (assert-false (assertion-succeeded?
                              ((assert-in-range #:min 1 #:max 10 #:got 11) vc))))
         (test "one-of"
               (assert-true (assertion-succeeded?
                             ((assert-one-of #:expect '(red green) #:got 'green) vc)))
               (assert-false (assertion-succeeded?
                              ((assert-one-of #:expect '(red green) #:got 'blue) vc))))
         (test "length on vectors and strings"
               (assert-true (assertion-succeeded?
                             ((assert-length #:expect 3 #:got #(1 2 3)) vc)))
               (assert-true (assertion-succeeded?
                             ((assert-length #:expect 5 #:got "hello") vc)))
               (assert-false (assertion-succeeded?
                              ((assert-length #:expect 4 #:got "hello") vc))))
         (test "length and emptiness of unsized values"
               (assert-false (assertion-succeeded?
                              ((assert-length #:expect 1 #:got 42) vc)))
               (assert-false (assertion-succeeded?
                              ((assert-empty #:got 42) vc)))
               (assert-false (assertion-succeeded?
                              ((assert-not-empty #:got '()) vc)))
               (assert-true (assertion-succeeded?
                             ((assert-not-empty #:got #(1)) vc))))
         (test "assert-raises failure paths"
               (assert-false (assertion-succeeded?
                              ((assert-raises (lambda ()
                                                42)) vc)))
               (assert-false (assertion-succeeded?
                              ((assert-raises (lambda ()
                                                (+ 1 'a))
                                              #:on 'misc-error) vc)))
               (assert-true (assertion-succeeded?
                             ((assert-raises (lambda ()
                                               (+ 1 'a))
                                             #:on 'wrong-type-arg) vc))))))

(define (spec)
  (veritas-run #:debug? #f passing-suite wiring-suite edge-suite))
