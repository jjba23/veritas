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

(define-module (veritas unit simple)
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:export (spec))

(define (add-two x)
  (+ 2 x))

(define basic-suite
  (suite "Basic Suite"
         (test "Basic Numeric Tests"
               (assert-equal #:expect 1
                             #:got (+ 10
                                      (- 0 9)))
               (assert-equal #:expect 1
                             #:got (* 1 1 1 1 1))
               (assert-equal #:name "checks if addition works"
                             #:expect 4
                             #:got (+ 2 2))
               (assert-equal #:expect 0
                             #:got (* 1 1 1 1 0)))
         (test "Basic Boolean Tests"
               #:skip? #t
               (assert-false #f
                             #:name
                             "should succeed on checking that false is false"))
         (test "Check that function calls work"
               (assert-equal #:expect 42
                             #:got (add-two 40)))))

(define str-suite
  (suite "str"
         (test "String Operation Tests"
               (assert-equal #:expect "hello world"
                             #:got (string-append "hello" " " "world"))
               (assert-true (string=? "scheme" "scheme"))
               (assert-false (string=? "guile" "Scheme")))))

(define (is-even n)
  (= (modulo n 2) 0))

(define (get-first-element lst)
  (if (pair? lst)
      (car lst) #f))

(define (divide a b)
  (if (= b 0)
      (error "Division by zero")
      (/ a b)))

(define some-alist
  '((name . "Alice") (age . 30)
    (city . "New York")))

(define some-vector
  (vector 1 2 3))

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
                             #:name "1 should not be even"))
         (test "List Predicates"
               (assert-true (null? '())
                            #:name "Empty list should be null")
               (assert-false (null? '(1 2))
                             #:name "Non-empty list should not be null")
               (assert-true (pair? '(1 2))
                            #:name "Pair should be a pair")
               (assert-false (pair? '())
                             #:name "Empty list should not be a pair"))))

(define list-suite
  (suite "List Manipulation Tests"
         (test "List Length"
               (assert-equal #:expect 0
                             #:got (length '())
                             #:name "Length of empty list")
               (assert-equal #:expect 3
                             #:got (length '(a b c))
                             #:name "Length of list with 3 elements")
               (assert-equal #:expect 1
                             #:got (length '(10))
                             #:name "Length of list with one element"))
         (test "Get First Element"
               (assert-equal #:expect 1
                             #:got (get-first-element '(1 2 3))
                             #:name "First element of a number list")
               (assert-equal #:expect 'a
                             #:got (get-first-element '(a b c))
                             #:name "First element of a symbol list")
               (assert-false (get-first-element '())
                             #:name
                             "First element of an empty list should be #f"))
         (test "List Concatenation"
               (assert-equal #:expect '(1 2 3 4 5)
                             #:got (append '(1 2 3)
                                           '(4 5))
                             #:name "Concatenate two non-empty lists")
               (assert-equal #:expect '(a b)
                             #:got (append '()
                                           '(a b))
                             #:name "Concatenate empty list with non-empty")
               (assert-equal #:expect '(x y)
                             #:got (append '(x y)
                                           '())
                             #:name "Concatenate non-empty list with empty")
               (assert-equal #:expect '()
                             #:got (append '()
                                           '())
                             #:name "Concatenate two empty lists"))))

(define error-handling-suite
  (suite "Error Handling Tests"
         (test "Division by Zero Error"
               (assert-error (lambda ()
                               (divide 10 0))
                             #:name "Should signal error on division by zero"))
         (test "No Error on Valid Division"
               (assert-no-error (lambda ()
                                  (divide 10 2))
                                #:name
                                "Should not signal error on valid division")
               (assert-equal #:expect 5
                             #:got (divide 10 2)
                             #:name "Valid division result"))))

(define complex-data-suite
  (suite "Complex Data Structure Tests"
         (test "Association List Lookups"

               (assert-equal #:expect "Alice"
                             #:got (cdr (assoc 'name some-alist))
                             #:name "Lookup 'name' in alist")
               (assert-equal #:expect 30
                             #:got (cdr (assoc 'age some-alist))
                             #:name "Lookup 'age' in alist")
               (assert-false (assoc 'country some-alist)
                             #:name "Lookup non-existent key should be #f"))
         (test "Vector Operations"

               (assert-equal #:expect 3
                             #:got (vector-length some-vector)
                             #:name "Vector length")
               (assert-equal #:expect 2
                             #:got (vector-ref some-vector 1)
                             #:name "Vector ref at index 1")
               (assert-equal #:expect 1
                             #:got (vector-ref some-vector 0)
                             #:name "Vector ref at index 0"))))

(define (factorial n)
  (if (= n 0) 1
      (* n
         (factorial (- n 1)))))

(define simple-factorial-benchmark
  (benchmark #:name "Recursive computation of the factorial of 10000"
             #:runs 3
             #:thunk (lambda ()
                       (factorial 10000))))

(define (spec)
  (veritas-run #:reporter colorful-reporter
               ;; ⤴ or monotone-reporter, or your own implementation
               #:debug? #f
               predicate-suite
               list-suite
               error-handling-suite
               complex-data-suite
               basic-suite
               simple-factorial-benchmark
               (test "some test without suite"
                     (assert-equal #:expect 1
                                   #:got (* 1 1 1 1)))
               str-suite))
