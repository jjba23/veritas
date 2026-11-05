;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit lists)
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:export (spec))

(define (get-first-element lst)
  (if (pair? lst)
      (car lst) #f))

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

(define (spec)
  (veritas-run list-suite))

