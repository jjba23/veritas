(define-module (veritas unit simple)
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:export (simple-check))

(define (add-two x)
  (+ 2 x))

(define basic-suite
  (suite "Basic Suite"
         (test "Basic Numeric Tests"
               (assert-equal #:expected 1
                             #:sut (+ 10
                                      (- 0 9)))
               (assert-equal #:expected 1
                             #:sut (* 1 1 1 1 1))
               (assert-equal #:name "checks if addition works"
                             #:expected 4
                             #:sut (+ 2 2))
               (assert-equal #:expected 0
                             #:sut (* 1 1 1 1 0)))
         (test "Basic Boolean Tests"
               #:debug? #t
               (assert-false #f
                             #:name
                             "should succeed on checking that false is false"))
         (test "Check that function calls work"
               (assert-equal #:expected 42
                             #:sut (add-two 40)))))

(define str-suite
  (suite "str"
         (test "String Operation Tests"
               (assert-equal #:expected "hello world"
                             #:sut (string-append "hello" " " "world"))
               (assert-true (string=? "scheme" "scheme"))
               (assert-false (string=? "guile" "Scheme")))))

(define (is-even n)
  (= (modulo n 2) 0))

(define (list-length lst)
  (length lst))

(define (get-first-element lst)
  (if (pair? lst)
      (car lst) #f))

(define (concatenate-lists list1 list2)
  (append list1 list2))

(define (divide a b)
  (if (= b 0)
      (error "Division by zero")
      (/ a b)))

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
               (assert-equal #:expected 0
                             #:sut (list-length '())
                             #:name "Length of empty list")
               (assert-equal #:expected 3
                             #:sut (list-length '(a b c))
                             #:name "Length of list with 3 elements")
               (assert-equal #:expected 1
                             #:sut (list-length '(10))
                             #:name "Length of list with one element"))
         (test "Get First Element"
               (assert-equal #:expected 1
                             #:sut (get-first-element '(1 2 3))
                             #:name "First element of a number list")
               (assert-equal #:expected 'a
                             #:sut (get-first-element '(a b c))
                             #:name "First element of a symbol list")
               (assert-false (get-first-element '())
                             #:name
                             "First element of an empty list should be #f"))
         (test "List Concatenation"
               (assert-equal #:expected '(1 2 3 4 5)
                             #:sut (concatenate-lists '(1 2 3)
                                                      '(4 5))
                             #:name "Concatenate two non-empty lists")
               (assert-equal #:expected '(a b)
                             #:sut (concatenate-lists '()
                                                      '(a b))
                             #:name "Concatenate empty list with non-empty")
               (assert-equal #:expected '(x y)
                             #:sut (concatenate-lists '(x y)
                                                      '())
                             #:name "Concatenate non-empty list with empty")
               (assert-equal #:expected '()
                             #:sut (concatenate-lists '()
                                                      '())
                             #:name "Concatenate two empty lists"))))

(define error-handling-suite
  (suite "Error Handling Tests"
         #:debug? #t
         (test "Division by Zero Error"
               (assert-error (lambda ()
                               (divide 10 0))
                             #:name "Should signal error on division by zero"))
         (test "No Error on Valid Division"
               (assert-no-error (lambda ()
                                  (divide 10 2))
                                #:name
                                "Should not signal error on valid division")
               (assert-equal #:expected 5
                             #:sut (divide 10 2)
                             #:name "Valid division result"))))

(define some-alist
  '((name . "Alice") (age . 30)
    (city . "New York")))

(define some-vector
  (vector 1 2 3))

(define complex-data-suite
  (suite "Complex Data Structure Tests"
         (test "Association List Lookups"

               (assert-equal #:expected "Alice"
                             #:sut (cdr (assoc 'name some-alist))
                             #:name "Lookup 'name' in alist")
               (assert-equal #:expected 30
                             #:sut (cdr (assoc 'age some-alist))
                             #:name "Lookup 'age' in alist")
               (assert-false (assoc 'country some-alist)
                             #:name "Lookup non-existent key should be #f"))
         (test "Vector Operations"

               (assert-equal #:expected 3
                             #:sut (vector-length some-vector)
                             #:name "Vector length")
               (assert-equal #:expected 2
                             #:sut (vector-ref some-vector 1)
                             #:name "Vector ref at index 1")
               (assert-equal #:expected 1
                             #:sut (vector-ref some-vector 0)
                             #:name "Vector ref at index 0"))))

(define (simple-check)
  (run-suite #:reporter colorful-stdout-reporter
             predicate-suite
             list-suite
             error-handling-suite
             complex-data-suite)
  (run-suite #:reporter colorful-stdout-reporter basic-suite str-suite))
