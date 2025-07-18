(define-module (veritas unit simple)
  #:use-module (veritas veritas)
  #:export (simple-check))

(define (add-two x)
  (+ 2 x))

(define (basic-suite)
  (suite "Basic Suite"
         (lambda ()
           (test "Basic Numeric Tests"
                 (lambda ()
                   (assert-equal #:expected 1
                                 #:sut (+ 10
                                          (- 0 9))))
                 (lambda ()
                   (assert-equal #:expected 1
                                 #:sut (* 1 1 1 1 1)))
                 (lambda ()
                   "checks if addition works"
                   (assert-equal #:expected 4
                                 #:sut (+ 2 2)))
                 (lambda ()
                   (assert-equal #:expected 0
                                 #:sut (* 1 1 1 1 0)))))
         (lambda ()
           (test "Basic Boolean Tests"
                 #:debug? #t
                 (lambda ()
                   "Ridiculous assertion to prove tests can fail if checking that true is false"
                   (assert-false #t))))
         (lambda ()
           (test "Check that function calls work"
                 #:debug? #f
                 (lambda ()
                   (assert-equal #:expected 42
                                 #:sut (add-two 40)))))))

(define (str-suite)
  (suite "str"
         #:debug? #f
         (lambda ()
           (test "String Operation Tests"
                 #:debug? #f
                 (lambda ()
                   (assert-equal #:expected "hello world"
                                 #:sut (string-append "hello" " " "world")))
                 (lambda ()
                   (assert-true (string=? "scheme" "scheme")))
                 (lambda ()
                   (assert-false (string=? "guile" "Scheme")))))))

(define (simple-check)
  (run-suite basic-suite str-suite))

