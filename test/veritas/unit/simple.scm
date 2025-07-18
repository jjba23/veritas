(define-module (veritas unit simple)
  #:use-module (veritas veritas)
  #:export (simple-check))

(define (add-two x)
  (+ 2 x))

(define (simple-check)
  (define numeric-tests
    (list (lambda ()
            (test "Simple Numeric Tests"
                  #:debug? #t
                  #:cases (list (lambda ()
                                  (assert-equal #:expected 1
                                                #:sut (+ 10
                                                         (- 0 9))))
                                (lambda ()
                                  (assert-equal #:expected 1
                                                #:sut (* 1 1 1 1 1)))
                                (lambda ()
                                  (assert-equal #:expected 0
                                                #:sut (* 1 1 1 1 0))))))
          (lambda ()
            (test "check that function calls work"
                  #:debug? #t
                  #:cases (list (lambda ()
                                  (assert-equal #:expected 42
                                                #:sut (add-two 40))))))))
  (run-suite (lambda ()
               (suite "simple-check"
                      #:debug? #t
                      #:tests numeric-tests)))

  (define string-operation-tests
    (list (lambda ()
            (test "String Operation Tests"
                  #:debug? #f
                  #:cases (list (lambda ()
                                  (assert-equal #:expected "hello world"
                                                #:sut (string-append "hello"
                                                                     " "
                                                                     "world")))
                                (lambda ()
                                  (assert-true (string=? "scheme" "scheme")))
                                (lambda ()
                                  (assert-false (string=? "guile" "Scheme"))))))))
  (run-suite (lambda ()
               (suite "str"
                      #:debug? #t
                      #:tests string-operation-tests))))

