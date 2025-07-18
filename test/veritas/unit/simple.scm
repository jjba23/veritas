(define-module (veritas unit simple)
  #:use-module (veritas veritas)
  #:export (simple-check))

(define (add-two x)
  (+ 2 x))

(define (simple-check)
  (define tt
    (list (lambda ()
            (test "do some numeric checks"
                  #:debug? #t
                  #:cases (list (lambda ()
                                  (assert-equal #:expected 1
                                                #:sut (+ 10
                                                         (- 0 9))))
                                (lambda ()
                                  (assert-equal #:expected 1
                                                #:sut (* 1 1 1 1 1))))))
          (lambda ()
            (test "check that function calls work"
                  #:debug? #t
                  #:cases (list (lambda ()
                                  (assert-equal #:expected 42
                                                #:sut (add-two 40))))))))
  (run-suite (lambda ()
               (suite "simple-check"
                      #:debug? #t
                      #:tests tt)))

  (define ttstr
    (list (lambda ()
            (test "String Operations"
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
                      #:tests ttstr))))

