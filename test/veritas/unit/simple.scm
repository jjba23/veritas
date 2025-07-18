(define-module (veritas unit simple)
  #:use-module (veritas veritas)
  #:export (simple-check))

(define (add-two x)
  (+ 2 x))

(define some-suite
  (suite "smaller"
         #:debug? #t
         '(test "String Operations"
                (assert-equal "hello world"
                              (string-append "hello" " " "world"))
                (assert-equal #t
                              (string=? "scheme" "scheme"))
                (assert-equal #f
                              (string=? "guile" "Scheme")))))

(define numeric-suite
  (suite "simple-check"
         #:debug? #t
         '(test "do some numeric checks"
                (assert-equal 1 1)
                (assert-equal 1 2))
         `(test "check that function calls work"
                (assert-equal ,(add-two 40) 42))))

(define (simple-check)
  (log-message ""))

