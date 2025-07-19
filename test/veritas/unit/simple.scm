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
               (assert-false #t
                             #:name
                             "should fail on checking that true is false"))
         (test "Check that function calls work"
               #:debug? #f
               (assert-equal #:expected 42
                             #:sut (add-two 40)))))

(define str-suite
  (suite "str"
         #:debug? #f
         #:reporter monotone-stdout-reporter
         (test "String Operation Tests"
               #:debug? #f
               #:reporter monotone-stdout-reporter
               (assert-equal #:expected "hello world"
                             #:sut (string-append "hello" " " "world")
                             #:reporter monotone-stdout-reporter)
               (assert-true (string=? "scheme" "scheme")
                            #:reporter monotone-stdout-reporter)
               (assert-false (string=? "guile" "Scheme")
                             #:reporter monotone-stdout-reporter))))

(define (simple-check)
  (run-suite #:reporter monotone-stdout-reporter basic-suite str-suite))

