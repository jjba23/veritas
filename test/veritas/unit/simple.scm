(define-module (veritas unit simple)
  #:use-module (veritas veritas)
  #:export (simple-check))

(define some-suite
  '(suite "str"
          #:debug? #t
          '(test "String Operations"
                 (assert-equal "hello world"
                               (string-append "hello" " " "world"))
                 (assert-equal #t
                               (string=? "scheme" "scheme"))
                 (assert-equal #f
                               (string=? "guile" "Scheme")))))

(define numeric-suite
  `(suite "simple-check"
          #:debug? #t
          '(test "do some numeric checks"
                 (assert-equal 1 1)
                 (assert-equal 1 2))
          '(test "check that function calls work"
                 (assert-equal (+ 2 40) 42))))

(define (simple-check)
  (use-modules (veritas veritas))
  (eval numeric-suite
        (interaction-environment))
  (run-suite some-suite))

