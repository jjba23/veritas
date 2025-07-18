(define-module (veritas unit simple)
  #:use-module (veritas veritas)
  #:export (simple-check))

(define (simple-check)
  (use-modules (veritas veritas))
  (suite "simple-check"
         '((test "do some numeric checks"
               ((assert-equal 1 1)
                 (assert-equal 1 2))))))
