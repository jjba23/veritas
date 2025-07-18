(define-module (veritas unit simple)
  #:use-module (veritas veritas))

(assert-true (eq? 1 2)
             #:message "1 should be equal to 2")

(assert-true (eq? 1 1) #:message "1 should be equal to 1")
