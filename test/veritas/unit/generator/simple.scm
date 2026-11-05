;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit generator simple)
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:export (spec))

(define basic-suite
  (let* ((rand (random-state-from-platform))
         (password-mock-generator (generate-list (generate-char)
                                                 #:min-len 8
                                                 #:max-len 64))
         (phone-generator (generate-list (generate-integer)))
         (int-generator (generate-integer))
         (str-generator (generate-string)))
    (suite "Basic Suite"
           (test "Basic Generation"
                 (assert-no-error (lambda ()
                                    (format #t "\n~a"
                                            (password-mock-generator rand))
                                    (format #t "\n~a"
                                            (password-mock-generator rand))))
                 (assert-no-error (lambda ()
                                    (format #t "\n~a"
                                            (phone-generator rand))
                                    (format #t "\n~a"
                                            (phone-generator rand))))
                 (assert-no-error (lambda ()
                                    (newline)
                                    (write (int-generator rand))
                                    (newline)
                                    (write (int-generator rand))))
                 (assert-no-error (lambda ()
                                    (newline)
                                    (write (str-generator (seed->random-state
                                                                              42)))
                                    (newline)
                                    (write (str-generator rand))))))))

(define (spec)
  (veritas-run #:debug? #f
               #:with-coverage? #t basic-suite))
