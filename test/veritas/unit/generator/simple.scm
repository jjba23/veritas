;; veritas

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; veritas is free software: you can redistribute it and/or modify
;; it under the terms of the Lesser GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; veritas is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; Lesser GNU General Public License for more details.

;; You should have received a copy of the Lesser GNU General Public License
;; along with veritas.  If not, see <https://www.gnu.org/licenses/>.

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
