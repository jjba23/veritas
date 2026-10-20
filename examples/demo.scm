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

;;; Commentary:
;;
;; The demo the README's animated GIF was recorded from, and the shortest way to
;; see what a Veritas run looks like.  A suite of ordinary assertions, a
;; table-driven suite, a Given/When/Then scenario, and one check that fails on
;; purpose so you can see how a failure reads.
;;
;; Run it with:
;;
;;   guix shell -m manifest.scm -- guile -L ./src examples/demo.scm
;;
;;; Code:

(use-modules (ice-9 format)
             (veritas veritas)
             (veritas bdd)
             (veritas table))

(define (is-even n)
  (= (modulo n 2) 0))

(define (factorial n)
  (if (= n 0) 1
      (* n
         (factorial (- n 1)))))

(define arithmetic-suite
  (suite "Arithmetic"
         #:shuffle? #f
         #:concurrent? #f
         (test "is-even"
               (assert-true (is-even 4)
                            #:name "4 is even")
               (assert-false (is-even 7)
                             #:name "7 is odd"))
         (test "factorial"
               (assert-equal #:expect 120
                             #:got (factorial 5)
                             #:name "5! is 120"))))

(define addition-suite
  (test-each "addition, one test per row"
             '((1 2 3)
               (10 20 30))
             (lambda (row)
               (let ((a (car row))
                     (b (cadr row))
                     (expected (caddr row)))
                 (test (format #f "~a + ~a = ~a" a b expected)
                       (assert-equal #:expect expected
                                     #:got (+ a b)))))))

(define withdraw-scenario
  (scenario "withdrawing money"
    (let ((balance 100))
      (given "an account with 100 in it"
        (assert-equal #:expect 100 #:got balance))
      (when-step "20 is withdrawn"
        (assert-equal #:expect 80 #:got (- balance 20)))
      (then "the balance is still positive"
        (assert-true (> balance 0))))))

;; Meant to fail: a report is only trustworthy if a failure reads clearly.
(define failing-check
  (test "a check that fails on purpose"
        (assert-equal #:expect 5
                      #:got (+ 2 2)
                      #:name "2 + 2 is 5 (it is not)")))

(veritas-run #:shuffle? #f
             #:concurrent? #f
             arithmetic-suite
             addition-suite
             withdraw-scenario
             failing-check)
