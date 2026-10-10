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

(define-module (veritas unit bdd)
  #:use-module (veritas veritas)
  #:use-module (veritas bdd)
  #:use-module (veritas reporter)
  #:export (spec))

;; A scenario over immutable data: pure checks read exactly like English.
(define numbers-scenario
  (scenario "summing a list of numbers"
    (given "a list of three numbers"
      (assert-equal #:expect 3 #:got (length '(1 2 3))))
    (when-step "the numbers are summed"
      (assert-equal #:expect 6 #:got (apply + '(1 2 3))))
    (then "the total is correct"
      (assert-equal #:expect 6 #:got (apply + '(1 2 3))))
    (and-step "the list is non-empty"
      (assert-not-empty #:got '(1 2 3)))
    (but-step "the list does not contain four"
      (assert-not-contains #:expect '(1 2 3) #:got 4))))

;; Side effects and their checks must be deferred inside thunks, because Veritas
;; assertion arguments are evaluated when the assertion is built.
(define events '())

(define ordered-scenario
  (scenario "steps run in declaration order"
    (given "no recorded events"
      (assert-no-error (lambda ()
                         (set! events '())
                         (null? events))))
    (when-step "the first event is recorded"
      (assert-no-error (lambda ()
                         (set! events (cons 'first events))
                         (equal? events '(first)))))
    (then "the order is preserved"
      (assert-no-error (lambda ()
                         (equal? events '(first)))))))

;; The Latin aliases must behave exactly like their English counterparts.
(define latin-scenario
  (fabula "a scenario written in latin"
    (dato "a list of two numbers"
      (assert-equal #:expect 2 #:got (length '(10 20))))
    (quando "the numbers are summed"
      (assert-equal #:expect 30 #:got (+ 10 20)))
    (tunc "the total is thirty"
      (assert-equal #:expect 30 #:got (+ 10 20)))
    (etiam "the values are positive"
      (assert-true (and (> 10 0) (> 20 0))))
    (sed "the list is not empty"
      (assert-not-empty #:got '(10 20)))))

;; Steps must execute in declaration order even though a scenario is built as
;; an ordinary suite, and #:before-all must run before every step.
(define bdd-trace '())

(define ordered-steps-scenario
  (scenario "scenario steps keep declaration order"
    #:before-all (lambda (vc)
                   (set! bdd-trace '())
                   (set! bdd-trace (cons 'before-all bdd-trace)))
    (given "the first step records itself"
      (assert-no-error (lambda ()
                         (set! bdd-trace (cons 'given bdd-trace))
                         #t)))
    (when-step "the second step records itself"
      (assert-no-error (lambda ()
                         (set! bdd-trace (cons 'when bdd-trace))
                         #t)))
    (then "the third step records itself"
      (assert-no-error (lambda ()
                         (set! bdd-trace (cons 'then bdd-trace))
                         #t)))
    (and-step "the recorded order is Given, When, Then"
      (assert-no-error (lambda ()
                         (equal? (reverse bdd-trace)
                                 '(before-all given when then)))))))

;; A scenario is a suite, so scenarios compose: an inner scenario can appear as
;; a step of an outer one and is nested in the report.
(define nested-scenario
  (scenario "outer scenario"
    (scenario "inner scenario"
      (given "inner given"
        (assert-true #t))
      (then "inner then"
        (assert-equal #:expect 2 #:got (+ 1 1))))
    (then "outer then"
      (assert-true (> 2 1)))))

(define (spec)
  (veritas-run #:debug? #f
               numbers-scenario
               ordered-scenario
               latin-scenario
               ordered-steps-scenario
               nested-scenario))
