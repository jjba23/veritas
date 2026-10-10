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
;; Behaviour-driven (Given/When/Then) helpers for Veritas, in the spirit of
;; Cucumber/Gherkin scenarios.  A scenario is a suite whose steps are ordinary
;; tests, so every existing reporter, filter and runner feature keeps working.
;; Step names are prefixed with their keyword so the output reads naturally.
;;
;; Steps run in order by default: scenarios disable shuffling and concurrency so
;; that Given -> When -> Then always executes sequentially.
;;
;; State is shared between steps simply by closing over a `let' binding in the
;; scenario body, for example:
;;
;;   (scenario "withdrawing too much"
;;     (let ((account (make-account 100)))
;;       (given "an account with 100"
;;         (assert-equal #:expect 100 #:got (account-balance account)))
;;       (when-step "withdrawing 150"
;;         (assert-false (withdraw account 150)))
;;       (then "the balance is unchanged"
;;         (assert-equal #:expect 100 #:got (account-balance account)))))
;;
;; Latin aliases are provided for those who prefer them: fabula/scaena for
;; scenario, dato for given, quando for when, tunc for then, etiam for and,
;; and sed for but.
;;
;; The When step is called `when-step' rather than `when' because Guile's core
;; `when' is a syntax keyword and importing a conflicting binding would break
;; `(when ...)' for every module that imports Veritas BDD.  The Latin `quando'
;; is unshadowed and reads naturally.
;;
;;; Code:

(define-module (veritas bdd)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:export (scenario feature
                     story
                     given
                     when-step
                     then
                     and-step
                     but-step
                     fabula
                     scaena
                     dato
                     quando
                     tunc
                     etiam
                     sed))

(define (labeled prefix name)
  "Prefix a step @var{name} with its keyword @var{prefix}."
  (string-append prefix " " name))

(define* (scenario name
                   #:key (skip? #f)
                   (debug? #f)
                   (shuffle? #f)
                   (concurrent? #f)
                   (before-all #f)
                   (after-all #f)
                   #:rest steps)
  "Define a Given/When/Then scenario named @var{name}.

A scenario is a suite whose steps execute in declaration order.  All step
arguments are forwarded to @code{suite}; steps are normally created with
@code{given}, @code{when-step}, @code{then}, @code{and-step} and
@code{but-step}.

@example
(scenario \"emptying the cart\"
  (given \"a cart with one item\"
    (assert-equal #:expect 1 #:got (cart-size cart)))
  (when-step \"the cart is emptied\"
    (empty-cart! cart))
  (then \"the cart is empty\"
    (assert-true (cart-empty? cart))))
@end example"
  (apply suite
         (append (list (labeled "Scenario:" name)
                       #:skip? skip?
                       #:debug? debug?
                       #:shuffle? shuffle?
                       #:concurrent? concurrent?
                       #:before-all before-all
                       #:after-all after-all) steps)))

(define* (given name
                #:rest cases)
  "Define a Given setup step named @var{name} holding assertion @var{cases}."
  (apply test
         (labeled "Given" name) cases))

(define* (when-step name
                    #:rest cases)
  "Define a When action step named @var{name} holding assertion @var{cases}.

Named @code{when-step} to avoid shadowing Guile's core @code{when} syntax."
  (apply test
         (labeled "When" name) cases))

(define* (then name
               #:rest cases)
  "Define a Then outcome step named @var{name} holding assertion @var{cases}."
  (apply test
         (labeled "Then" name) cases))

(define* (and-step name
                   #:rest cases)
  "Define an And continuation step named @var{name} holding assertion @var{cases}."
  (apply test
         (labeled "And" name) cases))

(define* (but-step name
                   #:rest cases)
  "Define a But continuation step named @var{name} holding assertion @var{cases}."
  (apply test
         (labeled "But" name) cases))

;; ====== Latin aliases ======

(define feature
  scenario)
(define story
  scenario)
(define fabula
  scenario)
(define scaena
  scenario)

(define dato
  given)
(define quando
  when-step)
(define tunc
  then)
(define etiam
  and-step)
(define sed
  but-step)
