;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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
;; Every step of that scenario runs: a scenario body is read for the steps it
;; builds, not for the value of its last expression, so a `let' around the steps
;; is the natural way to share state and it does not hide the steps from the
;; runner.  The same holds inside a step: a `let' body contributes every
;; assertion it builds.
;;
;; Latin aliases are provided for those who prefer them: fabula/scaena for
;; scenario, dato for given, quando for when, tunc for then, etiam for and,
;; and sed for but.
;;
;; The three steps also have PascalCase aliases -- Given, When and Then -- for
;; readers who expect the keywords to look like the Cucumber ones.
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
                     Given
                     When
                     Then
                     fabula
                     scaena
                     dato
                     quando
                     tunc
                     etiam
                     sed
                     %scenario
                     %step
                     %given
                     %when-step
                     %then
                     %and-step
                     %but-step))

;; ====== The procedures behind the macros ======
;;
;; `scenario' and the steps are macros, so a body is evaluated when the scenario
;; runs and every step and assertion in it is seen.  What they expand to is the
;; procedures below, which are exported for the code that builds steps at run
;; time and for anyone who needs to apply one.

(define* (%scenario name
                    #:key (skip? #f)
                    (debug? #f)
                    (shuffle? #f)
                    (concurrent? #f)
                    (timeout #f)
                    (tags '())
                    (before-all #f)
                    (after-all #f)
                    #:rest steps)
  "Define a Given/When/Then scenario named @var{name}; the procedure behind the macro.

A scenario is a suite, so its steps run in declaration order.  Keywords are
forwarded to @code{suite}, and steps come from @code{given}, @code{when-step},
@code{then}, @code{and-step} and @code{but-step}.

@var{timeout}, when given, caps the whole scenario in seconds, exactly like
@code{suite}; a step can carry its own @code{#:timeout} as well, since steps
are ordinary tests.

@example
(scenario \"emptying the cart\"
  (given \"a cart with one item\"
    (assert-equal #:expect 1 #:got (cart-size cart)))
  (when-step \"the cart is emptied\"
    (empty-cart! cart))
  (then \"the cart is empty\"
    (assert-true (cart-empty? cart))))
@end example"
  (apply %suite
         (append (list (string-append "Scenario: " name)
                       #:skip? skip?
                       #:debug? debug?
                       #:shuffle? shuffle?
                       #:concurrent? concurrent?
                       #:timeout timeout
                       #:tags tags
                       #:before-all before-all
                       #:after-all after-all) steps)))

(define* (%step label name #:rest cases)
  "Define a step named @var{name}, labeled with @var{label}; the procedure behind the step macros.

A step is an ordinary test, so it takes the same keywords and cases."
  (apply %test (string-append label " " name) cases))

(define* (%given name #:rest cases)
  "Define a Given (setup) step named @var{name}."
  (apply %step "Given" name cases))

(define* (%when-step name #:rest cases)
  "Define a When (action) step named @var{name}.

Called @code{when-step} rather than @code{when} so it does not shadow Guile's
@code{when} syntax."
  (apply %step "When" name cases))

(define* (%then name #:rest cases)
  "Define a Then (outcome) step named @var{name}."
  (apply %step "Then" name cases))

(define* (%and-step name #:rest cases)
  "Define an And (continuation) step named @var{name}."
  (apply %step "And" name cases))

(define* (%but-step name #:rest cases)
  "Define a But (continuation) step named @var{name}."
  (apply %step "But" name cases))

;; ====== The macros ======
;;
;; Each one is the same transformation: keep the keyword arguments where they
;; are, wrap every body form in a thunk, and hand the lot to the procedure
;; above.  `deferred-call' spells that transformation out once.

(define (deferred-call construct stx)
  "Expand the scenario or step call @var{stx} through the procedure @var{construct}.

The keyword arguments are passed through and each body form becomes a thunk, so
a body is evaluated when the scenario runs - under the collector that records
the steps and assertions it builds."
  (syntax-case stx ()
    ((_ name args ...)
     (call-with-values (lambda ()
                         (split-keyword-arguments #'(args ...)))
       (lambda (keywords body)
         (with-syntax (((keyword ...) keywords)
                       ((thunk ...) (defer-body-forms body)))
           #`(#,construct name keyword ... thunk ...)))))))

(define-syntax scenario
  (lambda (stx)
    (deferred-call #'%scenario stx)))

(define-syntax given
  (lambda (stx)
    (deferred-call #'%given stx)))

(define-syntax when-step
  (lambda (stx)
    (deferred-call #'%when-step stx)))

(define-syntax then
  (lambda (stx)
    (deferred-call #'%then stx)))

(define-syntax and-step
  (lambda (stx)
    (deferred-call #'%and-step stx)))

(define-syntax but-step
  (lambda (stx)
    (deferred-call #'%but-step stx)))

;; ====== PascalCase aliases ======

(define-syntax Given
  (lambda (stx)
    (deferred-call #'%given stx)))

(define-syntax When
  (lambda (stx)
    (deferred-call #'%when-step stx)))

(define-syntax Then
  (lambda (stx)
    (deferred-call #'%then stx)))

;; ====== Latin aliases ======

(define-syntax feature
  (lambda (stx)
    (deferred-call #'%scenario stx)))

(define-syntax story
  (lambda (stx)
    (deferred-call #'%scenario stx)))

(define-syntax fabula
  (lambda (stx)
    (deferred-call #'%scenario stx)))

(define-syntax scaena
  (lambda (stx)
    (deferred-call #'%scenario stx)))

(define-syntax dato
  (lambda (stx)
    (deferred-call #'%given stx)))

(define-syntax quando
  (lambda (stx)
    (deferred-call #'%when-step stx)))

(define-syntax tunc
  (lambda (stx)
    (deferred-call #'%then stx)))

(define-syntax etiam
  (lambda (stx)
    (deferred-call #'%and-step stx)))

(define-syntax sed
  (lambda (stx)
    (deferred-call #'%but-step stx)))
