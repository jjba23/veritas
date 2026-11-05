;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit flexible-bodies)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (veritas veritas)
  #:use-module (veritas bdd)
  #:use-module (veritas model)
  #:use-module (veritas prelude)
  #:use-module (veritas reporter)
  #:export (spec))

;; A body is read for what it *builds*, not for the value of its last
;; expression.  These tests pin that down: a `let' body contributes every
;; assertion it builds, a body whose last form is not an assertion still runs
;; the assertions before it, a list of assertions is a list of cases, and the
;; same holds for the steps of a scenario and the tests of a suite.
;;
;; Every check below runs a fixture and reads the outcomes it reported, so a
;; body that quietly skips an assertion fails here instead of passing.

(define vc
  (list (cons 'reporter 'monotone-reporter)))

(define (run-quietly thunk)
  "Call @var{thunk} swallowing the output its reporter would print."
  (with-output-to-port (open-output-string) thunk))

(define (outcomes unit)
  "Run @var{unit} and return the outcome of every assertion it reported, in order."
  (run-quietly (lambda ()
                 (collect-assertion-successful (unit vc)))))

(define (assertion-count unit)
  "Return how many assertions @var{unit} reported."
  (length (outcomes unit)))

(define (ok? assertion)
  "Run @var{assertion} and return whether it passed."
  (cdr (assq 'assertion-successful (assertion vc))))

;; ====== Fixtures: bodies that used to contribute one case, or none ======

(define let-body-test
  (test "a let body of three assertions"
    (let ((n 2))
      (assert-equal #:expect 2 #:got n)
      (assert-true (even? n))
      (assert-false (odd? n)))))

;; The assertion in the middle fails, and it is the failing one that used to be
;; dropped: the body only handed the runner the value of its last expression.
(define failing-let-body-test
  (test "a let body with a failing assertion in the middle"
    (let ((n 2))
      (assert-equal #:expect 2 #:got n)
      (assert-equal #:expect 3 #:got n)
      (assert-true #t))))

(define trailing-statement-test
  (test "a let body whose last form is not an assertion"
    (let ((n 2))
      (assert-equal #:expect 2 #:got n)
      (display ""))))

(define when-body-test
  (test "a when body"
    (when #t
      (assert-true #t)
      (assert-false #f))))

(define cond-body-test
  (test "a cond body"
    (cond
      ((> 2 1) (assert-true (> 2 1))
       (assert-equal #:expect 2 #:got (+ 1 1)))
      (else (assert-true #f)))))

(define mapped-list-test
  (test "a list of assertions built by map"
    (map (lambda (n)
           (assert-equal #:expect n #:got (* n 1)))
         '(1 2 3))))

(define (cart-checks size full?)
  "Return the checks a cart of @var{size} items deserves."
  (list (assert-equal #:expect 2 #:got size)
        (assert-true full?)))

(define helper-built-test
  (test "assertions built by a helper procedure"
    (cart-checks 2 #t)))

(define consumed-assertion-test
  (test "an assertion the body runs itself is not a case"
    (assert-true (ok? (assert-equal #:expect 1 #:got 1)))
    (assert-false (ok? (assert-equal #:expect 1 #:got 2)))))

(define no-assertion-test
  (test "a body that builds no assertions"
    (display "")))

;; ====== Fixtures: suites and scenarios ======

(define let-around-tests-suite
  (suite "a let around two tests"
         #:concurrent? #f
         #:shuffle? #f
         (let ((n 3))
           (test "the first child sees the binding"
                 (assert-equal #:expect 3 #:got n))
           (test "the second child sees the binding too"
                 (assert-true (> n 2))))))

(define let-around-steps-scenario
  (scenario "a let around the steps"
    (let ((account 100))
      (given "an account with 100"
        (assert-equal #:expect 100 #:got account))
      (when-step "40 is withdrawn"
        (assert-equal #:expect 60 #:got (- account 40)))
      (then "the account is still open"
        (assert-true (> account 0))))))

;; ====== Fixtures: when a body is evaluated, and what brackets it ======

(define body-ran? #f)

(define deferred-body-test
  (test "a body is evaluated when the test runs"
    (let ((n 1))
      (set! body-ran? #t)
      (assert-equal #:expect 1 #:got n))))

(define hook-state 'pending)

(define hook-ordered-test
  (test "hooks still bracket the body"
    #:before (lambda (vc)
               (set! hook-state 'ready))
    #:after (lambda (vc)
              (set! hook-state 'done))
    (let ((n 4))
      ;; A #:got written directly is computed after #:before has run.
      (assert-eq #:expect 'ready #:got hook-state)
      (assert-equal #:expect 4 #:got n))))

(define timed-let-body-test
  (test "a deadline covers a let body"
    #:timeout 5
    (let ((n 4))
      (assert-equal #:expect 4 #:got n))))

;; ====== Fixtures: the procedures behind the macros ======

(define applied-test
  (apply %test "assertions handed to %test"
         (map (lambda (n)
                (assert-equal #:expect n #:got (* n 1)))
              '(1 2 3))))

(define applied-suite
  (apply %suite "tasks handed to %suite"
         #:concurrent? #f
         #:shuffle? #f
         (list (test "a child built by apply"
                     (assert-true #t)))))

;; ====== The checks ======

(define flexible-body-suite
  (suite "flexible bodies"
         #:concurrent? #f
         #:shuffle? #f
         (test "a let body contributes every assertion it builds"
               (assert-equal #:expect '(#t #t #t)
                             #:got (outcomes let-body-test))
               (assert-length #:expect 3 #:got (outcomes let-body-test)))
         (test "a failing assertion inside a let body is reported"
               (assert-equal #:expect '(#t #f #t)
                             #:got (outcomes failing-let-body-test)))
         (test "a statement after the assertions does not hide them"
               (assert-equal #:expect '(#t)
                             #:got (outcomes trailing-statement-test)))
         (test "when and cond bodies are read the same way"
               (assert-equal #:expect '(#t #t)
                             #:got (outcomes when-body-test))
               (assert-equal #:expect '(#t #t)
                             #:got (outcomes cond-body-test)))
         (test "a list of assertions is a list of cases"
               (assert-equal #:expect '(#t #t #t)
                             #:got (outcomes mapped-list-test))
               (assert-equal #:expect '(#t #t)
                             #:got (outcomes helper-built-test)))
         (test "an assertion used as an input is not run twice"
               (assert-equal #:expect '(#t #t)
                             #:got (outcomes consumed-assertion-test)))
         (test "a body that builds no assertions is a failure"
               (assert-equal #:expect '(#f)
                             #:got (outcomes no-assertion-test))
               (assert-length #:expect 1 #:got (outcomes no-assertion-test)))
         (test "a let around several tests contributes every test"
               (assert-equal #:expect '(#t #t)
                             #:got (outcomes let-around-tests-suite))
               (assert-length #:expect 2
                              #:got (outcomes let-around-tests-suite)))
         (test "a let around the steps contributes every step"
               (assert-equal #:expect '(#t #t #t)
                             #:got (outcomes let-around-steps-scenario))
               (assert-length #:expect 3
                              #:got (outcomes let-around-steps-scenario)))
         (test "the body waits for the run"
               (assert-false body-ran?)
               (assert-equal #:expect '(#t)
                             #:got (outcomes deferred-body-test))
               (assert-true body-ran?))
         (test "hooks bracket the body and are visible to it"
               (assert-equal #:expect '(#t #t)
                             #:got (outcomes hook-ordered-test))
               (assert-eq #:expect 'done #:got hook-state))
         (test "a deadline still covers a let body"
               (assert-equal #:expect '(#t)
                             #:got (outcomes timed-let-body-test)))
         (test "the procedures behind the macros take built cases"
               (assert-equal #:expect '(#t #t #t)
                             #:got (outcomes applied-test))
               (assert-equal #:expect '(#t)
                             #:got (outcomes applied-suite)))))

(define (spec)
  (veritas-run #:debug? #f
               #:shuffle? #f
               #:concurrent? #f
               flexible-body-suite))
