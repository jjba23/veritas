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
;; Test doubles: spies, stubs and temporary bindings, in the spirit of a mocking
;; library.  Scheme needs less of one than most languages do - a module's
;; procedures can be passed in, so a fake is often just a lambda - but recording
;; what was called, and checking it after the fact, is worth having.
;;
;;   (define send (make-spy real-send))       ; records every call
;;   (define clock (make-stub #:returns 0))   ; ignores its arguments
;;
;;   (test "the receipt is sent once"
;;         (lambda (vc)
;;           (send "ada" "thanks")            ; the call under test
;;           ((assert-called-with send '("ada" "thanks")) vc)
;;           ((assert-call-count send #:expect 1) vc)))
;;
;; The assertions read the spy when they run, not when they are built, which is
;; why they are written as a case lambda: a spy's calls are test-time state.
;;
;; A stub can also stand in for a module binding without editing the module, as
;; long as the call site reads that binding when it runs - see
;; `with-temporary-binding' for exactly when that is.
;;
;;; Code:

(define-module (veritas mock)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9)
  #:use-module (veritas assert)
  #:export (make-spy
                    make-stub
                    spy?
                    spy-calls
                    spy-results
                    spy-call-count
                    spy-called-with?
                    assert-called
                    assert-called-with
                    assert-call-count
                    with-temporary-binding))

(define-record-type spy-state
  (make-spy-state name procedure calls results) spy-state?
  (name spy-state-name)
  (procedure spy-state-procedure)
  (calls spy-state-calls set-spy-state-calls!)
  (results spy-state-results set-spy-state-results!))

;; The recording lives beside the procedure rather than inside it, so a spy can
;; be passed around as the plain procedure it replaces.  Keys are compared by
;; identity, which is what a procedure is.
(define spy-registry (make-hash-table))

(define (spy? value)
  "Return @code{#t} when @var{value} was made by @code{make-spy} or @code{make-stub}."
  (and (procedure? value)
       (hash-ref spy-registry value)
       #t))

(define (spy-state-of spy)
  "Return the recording behind @var{spy}, or raise when it is not a spy."
  (or (hash-ref spy-registry spy)
      (error "not a spy or stub made by veritas mock:" spy)))

(define (spy-label spy)
  "Return the name @var{spy} was made with, or a generic label."
  (or (spy-state-name (spy-state-of spy)) "the spy"))

(define (spy-calls spy)
  "Return the argument lists @var{spy} was called with, oldest first."
  (spy-state-calls (spy-state-of spy)))

(define (spy-results spy)
  "Return what @var{spy} returned, oldest first, one entry per call."
  (spy-state-results (spy-state-of spy)))

(define (spy-call-count spy)
  "Return how many times @var{spy} was called."
  (length (spy-calls spy)))

(define (spy-called-with? spy args)
  "Return @code{#t} when @var{spy} was called with exactly @var{args}."
  (and (member args (spy-calls spy)) #t))

(define* (make-spy proc #:key name)
  "Return a procedure that calls @var{proc} and records every call.

The recording is read back with @code{spy-calls}, @code{spy-results} and
@code{spy-call-count}.  @var{name} is only used in the messages of the
@code{assert-*} checks below.

@example
(define double (make-spy (lambda (x) (* x 2))))
(double 3)
(spy-calls double)
@result{} ((3))
@end example"
  (let ((state (make-spy-state name proc '() '())))
    (let ((spy (lambda args
                 (set-spy-state-calls! state
                                       (append (spy-state-calls state)
                                               (list args)))
                 (let ((value (apply proc args)))
                   (set-spy-state-results! state
                                           (append (spy-state-results state)
                                                   (list value)))
                   value))))
      (hash-set! spy-registry spy state)
      spy)))

(define* (make-stub #:key (returns #f) name)
  "Return a stand-in procedure that ignores its arguments and returns @var{returns}.

A stub is a spy whose procedure ignores its arguments, so
@code{spy-calls}, @code{spy-call-count} and the @code{assert-*} checks work on
it unchanged."
  (make-spy (lambda args returns)
            #:name name))

(define* (assert-called spy #:key name)
  "Assert that @var{spy} was called at least once.

Checked when the assertion runs, so it is written as a case lambda:
@code{(lambda (vc) ((assert-called spy) vc))}."
  (lambda (vc)
    ((assert #:expect #t
             #:got (> (spy-call-count spy) 0)
             #:compare eq?
             #:name (or name (format #f "~a was called" (spy-label spy))))
     vc)))

(define* (assert-called-with spy args #:key name)
  "Assert that @var{spy} was called with exactly @var{args}.

Checked when the assertion runs, so it is written as a case lambda:
@code{(lambda (vc) ((assert-called-with spy '(1 2)) vc))}."
  (lambda (vc)
    ((assert #:expect args
             #:got (spy-calls spy)
             #:compare (lambda (wanted calls)
                         (and (member wanted calls) #t))
             #:name (or name
                        (format #f "~a was called with ~a"
                                (spy-label spy) args)))
     vc)))

(define* (assert-call-count spy #:key expect name)
  "Assert that @var{spy} was called exactly @var{expect} times.

Checked when the assertion runs, so it is written as a case lambda:
@code{(lambda (vc) ((assert-call-count spy #:expect 2) vc))}."
  (lambda (vc)
    ((assert #:expect expect
             #:got (spy-call-count spy)
             #:compare =
             #:name (or name
                        (format #f "~a was called ~a time(s)"
                                (spy-label spy) expect)))
     vc)))

(define (with-temporary-binding module var value thunk)
  "Call @var{thunk} with @var{module}'s @var{var} set to @var{value}, and restore it.

The previous value comes back even when @var{thunk} raises, so substituting a
dependency is safe to try.

Guile resolves a call to a known procedure while it compiles, so a compiled call
site does *not* see the change: a call written in a module the compiler can see
through keeps calling the original procedure.  Measured on Guile 3.0, swapping a
binding is observed by @code{module-ref}, by code passed to @code{eval}, and by
call sites in a module declared @code{#:declarative? #f}; it is not observed by a
call compiled in a declarative module, which is the default.

That leaves the ordinary Scheme answer, which needs no substitution at all: pass
the dependency in, or hand the caller a @code{make-spy} recording.  Use this
when the code under test reaches the binding at run time.

@example
(with-temporary-binding (resolve-module '(my app)) 'now
                        (lambda () 0)
                        (lambda ()
                          (assert-equal #:expect 0
                                        #:got (eval '((@@ (my app) now))
                                                    (resolve-module '(my app))))))
@end example"
  (let ((original (module-ref module var)))
    (dynamic-wind (lambda () (module-set! module var value))
                  thunk
                  (lambda () (module-set! module var original)))))
