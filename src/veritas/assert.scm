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

(define-module (veritas assert)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (veritas reporter)
  #:export (assert-equal assert-eq
                         assert-eqv
                         assert-true
                         assert-false
                         assert-error
                         assert-equal*
                         assert-no-error))

(define* (assert-equal* #:key expect got compare name)
  "Returns a function taking a @acronym{VC, veritas-context}
that returns an assertion that two values are equal based on a given comparison procedure.
This is the core assertion procedure that the other `assert-equal` variants
are built upon. It's used to compare an expected value with a computed value.

The @var{got} parameter can be a value or a special list to compute a value
dynamically. If @var{got} is a list starting with @code{compute},
@code{comp}, or @code{computare}, the second element of the list is
evaluated to get the actual value for comparison.

For example, @code{(assert-equal* #:got '(compute (+ 1 1)) ...)}
would evaluate @code{(+ 1 1)} to @code{2} before the comparison.

@table @var
@item expect
The expected value.
@item got
The value to compare against, or a list to be computed.
@item compare
The comparison procedure, which should be a function of two
 arguments, e.g., `equal?`, `eqv?`, or `eq?`.
@item name
A doc string to identify the assertion in the logs.
@end table

@example
(assert-equal* #:expect 3
               #:got (+ 1 1 1)
               #:compare equal?
               #:name \"Ensuring addition works\")
@result{} #<procedure 7f58ec5ce390 at veritas/assert.scm:69:2 (vc)>

;; Upon applying vc to the procedure we will compute the assertion:
`((assertion-successful . #t)
  (assertion-name \"Ensuring addition works\"))
@end example"
  (lambda (vc)
    (let* ((gott (if (and (list? got)
                          (not (null? got))
                          (or (equal? 'compute
                                      (car got))
                              (equal? 'comp
                                      (car got))
                              (equal? 'computare
                                      (car got))))
                     (begin
                       (eval (cadr got)
                             (interaction-environment))) got)))
      (when name
        (log-assert vc name))
      (if (compare expect gott)
          (begin
            (log-pass vc
                      (format #f "expect: ~a, got: ~a" expect gott))
            `((assertion-successful unquote #t)
              (assertion-name unquote name)))
          (begin
            (log-fail vc
                      (format #f "error, expect: ~a, got: ~a" expect gott))
            `((assertion-successful unquote #f)
              (assertion-name unquote name)))))))

(define* (assert-error thunk
                       #:key name)
  "Returns a function taking a @acronym{VC, veritas-context}
that returns an assertion that checks that a thunk
(a procedure with no arguments) raises an error when called.
  
This is a crucial function for testing error-handling logic. It attempts
to execute the provided @var{thunk}. If an error is caught during its execution,
the assertion passes. If the @code{thunk} completes without raising an error,
the assertion fails.

@table @var
@item thunk
A procedure of no arguments to be executed.
@item name
A string to identify the assertion in the logs.
@end table

@example
(assert-error (lambda ()
                      (sum-list-elements '(1 2 \"a\" 4)))
                    #:name
                    \"Should fail when list contains non-numbers\")
@result{} #<procedure 8f58ec5ce390 at veritas/assert.scm:69:2 (vc)>

;; Upon applying vc to the procedure we will compute the assertion:
`((assertion-successful . #t)
  (assertion-name \"Should fail when list contains non-numbers\"))
@end example"
  (lambda (vc)
    (when name
      (log-assert vc name))
    (catch #t
           (lambda ()
             (apply thunk
                    '())
             (log-fail vc "did not catch any error")
             `((assertion-successful unquote #f)
               (assertion-name unquote name)))
           (lambda (key . args)
             (log-pass vc
                       (format #f "success catching error: ~a: ~a" key args))
             `((assertion-successful unquote #t)
               (assertion-name unquote name))))))

(define* (assert-no-error thunk
                          #:key name)
  (lambda (vc)
    (when name
      (log-assert vc name))
    (catch #t
           (lambda ()
             (apply thunk
                    '())
             (log-pass vc "success running without errors")
             `((assertion-successful unquote #t)
               (assertion-name unquote name)))
           (lambda (key . args)
             (log-fail vc
                       (format #f "caught error: ~a: ~a" key args))
             `((assertion-successful unquote #f)
               (assertion-name unquote name))))))

(define* (assert-equal #:key expect got name)
  (assert-equal* #:expect expect
                 #:got got
                 #:compare equal?
                 #:name name))

(define* (assert-eqv #:key expect got name)
  (assert-equal* #:expect expect
                 #:got got
                 #:compare eqv?
                 #:name name))

(define* (assert-eq #:key expect got name)
  (assert-equal* #:expect expect
                 #:got got
                 #:compare eq?
                 #:name name))

(define* (assert-true x
                      #:key name)
  (assert-equal* #:got x
                 #:expect #t
                 #:compare eq?
                 #:name name))

(define* (assert-false x
                       #:key name)
  (assert-equal* #:got x
                 #:expect #f
                 #:compare eq?
                 #:name name))

