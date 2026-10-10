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
;; This module is a Scheme testing library that provides a comprehensive set
;; of assertion functions. At its core is the assert procedure,
;; a flexible and powerful tool for comparing an expected value against a result.
;;
;; Beyond simple value comparison, the module offers a set of tools for
;; testing program behavior related to error handling.
;;
;; These functions provide a complete assertion toolkit, enabling developers
;; to write robust tests that cover both expected outcomes and predictable
;; failures, thereby improving the reliability and quality of their Scheme code.
;;
;;; Code:

(define-module (veritas assert)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (veritas reporter)
  #:use-module (veritas prelude)
  #:export (assert-equal assert-eq
                         assert-eqv
                         assert-true
                         assert-false
                         assert-error
                         assert
                         assert-no-error
                         assert-not-eq
                         assert-not-equal
                         assert-not-eqv
                         is-approximate?
                         assert-approximate
                         assert-not-approximate
                         assert-contains
                         assert-not-contains
                         assert-matches
                         assert-not-matches
                         assert-raises
                         assert-empty
                         assert-not-empty
                         assert-length
                         assert-in-range
                         assert-one-of))

(define* (assert #:key expect got compare name)
  "Returns a function taking a @acronym{VC, veritas-context}
that returns an assertion that two values are equal based on a given comparison procedure.
This is the core assertion procedure that the other `assert-equal` variants
are built upon. It's used to compare an expected value with a computed value.

The @var{got} parameter can be a value or a special list to compute a value
dynamically. If @var{got} is a list starting with @code{compute},
@code{comp}, or @code{computare}, the second element of the list is
evaluated to get the actual value for comparison.

For example, @code{(assert #:got '(compute (+ 1 1)) ...)}
would evaluate @code{(+ 1 1)} to @code{2} before the comparison.

@table @var
@item expect
The expected value.
@item got
The value to compare against, or a list to be computed.
@item compare
The comparison procedure, the predicate, which should be a function of two
 arguments, e.g., `equal?`, `eqv?`, or `eq?`.
@item name
A doc string to identify the assertion in the logs.
@end table

@example
(assert #:expect 3
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
                      (format #f "assertion succesful, got: ~a" gott))
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
the assertion passes. If the @var{thunk} completes without raising an error,
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
  "Returns a function taking a @acronym{VC, veritas-context}
that returns an assertion that checks that a thunk
(a procedure with no arguments) does not raise an error when called.

This is a crucial function for testing error-handling logic. It attempts
to execute the provided @var{thunk}. If no error is caught during its execution,
the assertion passes. If the @var{thunk} raises an error,
the assertion fails.

@table @var
@item thunk
A procedure of no arguments to be executed.
@item name
A string to identify the assertion in the logs.
@end table

@example
(assert-no-error (lambda ()
                      (sum-list-elements '(1 2 \"a\" 4))))
@result{} #<procedure 8f58ec5ce390 at veritas/assert.scm:69:2 (vc)>

;; Upon applying vc to the procedure we will compute the assertion:
`((assertion-successful . #f))
@end example"
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
  "Returns a function taking a @acronym{VC, veritas-context}
that returns an assertion checking that @var{expect}
and @var{got} are structurally equal using @code{equal?} predicate.

This is the standard assertion for comparing lists, strings,
vectors, and other compound data structures.

It is a convenience wrapper around @code{assert}.

@table @var
@item expect
The expected value.
@item got
The value to compare against.
@item name
A string to identify the assertion in the logs.
@end table

@example
(assert-equal #:expect '(1 2 3)
              #:got '(1 2 3)
              #:name \"Lists should match\")
@end example"
  (assert #:expect expect
          #:got got
          #:compare equal?
          #:name name))

(define* (assert-eqv #:key expect got name)
  "Returns a function taking a @acronym{VC, veritas-context}
that returns an assertion checking that @var{expect}
and @var{got} are equivalent using @code{eqv?} predicate.

This is useful for comparing numbers, characters, and symbols
where value equivalence matters.

It is a convenience wrapper around @code{assert}.

@table @var
@item expect
The expected value.
@item got
The value to compare against.
@item name
A string to identify the assertion in the logs.
@end table

@example
(assert-eqv #:expect 42
            #:got (+ 40 2))
@end example"
  (assert #:expect expect
          #:got got
          #:compare eqv?
          #:name name))

(define* (assert-eq #:key expect got name)
  "Returns a function taking a @acronym{VC, veritas-context}
that returns an assertion checking that @var{expect}
and @var{got} are identical using @code{eq?}.

This is the strictest equality assertion and is typically used
for symbols, booleans, and identity-sensitive comparisons.

It is a convenience wrapper around @code{assert}.

@table @var
@item expect
The expected value.
@item got
The value to compare against.
@item name
A string to identify the assertion in the logs.
@end table

@example
(assert-eq #:expect 'foo
           #:got 'foo)
@end example"
  (assert #:expect expect
          #:got got
          #:compare eq?
          #:name name))

(define* (assert-not-eq #:key expect got name)
  "Returns a function taking a @acronym{VC, veritas-context}
that returns an assertion checking that @var{expect}
and @var{got} are not identical using @code{eq?}.

This assertion passes when the two values do not share identity.

It is the inverse of @code{assert-eq}.

@example
(assert-not-eq #:expect '(1 2)
               #:got '(1 2))
@end example"
  (assert #:expect expect
          #:got got
          #:compare (lambda (a b)
                      (not (eq? a b)))
          #:name name))

(define* (assert-not-eqv #:key expect got name)
  "Returns a function taking a @acronym{VC, veritas-context}
that returns an assertion checking that @var{expect}
and @var{got} are not equivalent using @code{eqv?}.

This assertion is useful when values should differ in meaning
or representation.

It is the inverse of @code{assert-eqv}.

@example
(assert-not-eqv #:expect 1
                #:got 2)
@end example"
  (assert #:expect expect
          #:got got
          #:compare (lambda (a b)
                      (not (eqv? a b)))
          #:name name))

(define* (assert-not-equal #:key expect got name)
  "Returns a function taking a @acronym{VC, veritas-context}
that returns an assertion checking that @var{expect}
and @var{got} are not structurally equal using @code{equal?}.

This assertion is useful when testing for inequality
of compound values.

It is the inverse of @code{assert-equal}.

@example
(assert-not-equal #:expect '(1 2 3)
                  #:got '(1 2 4))
@end example"
  (assert #:expect expect
          #:got got
          #:compare (lambda (a b)
                      (not (equal? a b)))
          #:name name))

(define* (assert-true x
                      #:key name)
  "Returns a function taking a @acronym{VC, veritas-context}
that returns an assertion checking that @var{x} is @code{#t}.

This is a convenience wrapper for boolean truth assertions.

@example
(assert-true (> 5 2)
             #:name \"5 is greater than 2\")
@end example"
  (assert #:got x
          #:expect #t
          #:compare eq?
          #:name name))

(define* (assert-false x
                       #:key name)
  "Returns a function taking a @acronym{VC, veritas-context}
that returns an assertion checking that @var{x} is @code{#f}.

This is a convenience wrapper for boolean falsehood assertions.

@example
(assert-false (< 5 2)
              #:name \"5 is not less than 2\")
@end example"
  (assert #:got x
          #:expect #f
          #:compare eq?
          #:name name))

(define* (is-approximate? a b
                          #:key epsilon)
  "Returns @code{#t} if the absolute difference between
@var{a} and @var{b} is smaller than @var{epsilon}.

This is useful for comparing floating-point values where
exact equality may not be reliable.

@table @var
@item a
The first numeric value.
@item b
The second numeric value.
@item epsilon
The maximum allowed difference.
@end table

@example
(is-approximate? 3.14159 3.14160
                 #:epsilon 0.0001)
@result{} #t
@end example"
  (< (abs (- a b)) epsilon))

(define* (assert-approximate #:key expect got name
                             (epsilon 1.0e-8))
  "Returns a function taking a @acronym{VC, veritas-context}
that returns an assertion checking that @var{expect}
and @var{got} are approximately equal.

Two values are considered approximately equal if their
difference is smaller than @var{epsilon}.

This is useful for floating-point computations.

@example
(assert-approximate #:expect 3.14
                    #:got 3.141
                    #:epsilon 0.01)
@end example"

  (assert #:expect expect
          #:got got
          #:name name
          #:compare (lambda (a b)
                      (is-approximate? a b
                                       #:epsilon epsilon))))

(define* (assert-not-approximate #:key expect got name
                                 (epsilon 1.0e-8))
  "Returns a function taking a @acronym{VC, veritas-context}
that returns an assertion checking that @var{expect}
and @var{got} are not approximately equal.

This assertion passes when the difference between the values
is greater than or equal to @var{epsilon}.

It is the inverse of @code{assert-approximate}.

@example
(assert-not-approximate #:expect 3.14
                        #:got 3.5
                        #:epsilon 0.01)
@end example"
  (assert #:expect expect
          #:got got
          #:name name
          #:compare (lambda (a b)
                      (not (is-approximate? a b
                                            #:epsilon epsilon)))))

;;; ====== Rich matchers ======
;;
;; The predicates below back a small family of higher-level assertions in the
;; spirit of RSpec matchers, Jest's expect and AssertJ.  They are intentionally
;; expressed through the core `assert' combinator so that reporting stays
;; uniform.

(define (length-of x)
  "Return the number of elements in list, vector or string @var{x}.

Returns @code{#f} for other types."
  (cond
    ((list? x)
     (length x))
    ((vector? x)
     (vector-length x))
    ((string? x)
     (string-length x))
    (else #f)))

(define (contains-value? container needle)
  "Return @code{#t} when @var{container} contains @var{needle}.

Strings are searched for substrings, lists and vectors for an element
@code{equal?} to @var{needle}, and hash tables for a key @code{equal?} to
@var{needle}."
  (cond
    ((string? container)
     (and (string? needle)
          (string-contains container needle)
          #t))
    ((list? container)
     (and (member needle container) #t))
    ((vector? container)
     (and (any (lambda (x) (equal? x needle))
               (vector->list container))
          #t))
    ((hash-table? container)
     (and (hash-get-handle container needle) #t))
    (else #f)))

(define (empty-value? x)
  "Return @code{#t} when @var{x} is an empty list, vector or string."
  (cond
    ((null? x)
     #t)
    ((vector? x)
     (zero? (vector-length x)))
    ((string? x)
     (string=? x ""))
    (else #f)))

(define* (assert-contains #:key expect got name)
  "Assert that the container @var{expect} contains @var{got}.

Works for strings (substring), lists and vectors (membership) and hash tables
(key membership).

@example
(assert-contains #:expect '(1 2 3) #:got 2)
(assert-contains #:expect \"hello world\" #:got \"world\")
@end example"
  (assert #:expect expect #:got got
          #:compare (lambda (container needle)
                      (contains-value? container needle))
          #:name name))

(define* (assert-not-contains #:key expect got name)
  "Assert that the container @var{expect} does not contain @var{got}.

Inverse of @code{assert-contains}."
  (assert #:expect expect #:got got
          #:compare (lambda (container needle)
                      (not (contains-value? container needle)))
          #:name name))

(define* (assert-matches #:key pattern got name)
  "Assert that string @var{got} matches the regular expression @var{pattern}.

@var{pattern} may be a compiled regexp or a string.

@example
(assert-matches #:pattern \"^[a-z]+@[a-z]+$\" #:got \"user@host\")
@end example"
  (let ((rx (if (regexp? pattern) pattern (make-regexp pattern))))
    (assert #:expect rx #:got got
            #:compare (lambda (regexp value)
                        (and (string? value)
                             (regexp-exec regexp value)
                             #t))
            #:name name)))

(define* (assert-not-matches #:key pattern got name)
  "Assert that string @var{got} does not match regular expression @var{pattern}.

Inverse of @code{assert-matches}."
  (let ((rx (if (regexp? pattern) pattern (make-regexp pattern))))
    (assert #:expect rx #:got got
            #:compare (lambda (regexp value)
                        (not (and (string? value)
                                  (regexp-exec regexp value))))
            #:name name)))

(define* (assert-raises thunk
                        #:key (on #t) predicate name)
  "Assert that calling @var{thunk} raises an error.

By default any error satisfies the assertion.  Pass @var{on} to require a
specific error key (for example @code{'wrong-type-arg}), or @var{predicate},
a procedure of @code{(key args)} returning @code{#t} for the expected error.

@example
(assert-raises (lambda () (/ 1 0)))
(assert-raises (lambda () (+ 1 'a)) #:on 'wrong-type-arg)
@end example"
  (lambda (vc)
    (when name
      (log-assert vc name))
    (let* ((raised?
            (catch #t
                   (lambda ()
                     (apply thunk
                            '())
                     #f)
                   (lambda (key . args)
                     (cond
                       ;; An explicit predicate is authoritative: when supplied
                       ;; it alone decides whether the error is expected.  It
                       ;; must be checked before the default `on' of #t,
                       ;; otherwise any error would satisfy the assertion and
                       ;; the predicate would never be able to reject one.
                       (predicate
                        (and (predicate key args)
                             #t))
                       ((eq? on #t)
                        #t)
                       (else (and (equal? key on)
                                  #t)))))))
      (if raised?
          (begin
            (log-pass vc "raised an error as expected")
            `((assertion-successful unquote #t)
              (assertion-name unquote name)))
          (begin
            (log-fail vc "did not raise an expected error")
            `((assertion-successful unquote #f)
              (assertion-name unquote name)))))))

(define* (assert-empty #:key got name)
  "Assert that @var{got} is an empty list, vector or string."
  (assert #:expect #t #:got got
          #:compare (lambda (_ value) (empty-value? value))
          #:name name))

(define* (assert-not-empty #:key got name)
  "Assert that @var{got} is a non-empty list, vector or string."
  (assert #:expect #t #:got got
          #:compare (lambda (_ value)
                      (and (length-of value)
                           (positive? (length-of value))))
          #:name name))

(define* (assert-length #:key expect got name)
  "Assert that @var{got} has @var{expect} elements (list, vector or string length)."
  (assert #:expect expect #:got got
          #:compare (lambda (n value)
                      (let ((len (length-of value)))
                        (and len
                             (= n len))))
          #:name name))

(define* (assert-in-range #:key min max got name)
  "Assert that @var{got} is within the inclusive range @var{min}--@var{max}."
  (assert #:expect (cons min max) #:got got
          #:compare (lambda (range value)
                      (and (>= value (car range))
                           (<= value (cdr range))))
          #:name name))

(define* (assert-one-of #:key expect got name)
  "Assert that @var{got} is @code{equal?} to one of the elements of list @var{expect}."
  (assert #:expect expect #:got got
          #:compare (lambda (choices value)
                      (and (member value choices)
                           #t))
          #:name name))
