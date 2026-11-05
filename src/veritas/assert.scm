;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Assertions for Veritas.  The core procedure is `assert', which compares a
;; value against an expected one using whatever predicate you give it.  The
;; assert-* procedures below are thin wrappers around it, together with a few
;; helpers for checking error handling.
;;
;; Every assertion also has a Latin alias, following the Latin naming already
;; used by the BDD helpers (dato, quando, tunc, ...): (proba ...) is (assert
;; ...), aequalis is assert-equal, verus is assert-true, continet is
;; assert-contains, and so on.  An alias is the exact same procedure object as
;; its English name, so keyword arguments, semantics and reporting are
;; identical.  The complete mapping is documented in the README and repeated in
;; the alias block at the end of this file.
;;
;;; Code:

(define-module (veritas assert)
  #:use-module (srfi srfi-9)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (veritas reporter)
  #:use-module (veritas prelude)
  #:export (lazy
         lazy-value?
         assert-equal assert-eq
                         assert-eqv
                         assert-true
                         assert-false
                         assert-error
                         assert
                         make-assertion
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
                         assert-one-of
                         ;; Latin aliases, see the block at the end of this file
                         proba
                         aequalis
                         idem
                         aequivalens
                         non-aequalis
                         non-idem
                         non-aequivalens
                         verus
                         falsus
                         propinquus?
                         propinquus
                         non-propinquus
                         errat
                         non-errat
                         suscitat
                         continet
                         non-continet
                         congruit
                         non-congruit
                         inanis
                         non-inanis
                         longitudo
                         intra
                         unus-ex))

(define (make-assertion assertion)
  "Return @var{assertion}, a procedure of one validation client, as a case to run.

The returned procedure is tagged so the runner can tell an assertion from a
task, and it records having run, which is how an assertion the body itself
consumes - with @code{value} or @code{ok?}, say - stays an input to another case
instead of becoming a case of its own.

It is also registered with the collector of the body being evaluated, if there
is one.  That registration is what keeps a body form that builds several
assertions, such as the body of a @code{let}, from contributing only the value
of its last expression.  Building an assertion outside any body - at load time,
or inside an assertion that runs later - simply registers nothing."
  (letrec ((thunk (lambda (vc)
                    (set-procedure-property! thunk
                                             'veritas-assertion-ran
                                             #t)
                    (assertion vc))))
    (set-procedure-property! thunk 'veritas-assertion #t)
    (collect-in! thunk)))

(define* (assertion-with-timeout timeout name assertion)
  "Wrap @var{assertion}, a procedure of one validation client, with a @var{timeout}.

With a positive @var{timeout} the assertion runs under @code{call-with-timeout}:
if it overruns, its fiber is aborted and a failure named after @var{name} is
reported instead of hanging the run.  @code{#f}, the default, runs
@var{assertion} as it is.

The result is always a case - tagged, and recorded with the body being
evaluated - whether or not a deadline was given.

Only work done while the assertion runs is covered, so keep expensive setup in a
@code{lazy} value or inside the thunk passed to @code{assert-error}.  A fiber
suspended in a blocking wait cannot be unwound; there the run stops waiting and
the fiber is dropped with its scheduler."
  (make-assertion (if (not timeout) assertion
                      (lambda (vc)
                        (call-with-timeout timeout
                                           (lambda ()
                                             (assertion vc))
                                           #:on-timeout (lambda ()
                                                          (let ((message (format #f
                                                                          "~a timed out after ~a seconds"
                                                                          (if name name
                                                                              "assertion")
                                                                          timeout)))
                                                            (log-fail vc message)
                                                            `((assertion-successful unquote #f)
                                                              (assertion-name unquote message)))))))))

;; ====== Deferred values ======
;;
;; An assertion usually receives values.  When the value is expensive, or when a
;; deadline should be able to interrupt it, it is wrapped in `lazy': the
;; expression is then evaluated when the assertion runs, in the scope where it
;; was written.  The older convention of a quoted (comp ...) list evaluated the
;; form with `eval' in the interaction environment, which lost that scope.

(define-record-type <lazy-value>
  (make-lazy-value thunk) lazy-value?
  (thunk lazy-value-thunk))

(define-syntax lazy
  ;; Wrap @var{expr} so an assertion evaluates it when the assertion runs.
  ;; A @code{lazy} value is evaluated afresh by each assertion that receives
  ;; it, so nothing is cached, and it runs inside the assertion's
  ;; @code{#:timeout}, which is what lets a deadline interrupt it.
  (syntax-rules ()
    ((_ expr) (make-lazy-value (lambda () expr)))))

(define (force-lazy value)
  "Return @var{value}, or what its thunk returns when it is a @code{lazy} value."
  (if (lazy-value? value)
      ((lazy-value-thunk value))
      value))

(define* (assert #:key expect
                 got
                 compare
                 name
                 timeout)
  "Return an assertion comparing @var{got} with @var{expect} using @var{compare},
a two-argument predicate such as @code{equal?}.

The @code{assert-*} wrappers below are built on this one.  @var{got} and
@var{expect} are usually plain values, but either may be wrapped in
@code{lazy}: @code{(assert-equal #:expect 2 #:got (lazy (+ 1 1)))} evaluates
the expression when the assertion runs, inside its @var{timeout}, which is what
makes an expensive comparison something a deadline can catch.  A @code{lazy}
value keeps the lexical scope of the code that wrote it and is evaluated once
per assertion that receives it.

@var{name} labels the assertion in the report.  @var{timeout} caps it in
seconds; @code{#f}, the default, means no deadline.

@example
(assert #:expect 3
        #:got (+ 1 1 1)
        #:compare equal?
        #:name \"Ensuring addition works\")
@end example"
  (assertion-with-timeout timeout name
                          (lambda (vc)
                            (let* ((expected (force-lazy expect))
                                   (gott (force-lazy got)))
                              (when name
                                (log-assert vc name))
                              (if (compare expected gott)
                                  (begin
                                    (log-pass vc
                                              (format #f "got: ~a" gott))
                                    `((assertion-successful unquote #t)
                                      (assertion-name unquote name)))
                                  (begin
                                    (log-fail vc
                                              (format #f
                                               "error, expect: ~a, got: ~a"
                                               expected gott))
                                    `((assertion-successful unquote #f)
                                      (assertion-name unquote name))))))))

(define* (assert-error thunk
                       #:key name timeout)
  "Return an assertion that @var{thunk} raises.

It fails if @var{thunk} returns normally.  @var{name} labels it in the report
and @var{timeout} caps it in seconds, as in @code{assert}.

@example
(assert-error (lambda () (sum-list-elements '(1 2 \"a\" 4)))
              #:name \"Should fail when list contains non-numbers\")
@end example"
  (assertion-with-timeout timeout name
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
                                               (format #f
                                                "success catching error: ~a: ~a"
                                                key args))
                                     `((assertion-successful unquote #t)
                                       (assertion-name unquote name)))))))

(define* (assert-no-error thunk
                          #:key name timeout)
  "Return an assertion that @var{thunk} does not raise.

It fails if @var{thunk} raises.  @var{name} labels it in the report and
@var{timeout} caps it in seconds, as in @code{assert}."
  (assertion-with-timeout timeout name
                          (lambda (vc)
                            (when name
                              (log-assert vc name))
                            (catch #t
                                   (lambda ()
                                     (apply thunk
                                            '())
                                     (log-pass vc
                                      "success running without errors")
                                     `((assertion-successful unquote #t)
                                       (assertion-name unquote name)))
                                   (lambda (key . args)
                                     (log-fail vc
                                               (format #f
                                                       "caught error: ~a: ~a"
                                                       key args))
                                     `((assertion-successful unquote #f)
                                       (assertion-name unquote name)))))))

(define* (assert-equal #:key expect got name timeout)
  "Assert that @var{got} is @code{equal?} to @var{expect}.

The usual choice for lists, strings, vectors and other compound values."
  (assert #:expect expect
          #:got got
          #:compare equal?
          #:name name
          #:timeout timeout))

(define* (assert-eqv #:key expect got name timeout)
  "Assert that @var{got} is @code{eqv?} to @var{expect}.

Good for numbers, characters and symbols, where value equivalence is what
matters."
  (assert #:expect expect
          #:got got
          #:compare eqv?
          #:name name
          #:timeout timeout))

(define* (assert-eq #:key expect got name timeout)
  "Assert that @var{got} is @code{eq?} to @var{expect}.

The strictest of the equality assertions; use it for symbols, booleans and
anything compared by identity."
  (assert #:expect expect
          #:got got
          #:compare eq?
          #:name name
          #:timeout timeout))

(define* (assert-not-eq #:key expect got name timeout)
  "Assert that @var{got} is not @code{eq?} to @var{expect}.

Inverse of @code{assert-eq}."
  (assert #:expect expect
          #:got got
          #:compare (lambda (a b)
                      (not (eq? a b)))
          #:name name
          #:timeout timeout))

(define* (assert-not-eqv #:key expect got name timeout)
  "Assert that @var{got} is not @code{eqv?} to @var{expect}.

Inverse of @code{assert-eqv}."
  (assert #:expect expect
          #:got got
          #:compare (lambda (a b)
                      (not (eqv? a b)))
          #:name name
          #:timeout timeout))

(define* (assert-not-equal #:key expect got name timeout)
  "Assert that @var{got} is not @code{equal?} to @var{expect}.

Inverse of @code{assert-equal}."
  (assert #:expect expect
          #:got got
          #:compare (lambda (a b)
                      (not (equal? a b)))
          #:name name
          #:timeout timeout))

(define* (assert-true x
                      #:key name timeout)
  "Assert that @var{x} is @code{#t}."
  (assert #:got x
          #:expect #t
          #:compare eq?
          #:name name
          #:timeout timeout))

(define* (assert-false x
                       #:key name timeout)
  "Assert that @var{x} is @code{#f}."
  (assert #:got x
          #:expect #f
          #:compare eq?
          #:name name
          #:timeout timeout))

(define* (is-approximate? a b
                          #:key epsilon)
  "Return @code{#t} when @var{a} and @var{b} differ by less than @var{epsilon}.

Useful for floating point, where exact equality rarely holds."
  (< (abs (- a b)) epsilon))

(define* (assert-approximate #:key expect
                             got
                             name
                             timeout
                             (epsilon 1.0e-8))
  "Assert that @var{got} is within @var{epsilon} of @var{expect}.

The default @var{epsilon} is @code{1.0e-8}."

  (assert #:expect expect
          #:got got
          #:name name
          #:compare (lambda (a b)
                      (is-approximate? a b
                                       #:epsilon epsilon))))

(define* (assert-not-approximate #:key expect
                                 got
                                 name
                                 timeout
                                 (epsilon 1.0e-8))
  "Assert that @var{got} differs from @var{expect} by at least @var{epsilon}.

Inverse of @code{assert-approximate}."
  (assert #:expect expect
          #:got got
          #:name name
          #:compare (lambda (a b)
                      (not (is-approximate? a b
                                            #:epsilon epsilon)))))

;; ====== Rich matchers ======
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
          (string-contains container needle) #t))
    ((list? container)
     (and (member needle container) #t))
    ((vector? container)
     (and (any (lambda (x)
                 (equal? x needle))
               (vector->list container)) #t))
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

(define* (assert-contains #:key expect got name timeout)
  "Assert that the container @var{expect} contains @var{got}.

Works for strings (substring), lists and vectors (membership) and hash tables
(key membership).

@example
(assert-contains #:expect '(1 2 3) #:got 2)
(assert-contains #:expect \"hello world\" #:got \"world\")
@end example"
  (assert #:expect expect
          #:got got
          #:compare (lambda (container needle)
                      (contains-value? container needle))
          #:name name
          #:timeout timeout))

(define* (assert-not-contains #:key expect got name timeout)
  "Assert that the container @var{expect} does not contain @var{got}.

Inverse of @code{assert-contains}."
  (assert #:expect expect
          #:got got
          #:compare (lambda (container needle)
                      (not (contains-value? container needle)))
          #:name name
          #:timeout timeout))

(define* (assert-matches #:key pattern got name timeout)
  "Assert that the string @var{got} matches @var{pattern}.

@var{pattern} is a string or a compiled regexp.

@example
(assert-matches #:pattern \"^veri.*s$\" #:got \"veritas\")
@end example"
  (let ((rx (if (regexp? pattern) pattern
                (make-regexp pattern))))
    (assert #:expect rx
            #:got got
            #:compare (lambda (regexp value)
                        (and (string? value)
                             (regexp-exec regexp value) #t))
            #:name name
            #:timeout timeout)))

(define* (assert-not-matches #:key pattern got name timeout)
  "Assert that string @var{got} does not match regular expression @var{pattern}.

Inverse of @code{assert-matches}."
  (let ((rx (if (regexp? pattern) pattern
                (make-regexp pattern))))
    (assert #:expect rx
            #:got got
            #:compare (lambda (regexp value)
                        (not (and (string? value)
                                  (regexp-exec regexp value))))
            #:name name
            #:timeout timeout)))

(define* (assert-raises thunk
                        #:key (on #t)
                        predicate
                        name
                        timeout)
  "Assert that calling @var{thunk} raises an error.

By default any error satisfies the assertion.  Pass @var{on} to require a
specific error key (for example @code{'wrong-type-arg}), or @var{predicate},
a procedure of @code{(key args)} returning @code{#t} for the expected error.

@example
(assert-raises (lambda () (/ 1 0)))
(assert-raises (lambda () (+ 1 'a)) #:on 'wrong-type-arg)
@end example"
  (assertion-with-timeout timeout name
                          (lambda (vc)
                            (when name
                              (log-assert vc name))
                            (let* ((raised? (catch #t
                                                   (lambda ()
                                                     (apply thunk
                                                            '()) #f)
                                                   (lambda (key . args)
                                                     (cond
                                                       ;; An explicit predicate is authoritative: when supplied
                                                       ;; it alone decides whether the error is expected.  It
                                                       ;; must be checked before the default `on' of #t,
                                                       ;; otherwise any error would satisfy the assertion and
                                                       ;; the predicate would never be able to reject one.
                                                       (predicate (and (predicate
                                                                        key
                                                                        args)
                                                                       #t))
                                                       ((eq? on #t)
                                                        #t)
                                                       (else (and (equal? key
                                                                          on)
                                                                  #t)))))))
                              (if raised?
                                  (begin
                                    (log-pass vc "raised an error as expected")
                                    `((assertion-successful unquote #t)
                                      (assertion-name unquote name)))
                                  (begin
                                    (log-fail vc
                                     "did not raise an expected error")
                                    `((assertion-successful unquote #f)
                                      (assertion-name unquote name))))))))

(define* (assert-empty #:key got name timeout)
  "Assert that @var{got} is an empty list, vector or string."
  (assert #:expect #t
          #:got got
          #:compare (lambda (_ value)
                      (empty-value? value))
          #:name name
          #:timeout timeout))

(define* (assert-not-empty #:key got name timeout)
  "Assert that @var{got} is a non-empty list, vector or string."
  (assert #:expect #t
          #:got got
          #:compare (lambda (_ value)
                      (and (length-of value)
                           (positive? (length-of value))))
          #:name name
          #:timeout timeout))

(define* (assert-length #:key expect got name timeout)
  "Assert that @var{got} has @var{expect} elements (list, vector or string length)."
  (assert #:expect expect
          #:got got
          #:compare (lambda (n value)
                      (let ((len (length-of value)))
                        (and len
                             (= n len))))
          #:name name
          #:timeout timeout))

(define* (assert-in-range #:key min
                          max
                          got
                          name
                          timeout)
  "Assert that @var{got} is within the inclusive range @var{min}--@var{max}."
  (assert #:expect (cons min max)
          #:got got
          #:compare (lambda (range value)
                      (and (>= value
                               (car range))
                           (<= value
                               (cdr range))))
          #:name name
          #:timeout timeout))

(define* (assert-one-of #:key expect got name timeout)
  "Assert that @var{got} is @code{equal?} to one of the elements of list @var{expect}."
  (assert #:expect expect
          #:got got
          #:compare (lambda (choices value)
                      (and (member value choices) #t))
          #:name name
          #:timeout timeout))

;; ====== Latin aliases ======
;;
;; Every assertion has a Latin alias, in the same spirit as the BDD step
;; aliases (dato, quando, tunc, etiam, sed).  Each alias is bound to the very
;; same procedure object as its English name, so `(eq? aequalis assert-equal)'
;; holds and behavior, keyword arguments and reporting are identical.
;;
;;   proba            assert
;;   aequalis         assert-equal
;;   idem             assert-eq
;;   aequivalens      assert-eqv
;;   non-aequalis     assert-not-equal
;;   non-idem         assert-not-eq
;;   non-aequivalens  assert-not-eqv
;;   verus            assert-true
;;   falsus           assert-false
;;   propinquus?      is-approximate?
;;   propinquus       assert-approximate
;;   non-propinquus   assert-not-approximate
;;   errat            assert-error
;;   non-errat        assert-no-error
;;   suscitat         assert-raises
;;   continet         assert-contains
;;   non-continet     assert-not-contains
;;   congruit         assert-matches
;;   non-congruit     assert-not-matches
;;   inanis           assert-empty
;;   non-inanis       assert-not-empty
;;   longitudo        assert-length
;;   intra            assert-in-range
;;   unus-ex          assert-one-of

(define proba
  assert)
(define aequalis
  assert-equal)
(define idem
  assert-eq)
(define aequivalens
  assert-eqv)
(define non-aequalis
  assert-not-equal)
(define non-idem
  assert-not-eq)
(define non-aequivalens
  assert-not-eqv)
(define verus
  assert-true)
(define falsus
  assert-false)
(define propinquus?
  is-approximate?)
(define propinquus
  assert-approximate)
(define non-propinquus
  assert-not-approximate)
(define errat
  assert-error)
(define non-errat
  assert-no-error)
(define suscitat
  assert-raises)
(define continet
  assert-contains)
(define non-continet
  assert-not-contains)
(define congruit
  assert-matches)
(define non-congruit
  assert-not-matches)
(define inanis
  assert-empty)
(define non-inanis
  assert-not-empty)
(define longitudo
  assert-length)
(define intra
  assert-in-range)
(define unus-ex
  assert-one-of)
