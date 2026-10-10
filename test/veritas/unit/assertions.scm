;; veritas

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; veritas is free software: you can redistribute it and/or modify
;; it under the terms of the Lesser GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; veritas is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with veritas.  If not, see <https://www.gnu.org/licenses/>.

(define-module (veritas unit assertions)
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:export (spec))

;; This file exercises the core assert combinators both ways: every passing
;; case and its mirror failing case.  Assertions are evaluated directly against
;; a minimal context so their result alist can be inspected, which is how the
;; runner itself decides whether a run failed.

(define vc (list (cons 'reporter 'monotone-reporter)))

(define (succeeded? result)
  (cdr (assq 'assertion-successful result)))

(define (value assertion)
  (succeeded? (assertion vc)))

(define equality-suite
  (suite "equality, identity and value assertions"
         (test "assert-equal"
               (assert-true (value (assert-equal #:expect 1 #:got 1)))
               (assert-false (value (assert-equal #:expect 1 #:got 2))))
         (test "assert-eq"
               (assert-true (value (assert-eq #:expect 'foo #:got 'foo)))
               (assert-false (value (assert-eq #:expect (list 1) #:got (list 1)))))
         (test "assert-eqv"
               (assert-true (value (assert-eqv #:expect 42 #:got (+ 40 2))))
               (assert-false (value (assert-eqv #:expect 1 #:got 2))))
         (test "assert-not-equal"
               (assert-true (value (assert-not-equal #:expect '(1 2) #:got '(1 3))))
               (assert-false (value (assert-not-equal #:expect '(1 2) #:got '(1 2)))))
         (test "assert-not-eq"
               (assert-true (value (assert-not-eq #:expect (list 1) #:got (list 1))))
               (assert-false (value (assert-not-eq #:expect 'x #:got 'x))))
         (test "assert-not-eqv"
               (assert-true (value (assert-not-eqv #:expect 1 #:got 2)))
               (assert-false (value (assert-not-eqv #:expect 1 #:got 1))))))

(define boolean-suite
  (suite "boolean assertions"
         (test "assert-true"
               (assert-true (value (assert-true #t)))
               (assert-false (value (assert-true #f))))
         (test "assert-false"
               (assert-true (value (assert-false #f)))
               (assert-false (value (assert-false #t))))))

(define approximate-suite
  (suite "approximate assertions"
         (test "is-approximate?"
               (assert-true (is-approximate? 3.14159 3.14160 #:epsilon 0.0001))
               (assert-false (is-approximate? 1 2 #:epsilon 0.5)))
         (test "assert-approximate"
               (assert-true (value (assert-approximate #:expect 3.14
                                                       #:got 3.141
                                                       #:epsilon 0.01)))
               (assert-false (value (assert-approximate #:expect 3.14
                                                        #:got 3.5
                                                        #:epsilon 0.01))))
         (test "assert-not-approximate"
               (assert-true (value (assert-not-approximate #:expect 3.14
                                                           #:got 3.5
                                                           #:epsilon 0.01)))
               (assert-false (value (assert-not-approximate #:expect 3.14
                                                            #:got 3.141
                                                            #:epsilon 0.01))))))

(define delayed-suite
  (suite "delayed evaluation of got"
         (test "comp, compute and computare are evaluated at test time"
               (assert-true (value (assert-equal #:expect 144
                                                 #:got `(comp (begin
                                                               (let ((x 12))
                                                                 (* x x)))))))
               (assert-true (value (assert-equal #:expect 9
                                                 #:got `(compute (begin
                                                                   (* 3 3))))))
               (assert-true (value (assert-equal #:expect 5
                                                 #:got `(computare (begin
                                                                     (+ 2 3)))))))))

(define error-suite
  (suite "error assertions"
         (test "assert-error"
               (assert-true (value (assert-error (lambda ()
                                                   (/ 1 0)))))
               (assert-false (value (assert-error (lambda ()
                                                    42)))))
         (test "assert-no-error"
               (assert-true (value (assert-no-error (lambda ()
                                                      (+ 1 2)))))
               (assert-false (value (assert-no-error (lambda ()
                                                       (/ 1 0))))))
         (test "assert-raises accepts any error by default"
               (assert-true (value (assert-raises (lambda ()
                                                    (/ 1 0)))))
               (assert-false (value (assert-raises (lambda ()
                                                     42)))))
         (test "assert-raises honours an expected error key"
               (assert-true (value (assert-raises (lambda ()
                                                    (+ 1 'a))
                                                  #:on 'wrong-type-arg)))
               (assert-false (value (assert-raises (lambda ()
                                                     (+ 1 'a))
                                                   #:on 'misc-error))))
         (test "assert-raises honours a predicate"
               (assert-true (value (assert-raises (lambda ()
                                                    (error "boom"))
                                                  #:predicate (lambda (key args)
                                                                (equal? key
                                                                        'misc-error)))))
               (assert-false (value (assert-raises (lambda ()
                                                     (error "boom"))
                                                   #:predicate (lambda (key args)
                                                                 #f)))))))

;; Every Latin alias is bound to the same procedure object as its English
;; name, and must therefore pass and fail on exactly the same inputs.  The
;; cases below mirror the English suites above, one alias at a time.
(define latin-alias-suite
  (suite "latin aliases"
         (test "proba is the core assert combinator"
               (assert-true (value (proba #:expect 3
                                          #:got (+ 1 1 1)
                                          #:compare equal?)))
               (assert-false (value (proba #:expect 3
                                           #:got 4
                                           #:compare equal?))))
         (test "equality aliases"
               (assert-true (value (aequalis #:expect '(1 2) #:got '(1 2))))
               (assert-false (value (aequalis #:expect '(1 2) #:got '(1 3))))
               (assert-true (value (idem #:expect 'foo #:got 'foo)))
               (assert-false (value (idem #:expect (list 1) #:got (list 1))))
               (assert-true (value (aequivalens #:expect 42 #:got (+ 40 2))))
               (assert-false (value (aequivalens #:expect 1 #:got 2)))
               (assert-true (value (non-aequalis #:expect '(1 2) #:got '(1 3))))
               (assert-false (value (non-aequalis #:expect '(1 2) #:got '(1 2))))
               (assert-true (value (non-idem #:expect (list 1) #:got (list 1))))
               (assert-false (value (non-idem #:expect 'x #:got 'x)))
               (assert-true (value (non-aequivalens #:expect 1 #:got 2)))
               (assert-false (value (non-aequivalens #:expect 1 #:got 1))))
         (test "boolean aliases"
               (assert-true (value (verus #t)))
               (assert-false (value (verus #f)))
               (assert-true (value (falsus #f)))
               (assert-false (value (falsus #t))))
         (test "approximate aliases"
               (assert-true (propinquus? 3.14159 3.14160 #:epsilon 0.0001))
               (assert-false (propinquus? 1 2 #:epsilon 0.5))
               (assert-true (value (propinquus #:expect 3.14
                                               #:got 3.141
                                               #:epsilon 0.01)))
               (assert-false (value (propinquus #:expect 3.14
                                                #:got 3.5
                                                #:epsilon 0.01)))
               (assert-true (value (non-propinquus #:expect 3.14
                                                   #:got 3.5
                                                   #:epsilon 0.01)))
               (assert-false (value (non-propinquus #:expect 3.14
                                                    #:got 3.141
                                                    #:epsilon 0.01))))
         (test "error aliases"
               (assert-true (value (errat (lambda ()
                                            (/ 1 0)))))
               (assert-false (value (errat (lambda ()
                                             42))))
               (assert-true (value (non-errat (lambda ()
                                                (+ 1 2)))))
               (assert-false (value (non-errat (lambda ()
                                                 (/ 1 0)))))
               (assert-true (value (suscitat (lambda ()
                                               (+ 1 'a))
                                             #:on 'wrong-type-arg)))
               (assert-false (value (suscitat (lambda ()
                                                42)))))
         (test "matcher aliases"
               (assert-true (value (continet #:expect '(1 2 3) #:got 2)))
               (assert-false (value (continet #:expect '(1 2) #:got 9)))
               (assert-true (value (non-continet #:expect '(1 2) #:got 9)))
               (assert-false (value (non-continet #:expect '(1 2) #:got 2)))
               (assert-true (value (congruit #:pattern "^h.*o$" #:got "hello")))
               (assert-false (value (congruit #:pattern "^x" #:got "hello")))
               (assert-true (value (non-congruit #:pattern "^x" #:got "hello")))
               (assert-false (value (non-congruit #:pattern "e" #:got "hello")))
               (assert-true (value (inanis #:got '())))
               (assert-false (value (inanis #:got '(1))))
               (assert-true (value (non-inanis #:got '(1))))
               (assert-false (value (non-inanis #:got '())))
               (assert-true (value (longitudo #:expect 3 #:got '(1 2 3))))
               (assert-false (value (longitudo #:expect 4 #:got '(1 2 3))))
               (assert-true (value (intra #:min 1 #:max 10 #:got 5)))
               (assert-false (value (intra #:min 1 #:max 10 #:got 11)))
               (assert-true (value (unus-ex #:expect '(red green) #:got 'green)))
               (assert-false (value (unus-ex #:expect '(red green) #:got 'blue))))))

(define (spec)
  (veritas-run #:debug? #f
               equality-suite
               boolean-suite
               approximate-suite
               delayed-suite
               error-suite
               latin-alias-suite))
