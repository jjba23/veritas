;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit structural-match)
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:export (spec))

(define vc
  (list (cons 'reporter
              'monotone-reporter)))

(define (succeeded? result)
  (cdr (assq 'assertion-successful result)))

(define (contains? text needle)
  (and (string-contains text needle) #t))

(define match-suite
  (suite "structural matching"
         (test "a rest pattern matches a list"
               (assert-true (succeeded? ((assert-match #:pattern (a b ...)
                                                       #:got '(1 2 3))
                                         vc))))
         (test "a fixed pattern rejects a longer list"
               (assert-false (succeeded? ((assert-match #:pattern (a b)
                                                        #:got '(1 2 3))
                                          vc))))
         (test "a quoted pattern is read as the pattern itself"
               (assert-true (succeeded? ((assert-match #:pattern '(a b)
                                                       #:got '(1 2))
                                         vc))))
         (test "predicates and literals work in a pattern"
               (assert-true (succeeded? ((assert-match #:pattern (a (? even?))
                                                       #:got '(10 4))
                                         vc)))
               (assert-false (succeeded? ((assert-match #:pattern (a (? even?))
                                                        #:got '(10 5))
                                          vc))))
         (test "name and timeout are accepted"
               (assert-true (succeeded? ((assert-match #:pattern (x)
                                                       #:got '(1)
                                                       #:name
                                                       "a one element list"
                                                       #:timeout 1)
                                         vc))))
         (test "a failure says what did not match"
               (let ((output (with-output-to-string (lambda ()
                                                      ((assert-match #:pattern
                                                                     (a b)
                                                                     #:got '(1
                                                                             2
                                                                             3))
                                                       vc)))))
                 (assert-true (contains? output "does not match"))))))

(define (spec)
  (veritas-run #:debug? #f match-suite))
