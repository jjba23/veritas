;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit generator more)
  #:use-module (srfi srfi-1)
  #:use-module (veritas veritas)
  #:use-module (veritas generator)
  #:export (spec))

(define (samples gen rand n)
  (map (lambda (_)
         (gen rand))
       (iota n)))

(define (all? pred xs)
  (or (null? xs)
      (and (pred (car xs))
           (all? pred
                 (cdr xs)))))

(define (member? x xs)
  (and (member x xs) #t))

(define primitives-suite
  (suite "primitive generators"
         (test "booleans are booleans and both values appear"
               (let* ((gen (generate-boolean))
                      (rand (seed->random-state 11))
                      (values (samples gen rand 50)))
                 (assert-true (all? boolean? values))
                 (assert-true (member? #t values))
                 (assert-true (member? #f values))))
         (test "naturals are inclusive and respect their bound"
               (let* ((gen (generate-natural #:max-bound 10))
                      (rand (seed->random-state 12))
                      (values (samples gen rand 200)))
                 (assert-true (all? (lambda (n)
                                      (and (integer? n)
                                           (>= n 0)
                                           (<= n 10))) values))))
         (test "reals stay inside their range"
               (let* ((gen (generate-real #:min -2.5
                                          #:max 2.5))
                      (rand (seed->random-state 13))
                      (values (samples gen rand 100)))
                 (assert-true (all? (lambda (x)
                                      (and (real? x)
                                           (>= x -2.5)
                                           (<= x 2.5))) values))))))

(define collections-suite
  (suite "collection generators"
         (test "vectors come from their element generator"
               (let* ((gen (generate-vector (generate-char)
                                            #:min-len 1
                                            #:max-len 4))
                      (rand (seed->random-state 14))
                      (values (samples gen rand 50)))
                 (assert-true (all? (lambda (v)
                                      (and (vector? v)
                                           (>= (vector-length v) 1)
                                           (<= (vector-length v) 4)
                                           (all? char?
                                                 (vector->list v)))) values))))
         (test "tuples hold one value per generator"
               (let* ((gen (generate-tuple (list (generate-boolean)
                                                 (generate-natural #:max-bound
                                                                   3))))
                      (rand (seed->random-state 15))
                      (values (samples gen rand 20)))
                 (assert-true (all? (lambda (pair)
                                      (and (= 2
                                              (length pair))
                                           (boolean? (car pair))
                                           (member (cadr pair)
                                                   '(0 1 2 3)))) values))))
         (test "one-of picks from its choices"
               (let* ((gen (generate-one-of '(red green blue)))
                      (rand (seed->random-state 16))
                      (values (samples gen rand 50)))
                 (assert-true (all? (lambda (choice)
                                      (member choice
                                              '(red green blue))) values))))
         (test "a constant generator returns its value"
               (let ((gen (generate-constant 'fixed)))
                 (assert-equal #:expect 'fixed
                               #:got (gen (seed->random-state 17)))))))

(define fake-suite
  (suite "fake data generators"
         (test "names have a first and a last part"
               (let* ((gen (generate-name))
                      (rand (seed->random-state 18))
                      (names (samples gen rand 20)))
                 (assert-true (all? string? names))
                 (assert-true (all? (lambda (name)
                                      (string-contains name " ")) names))))
         (test "first and last names are strings"
               (assert-true (all? string?
                                  (samples (generate-first-name)
                                           (seed->random-state 19) 20)))
               (assert-true (all? string?
                                  (samples (generate-last-name)
                                           (seed->random-state 20) 20))))
         (test "emails look like emails"
               (let* ((gen (generate-email))
                      (rand (seed->random-state 21))
                      (emails (samples gen rand 20)))
                 (assert-true (all? string? emails))
                 (assert-true (all? (lambda (email)
                                      (string-contains email "@example.com"))
                                    emails))
                 (assert-true (all? (lambda (email)
                                      (not (string-contains email " ")))
                                    emails))))
         (test "beers and cars are names, not empty strings"
               (let ((beer (generate-beer))
                     (car (generate-car))
                     (rand (seed->random-state 22)))
                 (assert-true (string? (beer rand)))
                 (assert-true (> (string-length (beer rand)) 0))
                 (assert-true (string? (car rand)))
                 (assert-true (> (string-length (car rand)) 0))))))

(define shrink-suite
  (suite "generator shrinkers"
         (test "integers shrink toward zero"
               (let ((candidates (generator-shrink (generate-natural) 73)))
                 (assert-true (member? 0 candidates))
                 (assert-true (member? 72 candidates))))
         (test "strings shrink to shorter strings"
               (let ((candidates (generator-shrink (generate-string) "abcd")))
                 (assert-true (member? "" candidates))
                 (assert-true (member? "ab" candidates))))
         (test "lists shrink to shorter lists"
               (let ((candidates (generator-shrink (generate-list (generate-integer))
                                                   '(1 2 3 4))))
                 (assert-true (member? '() candidates))
                 (assert-true (member? '(1 2) candidates))
                 (assert-true (member? '(3 4) candidates))))
         (test "a generator with no shrinker offers no candidates"
               (assert-equal #:expect '()
                             #:got (generator-shrink (generate-one-of '(a b))
                                                     'a)))))

(define (spec)
  (veritas-run #:debug? #f
               primitives-suite
               collections-suite
               fake-suite
               shrink-suite))
