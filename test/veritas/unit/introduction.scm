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

(define-module (veritas unit introduction)
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:use-module (ice-9 textual-ports)
  #:export (spec))

;; ====== Functions under test ======

(define (create-greeting person)
  (string-append "Hello, " person "!"))

(define (sum-list-elements lst)
  (apply + lst))

(define (classify-number n)
  (cond
    ((> n 0)
     'positive)
    ((< n 0)
     'negative)
    (else 'zero)))

;; ====== Suites ======

(define basics-suite
  (suite "Getting Started with Assertions"
         (test "Basic String and Predicate Tests"
               (assert-equal #:expect "Hello, Veritas!"
                             #:got (create-greeting "Veritas")
                             #:name "Greeting should be personalized")
               (assert-true (> 10 5)
                            #:name "10 should be greater than 5")
               (assert-false (string=? "guile" "GUILE")
                             #:name
                             "String comparison should be case-sensitive")
               (assert-true (not (equal? #f
                                         (string-contains (create-greeting
                                                           "World") "World")))
                            #:name "Greeting should contain the provided name"))))

(define logic-and-lists-suite
  (suite "Testing Logic and List Operations"
         (test "Number Classification Logic"
               (assert-equal #:expect 'positive
                             #:got (classify-number 99)
                             #:name "99 should be classified as positive")
               (assert-equal #:expect 'negative
                             #:got (classify-number -1)
                             #:name "-1 should be classified as negative")
               (assert-equal #:expect 'zero
                             #:got (classify-number 0)
                             #:name "0 should be classified as zero"))
         (test "List Summation"
               (assert-equal #:expect 15
                             #:got (sum-list-elements '(1 2 3 4 5))
                             #:name "Sum of elements 1 through 5 should be 15")
               (assert-equal #:expect 0
                             #:got (sum-list-elements '())
                             #:name "Sum of an empty list should be 0"))))

(define error-handling-suite
  (suite "Error and Exception Handling"
         (test "Summing a list with non-numeric elements"
               ;; 'apply' will throw a type error if args are not all numbers.
               (assert-error (lambda ()
                               (sum-list-elements '(1 2 "a" 4)))
                             #:name
                             "Should fail when list contains non-numbers"))
         (test "Valid summation should not throw an error"
               (assert-no-error (lambda ()
                                  (sum-list-elements '(10 20 30)))
                #:name "Should not fail with a valid list of numbers"))))

(define advanced-features-suite
  (suite "Advanced Veritas Features"
         #:before-all (lambda (vc)
                        (let* ((some-dir "./tmp-advanced-features-suite")
                               (some-file (format #f "~a/some-file.txt"
                                                  some-dir)))
                          (catch #t
                                 (lambda ()
                                   (mkdir some-dir))
                                 (lambda (key . args)
                                   (log-info vc
                                             (format #f "~a already existed"
                                                     some-dir))))
                          (with-output-to-file some-file
                            (lambda ()
                              (display "TRANSIRE RUBICONEM")
                              (display "\nALEA IACTA EST")))))
         #:after-all (lambda (vc)
                       (let* ((some-dir "./tmp-advanced-features-suite")
                              (some-file (format #f "~a/some-file.txt"
                                                 some-dir))
                              (file-lines (string-split (call-with-input-file some-file
                                                          get-string-all)
                                                        #\newline)))
                         (for-each (lambda (line)
                                     (log-info vc line)) file-lines)
                         (delete-file some-file)
                         (rmdir some-dir)))

         (test "A test that is currently skipped"
               #:skip? #t
               (assert-equal #:expect #f
                             #:got #t
                             #:name
                             "This test won't run because it is skipped"))
         (test "Addition Validation"
               #:before (lambda (vc)
                          (display "Initializing..."))
               (assert-equal #:got 4
                             #:expect (+ 2 2)))
         (test "A test with a complex computation block"
               #:before (lambda (vc)
                          (log-info vc "before test runs"))
               #:after (lambda (vc)
                         (log-info vc "after test runs"))
               (assert-equal #:expect 144
                             #:got (lazy
                                    (begin
                                      ;; Evaluated by Veritas when the
                                      ;; assertion runs.
                                      (let ((x 12))
                                        (* x x))))
                             #:name "Complex computation inside an assertion"))))

;; A test's #:before hook has to run before that test's own assertions, and
;; #:after after them, so state prepared in the hook is visible to the cases it
;; brackets.  This suite records the order the hooks actually fire in.  The
;; checks read ~hook-observations~ from inside a case lambda, which runs after
;; the hook; a #:got written directly would be computed when the body is
;; evaluated, which is also after the hook, since a body form is evaluated when
;; its test runs.
(define hook-observations
  '())

(define (note-hook! tag)
  (set! hook-observations
        (append hook-observations
                (list tag))))

(define lifecycle-hook-suite
  (suite "Lifecycle hooks bracket their assertions"
         #:concurrent? #f
         #:shuffle? #f
         #:before-all (lambda (vc)
                        (note-hook! 'suite-before-all))
         #:after-all (lambda (vc)
                       (note-hook! 'suite-after-all))
         (test "a #:before hook prepares state its own cases can see"
               #:shuffle? #f
               #:before (lambda (vc)
                          (note-hook! 'test-before))
               #:after (lambda (vc)
                         (note-hook! 'test-after))
               (lambda (vc)
                 ((assert-equal #:expect '(suite-before-all test-before)
                                #:got hook-observations
                                #:name "#:before has already run")
                  vc)))
         (test "the #:after of the test before it already ran"
               (lambda (vc)
                 ((assert-equal #:expect '(suite-before-all test-before
                                                            test-after)
                                #:got hook-observations)
                  vc)))))

(define (factorial n)
  (if (= n 0) 1
      (* n
         (factorial (- n 1)))))

(define simple-factorial-benchmark
  (benchmark #:name "Computation of the factorial of 10000"
             #:runs 2
             #:thunk (lambda ()
                       (factorial 10000))))

;; ====== Runner ======

(define (spec)
  (veritas-run #:debug? #f
               basics-suite
               logic-and-lists-suite
               error-handling-suite
               advanced-features-suite
               lifecycle-hook-suite
               simple-factorial-benchmark))
