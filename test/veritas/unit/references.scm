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

(define-module (veritas unit references)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (veritas veritas)
  #:use-module (veritas model)
  #:use-module (veritas prelude)
  #:use-module (veritas reporter)
  #:export (spec))

;; A test should not have to say where it lives: the `test' and `suite' macros
;; know, and a report should be able to name the file without any help from the
;; person writing the test.

;; The runner may load this file by a path relative to the test directory, so
;; the tests below check the end of it rather than the whole thing.
(define this-file "references.scm")

(define vc
  (list (cons 'reporter 'monotone-reporter)))

(define (quietly thunk)
  (with-output-to-port (open-output-string) thunk))

(define (run-task task)
  (quietly (lambda () (task vc))))

(define (test-of task)
  "Run @var{task} and return the @code{pre-test-result} it produced."
  (run-task task))

(define references-suite
  (suite "source references"
         #:shuffle? #f
         (test "a test result names the file the test was declared in"
               (lambda (vc)
                 (let ((result (test-of (test "declared here"
                                              (assert-true #t)))))
                   ((assert-contains #:expect (pre-test-result-file result)
                                     #:got this-file)
                    vc))))

         (test "a suite passes its own declaration site down to its tests"
               (lambda (vc)
                 (let* ((result (run-task (suite "declared here"
                                                 (test "child"
                                                       (assert-true #t)))))
                        (children (suite-result-test-results result)))
                   ((assert-contains
                     #:expect (pre-test-result-file
                               (test-result-results (car children)))
                     #:got this-file)
                    vc))))

         (test "the JUnit document writes the file of a test"
               (lambda (vc)
                 (let* ((result (run-task (test "declared here"
                                                (assert-true #t))))
                        (xml (assoc-ref (junit-report-data (list result)
                                                           #:seconds 1)
                                        'xml)))
                   ((assert-contains #:expect xml #:got "file=\"")
                    vc)
                   ((assert-contains #:expect xml #:got this-file)
                    vc))))

         (test "a location string is reported for the running test"
               (lambda (vc)
                 (let ((location (veritas-current-location)))
                   ((assert-contains #:expect location #:got this-file) vc))))))

(define (spec)
  (veritas-run #:debug? #f
               #:shuffle? #f
               #:concurrent? #f
               references-suite))
