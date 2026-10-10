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
;; Table-driven (parameterized) tests, in the spirit of pytest's
;; `parametrize', JUnit's @ParameterizedTest and Go's idiomatic table tests.
;; A single test definition is expanded over a list of cases and each case is
;; reported as its own test, so failures point at the exact input.

;;; Code:

(define-module (veritas table)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (veritas veritas)
  #:export (test-each suite-each cases->table))

(define (cases->table cases key)
  "Index @var{cases} by @var{key}, returning an alist of key to case list.

Useful to build readable tables with named columns.

@example
(cases->table '((:a 1) (:b 2)) car)
@result{} ((:a (:a 1)) (:b (:b 2)))
@end example"
  (map (lambda (case )
         (cons (key case) case)) cases))

(define* (test-each group-name
                    cases
                    case->test
                    #:key (shuffle? #t)
                    (concurrent? #f))
  "Expand @var{cases} into a suite of tests named @var{group-name}.

@var{case->test} is a procedure that receives one case and must return a
@code{test}.  Cases run in order by default, so table tests read top to bottom.

@example
(test-each \"integer addition\"
           '((0 0 0) (1 2 3) (10 20 30))
           (lambda (case)
             (let ((a (list-ref case 0))
                   (b (list-ref case 1))
                   (expected (list-ref case 2)))
               (test (format #f \"~a + ~a = ~a\" a b expected)
                 (assert-equal #:expect expected #:got (+ a b))))))
@end example"
  (apply suite
         group-name
         #:shuffle? shuffle?
         #:concurrent? concurrent?
         (map case->test cases)))

(define* (suite-each group-name
                     cases
                     case->suite
                     #:key (shuffle? #t)
                     (concurrent? #f))
  "Expand @var{cases} into nested suites under @var{group-name}.

Like @code{test-each} but @var{case->suite} returns a @code{suite} instead of
a single test, which is handy for grouping several assertions per case."
  (apply suite
         group-name
         #:shuffle? shuffle?
         #:concurrent? concurrent?
         (map case->suite cases)))
