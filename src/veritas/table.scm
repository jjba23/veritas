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
;; Table-driven tests, in the spirit of pytest's parametrize, JUnit's
;; parameterized tests and Go's table tests.  One test definition is expanded
;; over a list of cases and each case is reported as its own test, so a failure
;; points at the exact input.

;;; Code:

(define-module (veritas table)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (veritas veritas)
  #:export (test-each suite-each cases->table))

(define (cases->table cases key)
  "Index @var{cases} by @var{key}, giving an alist of key to case.

Handy for tables with named columns."
  (map (lambda (case )
         (cons (key case) case)) cases))

(define* (test-each group-name
                    cases
                    case->test
                    #:key (shuffle? #t)
                    (concurrent? #f))
  "Expand @var{cases} into one test per case, under a suite named @var{group-name}.

@var{case->test} takes one case and returns a @code{test}:

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
  "Like @code{test-each}, but @var{case->suite} returns a @code{suite}.

Use it when one case needs several tests rather than one."
  (apply suite
         group-name
         #:shuffle? shuffle?
         #:concurrent? concurrent?
         (map case->suite cases)))
