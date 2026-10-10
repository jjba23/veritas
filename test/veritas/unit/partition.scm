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

(define-module (veritas unit partition)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:use-module (veritas runner)
  #:export (spec))

(define vc (list (cons 'reporter 'monotone-reporter)))

(define (truthy? thunk)
  "Return @code{#t} when @var{thunk} returns without raising."
  (catch #t (lambda () (thunk) #t) (lambda (key . args) #f)))

(define partition-suite
  (suite "partitioning a run"
         #:concurrent? #f
         #:shuffle? #f
         (test "a partition string is parsed, and refused when impossible"
               (lambda (vc)
                 ((assert-equal #:expect (cons 2 3)
                                #:got (parse-partition "2/3"))
                  vc)
                 ((assert-equal #:expect #f #:got (parse-partition #f)) vc)
                 ((assert-false (truthy? (lambda () (parse-partition "0/2")))) vc)
                 ((assert-false (truthy? (lambda () (parse-partition "4/3")))) vc)
                 ((assert-false (truthy? (lambda () (parse-partition "nineteen")))) vc)
                 ((assert-false (truthy? (lambda () (parse-partition "1/0")))) vc)))
         (test "the partitions of a list are disjoint and complete"
               (lambda (vc)
                 (let* ((files (map (lambda (i) (format #f "f~a" i))
                                    (iota 7)))
                        (p1 (partition-files files 1 3))
                        (p2 (partition-files files 2 3))
                        (p3 (partition-files files 3 3)))
                   ((assert-equal #:expect files
                                  #:got (sort (append p1 (append p2 p3))
                                              string<?))
                    vc)
                   ((assert-empty #:got (lset-intersection equal? p1 p2)) vc)
                   ((assert-empty #:got (lset-intersection equal? p2 p3)) vc)
                   ((assert-empty #:got (lset-intersection equal? p1 p3)) vc))))
         (test "the split is round-robin over the sorted list"
               (lambda (vc)
                 ((assert-equal #:expect '("a" "c")
                                #:got (partition-files '("a" "b" "c" "d") 1 2))
                  vc)
                 ((assert-equal #:expect '("b" "d")
                                #:got (partition-files '("a" "b" "c" "d") 2 2))
                  vc)
                 ((assert-equal #:expect '()
                                #:got (partition-files '() 1 2))
                  vc)))
         (test "one partition of one is the whole list"
               (lambda (vc)
                 ((assert-equal #:expect '("a" "b")
                                #:got (partition-files '("a" "b") 1 1))
                  vc)))))

(define (spec)
  (veritas-run #:debug? #f
               #:shuffle? #f
               partition-suite))
