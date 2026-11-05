;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit nesting)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (veritas table)
  #:use-module (veritas reporter)
  #:export (spec))

;; A suite may contain other suites as well as tests, to any depth, and a test
;; may appear at any level.  Both execution strategies (concurrent fibers and
;; sequential) must recurse through every level, report each nested test, and be
;; considered when deciding whether the run passed.

(define leaf-suite
  (suite "depth 4"
         (test "leaf test"
               (assert-equal #:expect 24 #:got (* 4 3 2 1)))))

(define depth-3-suite
  (suite "depth 3"
         leaf-suite
         (test "test at depth 3"
               (assert-true (> 3 2)))))

(define depth-2-suite
  (suite "depth 2"
         depth-3-suite
         (test "test at depth 2"
               (assert-length #:expect 2 #:got '(a b)))))

(define depth-1-suite
  (suite "depth 1"
         depth-2-suite
         (test "test at depth 1"
               (assert-one-of #:expect '(x y z) #:got 'y))))

;; Nesting must also work when concurrency is disabled at every level.
(define sequential-nesting
  (suite "sequential nesting"
         #:concurrent? #f
         (suite "sequential child"
                #:concurrent? #f
                (suite "sequential grandchild"
                       #:concurrent? #f
                       (test "deep sequential test"
                             (assert-equal #:expect 6 #:got (+ 1 2 3)))))))

;; Table-driven suites can nest too (test-each produces a suite of tests).
(define table-nesting
  (suite "table nesting"
         (test-each "inner table"
                    '((1 1) (2 4) (3 9))
                    (lambda (case)
                      (let ((n (list-ref case 0))
                            (square (list-ref case 1)))
                        (test (format #f "~a squared is ~a" n square)
                              (assert-equal #:expect square
                                            #:got (* n n))))))))

(define (spec)
  (veritas-run #:debug? #f
               depth-1-suite
               sequential-nesting
               table-nesting))
