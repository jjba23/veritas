;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit table)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (veritas table)
  #:export (spec))

(define addition-suite
  (test-each "integer addition"
             '((0 0 0) (1 2 3) (10 20 30) (-1 1 0) (100 -100 0))
             (lambda (case)
               (let ((a (list-ref case 0))
                     (b (list-ref case 1))
                     (expected (list-ref case 2)))
                 (test (format #f "~a + ~a = ~a" a b expected)
                       (assert-equal #:expect expected #:got (+ a b)))))))

(define strings-suite
  (test-each "string reversal is involutive"
             '("" "a" "ab" "racecar" "veritas")
             (lambda (s)
               (let ((reversed (list->string (reverse (string->list s)))))
                 (test (format #f "reversing ~s twice is identity" s)
                       (assert-equal #:expect s
                                     #:got (list->string
                                            (reverse (string->list reversed)))))))))

(define nested-suite
  (suite-each "collection emptiness"
              '(("" #t) ("x" #f))
              (lambda (case)
                (let ((s (list-ref case 0))
                      (expected (list-ref case 1)))
                  (suite (format #f "string ~s" s)
                         (test "emptiness matches"
                               (assert-equal #:expect expected
                                             #:got (string=? s ""))))))))

;; cases->table re-shapes a case list into an alist indexed by a key, so a
;; table can name its columns.
(define columns-suite
  (suite "cases->table names the columns"
         (test "indexes every case by the key"
               (assert-equal #:expect '((:a :a 1) (:b :b 2))
                             #:got (cases->table '((:a 1) (:b 2)) car)))))

;; suite-each expands each case into its own nested suite, two levels deep.
(define grouped-suite
  (suite-each "grouped squares"
              '((1 1) (2 4) (3 9))
              (lambda (case)
                (let ((n (list-ref case 0))
                      (square (list-ref case 1)))
                  (suite (format #f "n = ~a" n)
                         (test "square is correct"
                               (assert-equal #:expect square
                                             #:got (* n n))))))))

(define (spec)
  (veritas-run #:debug? #f
               addition-suite
               strings-suite
               nested-suite
               columns-suite
               grouped-suite))
