;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit diff)
  #:use-module (veritas veritas)
  #:use-module (veritas diff)
  #:use-module (veritas reporter)
  #:export (spec))

(define vc
  (list (cons 'reporter
              'monotone-reporter)))

(define (contains? text needle)
  (and (string-contains text needle) #t))

(define diff-suite
  (suite "value diffs"
         (test "lists name the first differing index and both values"
               (let ((text (render-diff '(1 2 3)
                                        '(1 2 9))))
                 (assert-true (contains? text "index 2"))
                 (assert-true (contains? text "expected 3"))
                 (assert-true (contains? text "got 9"))))
         (test "strings name the first differing character"
               (let ((text (render-diff "abce" "abxd")))
                 (assert-true (contains? text "index 2"))
                 (assert-true (contains? text "#\\c"))
                 (assert-true (contains? text "#\\x"))))
         (test "equal values have no diff"
               (assert-false (render-diff '(1 2 3)
                                          '(1 2 3)))
               (assert-false (render-diff "same" "same")))
         (test "values that are not lists, vectors or strings have no diff"
               (assert-false (render-diff 1 2))
               (assert-false (render-diff 'a
                                          'b)))
         (test "a list that is a prefix says which one is longer"
               (let ((text (render-diff '(1 2)
                                        '(1 2 3))))
                 (assert-true (contains? text "first 2 elements"))
                 (assert-true (contains? text "continues with (3)"))))
         (test "assert-equal prints the diff after the values"
               (let ((output (with-output-to-string (lambda ()
                                                      ((assert-equal #:expect '
                                                                     (1 2 3)
                                                                     #:got '(1
                                                                             2
                                                                             9))
                                                       vc)))))
                 (assert-true (contains? output
                               "error, expect: (1 2 3), got: (1 2 9)"))
                 (assert-true (contains? output "index 2"))))))

(define (spec)
  (veritas-run #:debug? #f diff-suite))
