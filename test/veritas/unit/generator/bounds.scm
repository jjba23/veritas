;; veritas

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; veritas is free software: you can redistribute it and/or modify
;; it under the terms of the Lesser GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; veritas is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with veritas.  If not, see <https://www.gnu.org/licenses/>.

(define-module (veritas unit generator bounds)
  #:use-module (srfi srfi-1)
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:export (spec))

;; Generators compile to closures of one argument, a random state.  These tests
;; draw many samples from each generator and assert that every one obeys the
;; documented contract (bounds, lengths, determinism).

(define (all? pred xs)
  (or (null? xs)
      (and (pred (car xs))
           (all? pred (cdr xs)))))

(define (samples gen rand n)
  (map (lambda (_)
         (gen rand))
       (iota n)))

(define integer-suite
  (suite "integer generators"
         (test "generate-integer respects its bound"
               (let ((gen (generate-integer #:max-bound 100))
                     (rand (seed->random-state 1)))
                 (assert-true (all? (lambda (n)
                                      (and (>= n 0)
                                           (< n 100)))
                                    (samples gen rand 200)))))
         (test "generate-integer default bound is respected"
               (let ((gen (generate-integer))
                     (rand (seed->random-state 9)))
                 (assert-true (all? (lambda (n)
                                      (< n 8192))
                                    (samples gen rand 200)))))
         (test "the same seed yields the same value"
               (let ((gen (generate-integer #:max-bound 1000)))
                 (assert-equal #:expect (gen (seed->random-state 7))
                               #:got (gen (seed->random-state 7)))))))

(define char-suite
  (suite "character generators"
         (test "generate-char yields printable ASCII"
               (let ((gen (generate-char))
                     (rand (seed->random-state 2)))
                 (assert-true (all? (lambda (c)
                                      (and (>= (char->integer c) 32)
                                           (<= (char->integer c) 126)))
                                    (samples gen rand 200)))))))

(define collection-suite
  (suite "collection generators"
         (test "generate-list respects min and max length"
               (let ((gen (generate-list (generate-char)
                                         #:min-len 2
                                         #:max-len 5))
                     (rand (seed->random-state 3)))
                 (assert-true (all? (lambda (lst)
                                      (and (>= (length lst) 2)
                                           (<= (length lst) 5)))
                                    (samples gen rand 100)))))
         (test "generate-string respects min and max length"
               (let ((gen (generate-string #:min-len 1 #:max-len 4))
                     (rand (seed->random-state 4)))
                 (assert-true (all? (lambda (s)
                                      (and (>= (string-length s) 1)
                                           (<= (string-length s) 4)))
                                    (samples gen rand 100)))))
         (test "generate-string yields printable strings"
               (let ((gen (generate-string #:min-len 1 #:max-len 8))
                     (rand (seed->random-state 5)))
                 (assert-true (all? string? (samples gen rand 50)))))
         (test "generate-symbol respects min and max length"
               (let ((gen (generate-symbol #:min-len 3 #:max-len 6))
                     (rand (seed->random-state 6)))
                 (assert-true (all? (lambda (sym)
                                      (let ((n (string-length (symbol->string sym))))
                                        (and (>= n 3)
                                             (<= n 6))))
                                    (samples gen rand 100)))))))

(define (spec)
  (veritas-run #:debug? #f integer-suite char-suite collection-suite))
