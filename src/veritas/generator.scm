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
;; Generator combinators for the property-based testing subsystem.  Every
;; generator is a closure taking a random state, so the calling convention is
;; always: (lambda (rand) ...).  Sizes are picked inside the generator, which
;; saves threading a size argument through the pipeline by hand.

;;; Code:

(define-module (veritas generator)
  #:declarative? #t
  #:export (generate-integer generate-char generate-list generate-symbol
                             generate-string))

(define* (generate-integer #:key (max-bound 8192))
  "Return a generator of integers between 1 and @var{max-bound} (inclusive).

Call the generator with a random state:

@example
(define gen (generate-integer #:max-bound 100))
(gen (seed->random-state 42))
@end example"
  (lambda (rand)
    (let* ((random-size (+ 1
                           (random 100 rand)))
           (bound (min (max random-size 1) max-bound)))
      (random bound rand))))

(define (generate-char)
  "Return a generator of printable ASCII characters, 32 to 126."
  (lambda (rand)
    (integer->char (+ 32
                      (random 95 rand)))))

(define* (generate-symbol #:key (min-len 1)
                          (max-len #f))
  "Return a generator of lowercase symbols, @var{min-len} to @var{max-len} long.

Symbols stay unescaped because the characters are always @code{a-z}."
  (let* ((generate-symbol-char (lambda (rand)
                                 (integer->char (+ 97
                                                   (random 26 rand)))))
         (list-gen (generate-list generate-symbol-char
                                  #:min-len min-len
                                  #:max-len max-len)))
    (lambda (rand)
      (string->symbol (list->string (list-gen rand))))))

(define* (generate-string #:key (min-len 0)
                          (max-len #f))
  "Return a generator of printable-ASCII strings.

Strings are @var{min-len} to @var{max-len} characters long."
  (let ((list-gen (generate-list (generate-char)
                                 #:min-len min-len
                                 #:max-len max-len)))
    (lambda (rand)
      (list->string (list-gen rand)))))

(define* (generate-list elem-generator
                        #:key (min-len 0)
                        (max-len #f))
  "Return a generator of lists whose elements come from @var{elem-generator}.

Lists are @var{min-len} to @var{max-len} long."
  (lambda (rand)
    (let* ((random-size (+ 1
                           (random 100 rand)))
           (current-max (if max-len
                            (min random-size max-len) random-size))
           (safe-max (max (+ min-len 1) current-max))
           (delta (- safe-max min-len))
           (target-length (+ min-len
                             (if (> delta 0)
                                 (random delta rand) 0))))
      (let loop
        ((count 0)
         (acc '()))
        (if (= count target-length) acc
            (loop (+ count 1)
                  (cons (elem-generator rand) acc)))))))
