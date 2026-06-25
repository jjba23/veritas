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
;; This module implements data generator combinators for the Veritas
;; property-based testing sub-system. All generators compile down into a
;; predictable closure contract matching the simplified signature: (lambda (rand) ...)
;;
;; This simplified architecture automatically selects a random size internally,
;; removing the need to track sizing down through the generator pipeline manually.
;;
;;; Code:

(define-module (veritas generator)
  #:declarative? #t
  #:export (generate-integer generate-char generate-list generate-symbol
                             generate-string))

(define* (generate-integer #:key (max-bound 8192))
  "Create a generator closure that yields pseudo-random integers.

Returns a generator procedure accepting an explicit random rand object.
The random upper boundary is selected internally up to @var{max-bound}.

@example
(define gen (generate-integer #:max-bound 100))
(gen (seed->random-state 42))
;; or (gen (random-state-from-platform))
@result{} 42
@end example"
  (lambda (rand)
    (let* ((random-size (+ 1
                           (random 100 rand)))
           (bound (min (max random-size 1) max-bound)))
      (random bound rand))))

(define (generate-char)
  "Create a generator closure that yields random printable characters.

Returns a generator procedure accepting an explicit random rand object.
The returned character falls within the printable ASCII range (32 to 126).

@example
(define gen (generate-char))
(gen (seed->random-state 42))
;; or (gen (random-state-from-platform))
@result{} #\\A
@end example"
  (lambda (rand)
    (integer->char (+ 32
                      (random 95 rand)))))

(define* (generate-symbol #:key (min-len 1)
                          (max-len #f))
  "Create a generator closure that yields random Scheme symbols.

Returns a generator procedure accepting an explicit random rand object. Yields
valid, unescaped Scheme symbols with a length constrained between @var{min-len}
and @var{max-len}. Characters are limited to lowercase alphabetical letters (@code{a-z}).

@example
(define gen (generate-symbol #:min-len 3 #:max-len 6))
(gen (seed->random-state 42))
;; or (gen (random-state-from-platform))
@result{} veritas
@end example"
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
  "Create a generator closure that yields a random string.

Returns a generator procedure accepting an explicit random rand object.
Builds a string composed of random printable ASCII characters with an overall
length constrained between @var{min-len} and @var{max-len}.

@example
(define gen (generate-string #:min-len 5 #:max-len 10))
(gen (seed->random-state 42))
;; or (gen (random-state-from-platform))
@result{} \"lorem\"
@end example"
  (let ((list-gen (generate-list (generate-char)
                                 #:min-len min-len
                                 #:max-len max-len)))
    (lambda (rand)
      (list->string (list-gen rand)))))

(define* (generate-list elem-generator
                        #:key (min-len 0)
                        (max-len #f))
  "Create a generator closure that yields a list of randomly generated elements.

Returns a generator procedure accepting an explicit random rand object. Leverages
the internal mapping configurations of @var{elem-generator} to compile a list
with a structured size constrained between @var{min-len} and @var{max-len}.

@example
(define gen (generate-list (generate-integer #:max-bound 10)
                           #:min-len 2
                           #:max-len 4))
(gen (seed->random-state 42))
;; or (gen (random-state-from-platform))
@result{} (7 2 9)
@end example"
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
