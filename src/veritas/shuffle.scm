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

(define-module (veritas shuffle)
  #:declarative? #t
  #:export (fisher-yates-shuffle fresh-seed))

(define (fresh-seed)
  "Return a new seed for a shuffle, drawn from the platform's entropy source.

Report the seed of a shuffled run and the run can be replayed: the same seed and
the same list always produce the same order."
  (random 1000000000
          (random-state-from-platform)))

(define* (fisher-yates-shuffle xs #:key seed)
  "Return the elements of @var{xs} in random order (Fisher-Yates).

Shuffling happens on a vector copy, so @var{xs} is left alone.  @var{seed} makes
the order reproducible, which is how a failing order is replayed; without it a
fresh seed is pulled from @code{random-state-from-platform} on every call, so two
calls on the same list differ.  Lists of one element or fewer come back as they
are.

@example
(equal? (fisher-yates-shuffle '(1 2 3) #:seed 7)
        (fisher-yates-shuffle '(1 2 3) #:seed 7))
@result{} #t
@end example"
  (set! *random-state*
        (if seed
            (seed->random-state seed)
            (random-state-from-platform)))
  (if (or (null? xs)
          (null? (cdr xs))) xs
      (let* ((vec (list->vector xs))
             (len (vector-length vec)))
        ;; Walk backwards from the end, swapping each slot with a random slot
        ;; before it.
        (do ((i (- len 1)
                (- i 1)))
            ((< i 1))
            (let* ((j (random (1+ i)))
                   (temp (vector-ref vec i)))
              (vector-set! vec i
                           (vector-ref vec j))
              (vector-set! vec j temp)))
        (vector->list vec))))
