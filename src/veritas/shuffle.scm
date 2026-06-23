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
  #:export (fisher-yates-shuffle))

;; List shuffle which implements the modern Fisher-Yates shuffle algorithm.
;; It converts the list to a vector for efficient O(1) random access
;; and mutation, shuffles the vector in-place, and then converts it back to a list.
(define (fisher-yates-shuffle xs)
  "Returns a randomly shuffled copy of XS."
  (set! *random-state*
        (random-state-from-platform))
  (if (or (null? xs)
          (null? (cdr xs))) xs
      (let* ((vec (list->vector xs))
             (len (vector-length vec)))
        ;; Loop from the last element down to the second element.
        (do ((i (- len 1)
                (- i 1)))
            ((< i 1))
            ;; Pick a random index j from 0 to i (inclusive).
            (let* ((j (random (1+ i)))
                   ;; Swap the elements at positions i and j.
                   (temp (vector-ref vec i)))
              (vector-set! vec i
                           (vector-ref vec j))
              (vector-set! vec j temp)))
        ;; Convert the shuffled vector back to a list.
        (vector->list vec))))
