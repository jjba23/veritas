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

(define-module (veritas prelude)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:export (evaluate-thunks-concurrently evaluate-thunks
                                         collect-assertion-succesful))

(define (evaluate-thunks-concurrently thunks)
  (let* ((channel
           (make-channel)))
    (run-fibers (lambda ()
                  (for-each (lambda (func)
                              (spawn-fiber (lambda ()
                                             (let ((result (func)))
                                               (put-message channel result)))))
                            thunks))
                #:drain? #t)
    (map (lambda (_)
           (get-message channel)) thunks)))

(define (evaluate-thunks thunks)
  (map (lambda (t)
         (apply t
                '())) thunks))

(define (collect-assertion-succesful xs)
  (newline)
  (write "xs")
  (newline)
  (write xs)
  (newline)
  (cond
    ((null? xs)
     '())
    ((and (list? xs)
          (assoc-ref xs
                     'suite-results))
     (error "GOOD TRACK"))
    ((list? xs)
     (map collect-assertion-succesful xs))
    (else '())))

