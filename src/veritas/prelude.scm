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
  #:use-module (veritas model)
  #:export (evaluate-thunks-concurrently evaluate-thunks
                                         flatten-list
                                         display-suite-results
                                         display-assertion-results
                                         collect-assertion-successful
                                         collect-assertion-successful*))

(define-syntax-rule (evaluate-thunks-concurrently thunks)
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

(define-syntax-rule (evaluate-thunks thunks)
  (map (lambda (t)
         (apply t
                '())) thunks))

(define (alist-keys alist)
  (cond
    ((null? alist)
     '())
    ((list? alist)
     (fold (lambda (pair acc)
             (cons (car pair) acc))
           '() alist))
    ((pair? (car alist))
     (map car alist))
    (else '())))

(define (flatten-list xs)
  (cond
    ((null? xs)
     '())
    ((pair? xs)
     (append (flatten-list (car xs))
             (flatten-list (cdr xs))))
    (else (list xs))))

(define (collect-assertion-successful* xs)
  (cond
    ((test-result? xs)
     (collect-assertion-successful* (test-result-test-results xs)))
    ((and (pair? xs)
          (eq? (car xs)
               'test-results))
     (map collect-assertion-successful xs))
    ((list? xs)
     (map collect-assertion-successful xs))
    (else '())))

(define (collect-assertion-successful xs)
  (flatten-list (collect-assertion-successful* xs)))

(define (display-assertion-results assertion-results)
  (for-each (lambda (ar)
              (display (assoc-ref ar
                                  'assertion-output))) assertion-results))

(define (display-suite-results all-suite-results)
  (for-each (lambda (asr)
              (display (suite-result-output asr))
              (for-each (lambda (sr)
                          (display (test-result-output sr))
                          (display-assertion-results (assoc-ref (test-result-test-results
                                                                 sr)
                                                                'test-results)))
                        (suite-result-test-results asr))) all-suite-results))
