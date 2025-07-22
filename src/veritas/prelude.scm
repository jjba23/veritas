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
  #:use-module (veritas reporter)
  #:export (evaluate-thunks-concurrently evaluate-thunks
                                         flatten-list
                                         display-all-results
                                         display-assertion-results
                                         collect-assertion-successful
                                         collect-assertion-successful*))

(define-syntax-rule (evaluate-thunks-concurrently thunks)
  "Given a list of functions that don't take any inputs, run each one of
those functions concurrently, and then return a list of what each function
returned once they're all finished."
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
  "Given a list of functions that don't take any inputs, run each one of
those functions and return a list of what each function returned."
  (map (lambda (t)
         (apply t
                '())) thunks))

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
    ((assertion-result? xs)
     (assertion-result-successful xs))
    ((pre-test-result? xs)
     (map collect-assertion-successful*
          (pre-test-result-results xs)))
    ((test-result? xs)
     (collect-assertion-successful* (test-result-results xs)))
    ((list? xs)
     (map collect-assertion-successful xs))
    (else (error (format #f
                  "could not read assertion success from unknown type: ~a" xs)))))

(define (collect-assertion-successful xs)
  (flatten-list (collect-assertion-successful* xs)))

(define-syntax-rule (display-assertion-results assertion-results)
  (for-each (lambda (ar)
              (if (assertion-result? ar)
                  (display (assertion-result-output ar))
                  (error (format #f "not an assertion-result: ~a" ar))))
            assertion-results))

(define-syntax-rule (display-test-results test-results)
  (for-each (lambda (tr)
              (if (test-result? tr)
                  (begin
                    (display (test-result-output tr))
                    (display-assertion-results (pre-test-result-results (test-result-results
                                                                         tr))))
                  (error (format #f "not a test-result: ~a" tr))))
            test-results))

(define-syntax-rule (display-all-results vc all-results)
  (for-each (lambda (asr)
              (cond
                ((suite-result? asr)
                 (begin
                   (display (suite-result-output asr))
                   (display-test-results (suite-result-test-results asr))))
                (else (begin
                        (log-test vc
                                  (pre-test-result-name asr))
                        (display-assertion-results (pre-test-result-results
                                                    asr)))))) all-results))

