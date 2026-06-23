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
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (veritas model)
  #:use-module (veritas reporter)
  #:export (evaluate-thunks-concurrently evaluate-thunks
                                         flatten-list
                                         capture-result-and-output
                                         getenv-or
                                         display-all-results
                                         display-assertion-results
                                         collect-assertion-successful
                                         collect-assertion-successful*))

(define-syntax-rule (evaluate-thunks-concurrently thunks)
  "Given a list of functions that don't take any inputs, run each one of
those functions concurrently, and then return a list of what each function
returned once they're all finished."
  (let ((channel
          (make-channel)))
    (run-fibers (lambda ()
                  ;; Spawn producers
                  (for-each (lambda (func)
                              (spawn-fiber (lambda ()
                                             ;; Catch exceptions so the fiber doesn't die silently
                                             (catch #t
                                                    (lambda ()
                                                      (let ((result (func)))
                                                        (put-message channel
                                                                     result)))
                                                    (lambda (key . args)
                                                      ;; Send the error down the channel to prevent hanging
                                                      (put-message channel
                                                                   (format #f
                                                                    "~a: ~a"
                                                                    key args)))))))
                            thunks)

                  ;; Consume messages INSIDE the run-fibers block
                  ;;
                  ;; Note: #:drain? #t is not needed because the `map` loop implicitly
                  ;; blocks the main fiber until all thunks have reported back.
                  (map (lambda (_)
                         (get-message channel)) thunks)))))

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
    ((unspecified? xs)
     '())
    ((assertion-result? xs)
     (assertion-result-successful xs))
    ((pre-test-result? xs)
     (map collect-assertion-successful*
          (pre-test-result-results xs)))
    ((test-result? xs)
     (collect-assertion-successful* (test-result-results xs)))
    ((benchmark-result? xs)
     '())
    ((list? xs)
     (map collect-assertion-successful xs))
    ((equal? #t xs)
     '())
    (else (error (format #f
                  "could not read assertion success from unknown type: ~a" xs)))))

(define (collect-assertion-successful xs)
  (flatten-list (collect-assertion-successful* xs)))

(define (display-assertion-results assertion-results)
  (for-each (lambda (x)
              (catch #t
                     (lambda ()
                       (if (assertion-result? x)
                           (display (assertion-result-output x))
                           (error (format #f "not an assertion-result: ~a" x))))
                     (lambda (key . args)
                       (format #t "could not display assertion: ~a: ~a" key
                               args)))) assertion-results))

(define-syntax-rule (display-test-results test-results)
  (for-each (lambda (x)
              (cond
                ((unspecified? x)
                 '())
                ((test-result? x)
                 (begin
                   (catch #t
                          (lambda ()
                            (display (test-result-output x)))
                          (lambda (key . args)
                            (format #t "~a: ~a" key args)))
                   (display-assertion-results (pre-test-result-results (test-result-results
                                                                        x))))
                 '())

                (else '()))) test-results))

(define-syntax-rule (display-all-results vc all-results)
  (for-each (lambda (x)
              (cond
                ((suite-result? x)
                 (begin
                   (display (suite-result-output x))
                   (display-test-results (suite-result-test-results x))))
                ((pre-test-result? x)
                 (begin
                   (log-test vc
                             (pre-test-result-name x))
                   (display-assertion-results (pre-test-result-results x))))
                ((benchmark-result? x)
                 (log-benchmark vc x))
                (else (error (format #f
                              "could not display result of unknown type: ~a" x)))))
            all-results))

(define* (getenv-or var
                    #:key (default #f))
  (catch #t
         (lambda ()
           (let* ((v (getenv (symbol->string var))))
             (if (not v) default v)))
         (lambda (key . args)
           (display (format #f
                     "could not read environment variable ~a, return default: ~a
error: ~a: ~a"
                     var
                     default
                     key
                     args)) default)))

(define* (capture-result-and-output thunk)
  (let* ((output-port (open-output-string))
         (result (with-output-to-port output-port thunk))
         (the-output (get-output-string output-port))
         (_ (close-port output-port)))
    (make-result-and-output result the-output)))
