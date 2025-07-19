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

(define-module (veritas veritas)
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
  #:use-module (veritas reporter)
  #:use-module (veritas assert)
  #:use-module (veritas prelude)
  #:use-module (veritas shuffle)
  #:re-export (assert-equal assert-eqv
                            assert-true
                            assert-false
                            assert-error
                            assert-no-error
                            evaluate-thunks-concurrently
                            collect-assertion-succesful
                            fisher-yates-shuffle)
  #:export (test run-suite suite))

(define* (test name
               #:key (debug? #f)
               (shuffle? #t)
               #:rest cases)
  (lambda (reporter)
    (let* ((_ (apply (assoc-ref reporter
                                'log-test)
                     (list name)))
           (idx -1)
           (ccc (filter procedure? cases))
           (asserts-with-index (map (lambda (a)
                                      (set! idx
                                            (+ idx 1))
                                      (list idx a))
                                    (if shuffle?
                                        (fisher-yates-shuffle ccc) ccc)))
           (assert-thunks (map (lambda (ai)
                                 (lambda ()
                                   `((index unquote
                                            (car ai))
                                     (result unquote
                                             (apply (cadr ai)
                                                    (list reporter))))))
                               asserts-with-index))
           (test-results (evaluate-thunks-concurrently assert-thunks)))
      
      `((test-name unquote name)
        (test-results unquote
                      (map (lambda (a)
                             `((assertion-index unquote
                                                (car a))
                               (assertion-succesful unquote a))) test-results))))))

(define* (suite name
                #:key debug?
                (shuffle? #t)
                #:rest tests)
  (lambda (reporter)
    (apply (assoc-ref reporter
                      'log-suite)
           (list name))
    (let* ((idx -1)
           (ttt (filter procedure? tests))
           (tests-with-index (map (lambda (t)
                                    (set! idx
                                          (+ idx 1))
                                    (list idx t))
                                  (if shuffle?
                                      (fisher-yates-shuffle ttt) ttt)))
           (tests-thunks (map (lambda (ti)
                                (lambda ()
                                  (list (car ti)
                                        (apply (cadr ti)
                                               (list reporter)))))
                              tests-with-index))
           (suite-results (evaluate-thunks-concurrently tests-thunks))
           (s `((suite-name unquote name)
                (suite-results unquote suite-results))))
      (when debug?
        (newline)
        (write s)
        (newline)) s)))

(define* (run-suite #:key (fail-fast? #t)
                    (reporter colorful-stdout-reporter)
                    #:rest suites)
  (let* ((ss (filter procedure? suites))
         (suite-thunks (map (lambda (s)
                              (lambda ()
                                (apply s
                                       (list reporter)))) ss))
         (suite-results (evaluate-thunks-concurrently suite-thunks))
         (test-results-with-id (map (lambda (sr)
                                      (assoc-ref sr
                                                 'suite-results))
                                    suite-results))
         (test-results (map (lambda (ti)
                              (map cdr ti)) test-results-with-id))
         (assertion-results (collect-assertion-succesful test-results))
         (are-there-failures? (member #f assertion-results)))
    
    (newline)

    (when are-there-failures?
      (begin
        (apply (assoc-ref reporter
                          'log-error)
               (list "aborting due to error in test case...\n"))
        (exit 1))) suite-results))

