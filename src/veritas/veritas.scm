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
  #:use-module (veritas reporter)
  #:use-module (veritas assert)
  #:use-module (veritas shuffle)
  #:re-export (assert-equal assert-eqv
                            assert-true
                            assert-false
                            assert-error
                            assert-no-error
                            fisher-yates-shuffle)
  #:export (test run-suite suite collect-assertion-succesful))

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
           (asserts-i (map (lambda (a)
                             (set! idx
                                   (+ idx 1))
                             (list idx a))
                           (if shuffle?
                               (fisher-yates-shuffle ccc) ccc))))
      
      `((test-name unquote name)
        (test-results unquote
                      (map (lambda (a)
                             (let* ((proc-doc (procedure-documentation (car (cdr
                                                                             a)))))
                               (if proc-doc
                                   (apply (assoc-ref reporter
                                                     'log-assert)
                                          (list proc-doc))
                                   (apply (assoc-ref reporter
                                                     'log-assert)
                                          (list "")))
                               `((assertion-index unquote
                                                  (car a))
                                 (assertion-documentation unquote proc-doc)
                                 (assertion-succesful unquote
                                                      (apply (car (cdr a))
                                                             (list reporter))))))
                           asserts-i))))))

(define* (suite name
                #:key debug?
                (shuffle? #t)
                #:rest tests)
  (lambda (reporter)
    (let* ((_ (apply (assoc-ref reporter
                                'log-suite)
                     (list name)))
           (idx -1)
           (ttt (filter procedure? tests))
           (tests-i (map (lambda (t)
                           (set! idx
                                 (+ idx 1))
                           (list idx
                                 (apply t
                                        (list reporter))))
                         (if shuffle?
                             (fisher-yates-shuffle ttt) ttt)))
           (s `((suite-name unquote name)
                (suite-results unquote tests-i))))
      (when debug?
        (newline)
        (write s)
        (newline)) s)))

(define* (run-suite #:key (fail-fast? #t)
                    (reporter colorful-stdout-reporter)
                    #:rest suites)
  (let* ((ss (filter procedure? suites))
         (suite-results (map (lambda (s)
                               (apply s
                                      (list reporter))) ss))
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

(define (collect-assertion-succesful xs)
  (cond
    ((null? xs)
     '())
    ((pair? xs)
     (let ((head (car xs))
           (tail (cdr xs)))
       (cond
         ((and (pair? head)
               (eq? (car head)
                    'assertion-succesful))
          (cons (cdr head)
                (collect-assertion-succesful tail)))
         (else (append (collect-assertion-succesful head)
                       (collect-assertion-succesful tail))))))
    (else '())))
