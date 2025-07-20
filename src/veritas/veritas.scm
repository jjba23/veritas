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
                            collect-assertion-successful
                            fisher-yates-shuffle)
  #:export (test run-suites suite))

(define* (test name
               #:key (debug? #f)
               (shuffle? #t)
               (skip? #f)
               #:rest cases)
  (if skip? #f
      (lambda (reporter)
        ((assoc-ref reporter
                    'log-test)
         name)

        (let* ((procedure-cases (filter procedure? cases))
               (indexed-cases (map (lambda (case-proc i)
                                     (list i case-proc))
                                   (if shuffle?
                                       (fisher-yates-shuffle procedure-cases)
                                       procedure-cases)
                                   (iota (length procedure-cases))))
               (assertion-thunks (map (lambda (indexed-case)
                                        (let* ((idx (car indexed-case))
                                               (case-proc (cadr indexed-case))
                                               (result (case-proc reporter)))
                                          (lambda ()
                                            `((assertion-index unquote idx)
                                              (assertion-name unquote
                                                              (assoc-ref
                                                               result
                                                               'assertion-name))
                                              (assertion-successful unquote
                                                                    (assoc-ref
                                                                     result
                                                                     'assertion-successful))))))
                                      indexed-cases))
               (test-results (evaluate-thunks assertion-thunks)))
          
          (when debug?
            (newline)
            (write test-results)
            (newline))

          `((test-name unquote name)
            (test-results unquote test-results))))))

(define* (suite name
                #:key (debug? #f)
                (shuffle? #t)
                (skip? #f)
                (concurrent? #t)
                #:rest tests)
  (if skip? #f
      (lambda (reporter)
        ((assoc-ref reporter
                    'log-suite)
         name)

        (let* ((procedure-tests (filter procedure? tests))
               (indexed-tests (map (lambda (test-proc i)
                                     (list i test-proc))
                                   (if shuffle?
                                       (fisher-yates-shuffle procedure-tests)
                                       procedure-tests)
                                   (iota (length procedure-tests))))
               (test-thunks (map (lambda (indexed-test)
                                   (let ((idx (car indexed-test))
                                         (test-proc (cadr indexed-test)))
                                     (lambda ()
                                       (list idx
                                             (test-proc reporter)))))
                                 indexed-tests))
               (suite-results (if concurrent?
                                  (evaluate-thunks-concurrently test-thunks)
                                  (evaluate-thunks test-thunks)))
               (final-suite-data `((suite-name unquote name)
                                   (suite-results unquote suite-results))))
          (when debug?
            (newline)
            (write final-suite-data)
            (newline)) final-suite-data))))

(define* (run-suites #:key (shuffle? #t)
                     (concurrent? #t)
                     (debug? #f)
                     (reporter colorful-stdout-reporter)
                     #:rest suites)
  (let* ((start-test-time (current-time))
         (valid-suites (filter procedure? suites))
         (ordered-suites (if shuffle?
                             (fisher-yates-shuffle valid-suites) valid-suites))
         (suite-thunks (map (lambda (s)
                              (lambda ()
                                (s reporter))) ordered-suites))
         (all-suite-results (if concurrent?
                                (evaluate-thunks-concurrently suite-thunks)
                                (evaluate-thunks suite-thunks)))
         (test-results (map (lambda (sr)
                              (map cdr
                                   (assoc-ref sr
                                              'suite-results)))
                            all-suite-results))
         (assertion-results (collect-assertion-successful test-results))
         (end-test-time (current-time))
         (seconds-elapsed (- end-test-time start-test-time)))
    
    (display (format #f "\nran ~a test suites in ~a seconds"
                     (length valid-suites) seconds-elapsed))
    (when debug?
      (display (format #f "\nassertion-results: ~a" assertion-results)))

    (when (member #f assertion-results)
      ((assoc-ref reporter
                  'log-error)
       "aborting due to error in test case...\n")
      (exit 1))

    all-suite-results))
