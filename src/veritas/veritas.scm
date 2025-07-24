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
  #:use-module (veritas model)
  #:use-module (statprof)
  #:re-export (assert-equal assert-eqv
                            assert-true
                            assert-false
                            assert-error
                            assert-no-error
                            evaluate-thunks-concurrently
                            collect-assertion-successful
                            fisher-yates-shuffle)
  #:export (test veritas-run suite benchmark veritas-test-only))

(define veritas-only-test
  (make-parameter (getenv-or 'VERITAS_ONLY_TEST
                             #:default #f)))

(define veritas-only-suite
  (make-parameter (getenv-or 'VERITAS_ONLY_SUITE
                             #:default #f)))

(define veritas-run-benchmarks
  (make-parameter (equal? "true"
                          (getenv-or 'VERITAS_RUN_BENCHMARKS
                                     #:default "true"))))

(define* (if-matches-restriction #:key name restriction result)
  (if (not restriction) result
      (if (string-contains (string-downcase name)
                           (string-downcase restriction)) result #f)))

(define* (test name
               #:key (debug? #f)
               (shuffle? #t)
               (skip? #f)
               #:rest cases)
  (let* ((func (lambda (vc)
                 (log-test vc name)
                 (let* ((procedure-cases (filter procedure? cases))
                        (indexed-cases (map (lambda (case-proc i)
                                              (list i case-proc))
                                            (if shuffle?
                                                (fisher-yates-shuffle
                                                 procedure-cases)
                                                procedure-cases)
                                            (iota (length procedure-cases))))
                        (assertion-thunks (map (lambda (indexed-case)
                                                 (let* ((idx (car indexed-case))
                                                        (assertion-proc (cadr
                                                                         indexed-case))
                                                        (result-and-output (capture-result-and-output (lambda ()
                                                                                                        
                                                                                                        
                                                                                                        (assertion-proc
                                                                                                         vc)))))
                                                   (lambda ()
                                                     (make-assertion-result
                                                      idx
                                                      (result-and-output-output
                                                       result-and-output)
                                                      (assoc-ref (result-and-output-result
                                                                  result-and-output)
                                                                 'assertion-name)
                                                      (assoc-ref (result-and-output-result
                                                                  result-and-output)
                                                                 'assertion-successful)))))
                                               indexed-cases))
                        (test-results (evaluate-thunks assertion-thunks)))
                   
                   (when debug?
                     (newline)
                     (write test-results)
                     (newline))

                   (make-pre-test-result name test-results)))))
    (if skip? #f
        (if-matches-restriction #:name name
                                #:restriction (veritas-only-test)
                                #:result func))))

(define* (suite name
                #:key (debug? #f)
                (shuffle? #t)
                (skip? #f)
                (concurrent? #t)
                #:rest tests)
  (let* ((func (lambda (vc)
                 (let* ((procedure-tests (filter procedure? tests))
                        (indexed-tests (map (lambda (test-proc i)
                                              (list i test-proc))
                                            (if shuffle?
                                                (fisher-yates-shuffle
                                                 procedure-tests)
                                                procedure-tests)
                                            (iota (length procedure-tests))))
                        (test-thunks (map (lambda (indexed-test)
                                            (let* ((idx (car indexed-test))
                                                   (test-proc (cadr
                                                               indexed-test))
                                                   (output-port (open-output-string)))
                                              (lambda ()
                                                (let* ((result (with-output-to-port
                                                                output-port
                                                                (lambda ()
                                                                  (test-proc
                                                                   vc))))
                                                       (test-output (get-output-string
                                                                     output-port))
                                                       (_ (close-port
                                                           output-port)))
                                                  (make-test-result idx
                                                   test-output result)))))
                                          indexed-tests))
                        (_ (unless (null? test-thunks)
                             (log-suite vc name)))
                        (test-results (if concurrent?
                                          (evaluate-thunks-concurrently
                                           test-thunks)
                                          (evaluate-thunks test-thunks)))
                        (final-suite-data (make-suite-result name test-results
                                                             "")))
                   
                   (when debug?
                     (newline)
                     (write final-suite-data)
                     (newline)) final-suite-data))))
    (if skip? #f
        (if-matches-restriction #:name name
                                #:restriction (veritas-only-suite)
                                #:result func))))

(define* (benchmark #:key (skip? #f) name runs thunk)
  (let* ((func (lambda (vc)
                 (make-benchmark-result name
                                        (map (lambda (i)
                                               (let* ((sp (with-output-to-string (lambda ()
                                                                                   
                                                                                   
                                                                                   (statprof (lambda ()
                                                                                               
                                                                                               
                                                                                               (thunk)
                                                                                               #f))))))
                                                 `((run-index unquote i)
                                                   (run-details unquote sp))))
                                             (iota runs))))))
    (if skip? #f
        (if (veritas-run-benchmarks) func #f))))

(define* (veritas-run #:key (shuffle? #t)
                      (concurrent? #t)
                      (debug? #f)
                      (reporter colorful-reporter)
                      #:rest tasks)
  (let* ((start-test-time (gettimeofday))
         (start-seconds (car start-test-time))
         (start-microseconds (cdr start-test-time))
         (valid-tasks (filter procedure? tasks))
         (ordered-tasks (if shuffle?
                            (fisher-yates-shuffle valid-tasks) valid-tasks))
         (vc (list (cons 'reporter reporter)))
         (task-thunks (map (lambda (suite-proc)
                             (let* ((output-port (open-output-string)))
                               (lambda ()
                                 (let* ((result (with-output-to-port
                                                 output-port
                                                 (lambda ()
                                                   (suite-proc vc))))
                                        (task-output (get-output-string
                                                      output-port))
                                        (_ (close-port output-port)))
                                   (cond
                                     ((suite-result? result)
                                      (set-suite-result-output! result
                                                                task-output))
                                     (else '()))

                                   result)))) ordered-tasks))
         (all-results (if concurrent?
                          (evaluate-thunks-concurrently task-thunks)
                          (evaluate-thunks task-thunks)))
         (test-results (map (lambda (sr)
                              (cond
                                ((suite-result? sr)
                                 (suite-result-test-results sr))
                                ((pre-test-result? sr)
                                 sr)
                                ((benchmark-result? sr)
                                 '())
                                (else (error (format #f
                                              "could not read test results from unknown type: ~a"
                                              sr))))) all-results))
         (assertion-results (collect-assertion-successful test-results))
         (end-test-time (gettimeofday))
         (end-seconds (car end-test-time))
         (end-microseconds (cdr end-test-time))
         (seconds-elapsed (- end-seconds start-seconds))
         (microseconds-elapsed (- end-microseconds start-microseconds)))
    
    (display-all-results vc all-results)
    (log-info vc
              (format #f "ran ~a tasks in: ~a seconds, ~a microseconds"
                      (length valid-tasks) seconds-elapsed
                      microseconds-elapsed))
    (when debug?
      (log-debug vc
                 (format #f "assertion-results: ~a" assertion-results)))

    (when (member #f assertion-results)
      (log-error vc "\naborting due to error in test case...\n")
      (exit 1))

    all-results))

