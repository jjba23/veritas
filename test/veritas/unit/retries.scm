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

(define-module (veritas unit retries)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (veritas veritas)
  #:use-module (veritas model)
  #:use-module (veritas reporter)
  #:export (spec))

;; A retry is for an intermittent failure.  A test that fails and then passes
;; keeps its attempts in its result, so the report can say it was flaky instead
;; of pretending it always passed.

(define vc '((reporter . monotone-reporter)))

(define (quietly thunk)
  (with-output-to-port (open-output-string) thunk))

(define (run task)
  (quietly (lambda () (task vc))))

(define (successful? result)
  (every (lambda (r) (assertion-result-successful r))
         (pre-test-result-results result)))

(define retry-suite
  (suite "retries"
         #:shuffle? #f
         #:concurrent? #f
         (test "a flaky test passes on its second attempt"
               (lambda (vc)
                 (let ((attempts 0))
                   (let ((result
                          (run (test "flaky" #:retries 1
                                     (lambda (vc)
                                       (set! attempts (+ attempts 1))
                                       ((assert-equal #:expect 2
                                                      #:got attempts)
                                        vc))))))
                     ((assert-equal #:expect 2
                                    #:got (pre-test-result-attempts result))
                      vc)
                     ((assert-true (successful? result)) vc)))))

         (test "a test that never passes is still a failure after a retry"
               (lambda (vc)
                 (let ((result
                        (run (test "always failing" #:retries 1
                                   (assert-true #f)))))
                   ((assert-equal #:expect 2
                                  #:got (pre-test-result-attempts result))
                    vc)
                   ((assert-false (successful? result)) vc))))

         (test "without retries a test runs once"
               (lambda (vc)
                 (let ((result (run (test "once"
                                          (assert-true #f)))))
                   ((assert-equal #:expect 1
                                  #:got (pre-test-result-attempts result))
                    vc))))

         (test "the retry count can be set for a whole run"
               (lambda (vc)
                 (parameterize ((veritas-retries 2))
                   (let ((result (run (test "inherits the default"
                                            (assert-true #f)))))
                     ((assert-equal #:expect 3
                                    #:got (pre-test-result-attempts result))
                      vc)))))))

(define junit-attempts 0)

;; The run summary is not the only place flakiness belongs: a CI dashboard reads
;; the JUnit document, so the case that needed a retry says so there too.
(define junit-flaky-suite
  (suite "flaky in the JUnit document"
         #:shuffle? #f
         (test "a retried pass carries flaky=true"
               (lambda (vc)
                 (let ((result (run (test "flaky junit" #:retries 1
                                          (lambda (vc)
                                            (set! junit-attempts
                                                  (+ junit-attempts 1))
                                            ((assert-equal
                                              #:expect 2
                                              #:got junit-attempts)
                                             vc))))))
                   ((assert-equal #:expect 2
                                  #:got (pre-test-result-attempts result))
                    vc)
                   (let ((xml (assoc-ref (junit-report-data (list result)
                                                            #:seconds 1)
                                         'xml)))
                     ((assert-contains #:expect xml
                                       #:got "flaky=\"true\"")
                      vc)))))))

(define json-flaky-suite
  (suite "flaky in the JSON reporter"
         #:shuffle? #f
         (test "the flaky line carries its own level"
               (lambda (vc)
                 ((assert-contains
                   #:expect
                   (with-output-to-string
                     (lambda ()
                       (parameterize ((veritas-clock (lambda ()
                                                       "2026-01-01T00:00:00Z")))
                         (log-flaky (list (cons 'reporter 'json-reporter))
                                    "1 flaky"))))
                   #:got "\"level\":\"flaky\"")
                  vc)))))

(define (spec)
  (veritas-run #:debug? #f
               #:shuffle? #f
               #:concurrent? #f
               retry-suite
               junit-flaky-suite
               json-flaky-suite))
