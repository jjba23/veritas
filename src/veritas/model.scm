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

(define-module (veritas model)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:export (suite-result suite-result?
                         make-suite-result
                         suite-result-name
                         suite-result-output
                         set-suite-result-output!
                         suite-result-test-results
                         test-result
                         test-result?
                         make-test-result
                         test-result-index
                         test-result-output
                         test-result-results
                         pre-test-result
                         pre-test-result?
                         make-pre-test-result
                         pre-test-result-name
                         pre-test-result-results
                         pre-test-result-file
                         pre-test-result-line
                         pre-test-result-duration
                         pre-test-result-attempts
                         make-assertion-result
                         assertion-result
                         assertion-result?
                         assertion-result-index
                         assertion-result-output
                         assertion-result-name
                         assertion-result-successful
                         benchmark-result
                         make-benchmark-result
                         benchmark-result-name
                         benchmark-result?
                         benchmark-result-results
                         result-and-output
                         result-and-output?
                         make-result-and-output
                         result-and-output-result
                         result-and-output-output))

(define-record-type suite-result
  (make-suite-result name test-results output) suite-result?
  (name suite-result-name)
  (test-results suite-result-test-results)
  (output suite-result-output set-suite-result-output!))

(define-record-type test-result
  (make-test-result index output results) test-result?
  (index test-result-index)
  (output test-result-output)
  (results test-result-results))

(define-record-type pre-test-result
  (make-raw-pre-test-result name results file line duration attempts) pre-test-result?
  (name pre-test-result-name)
  (results pre-test-result-results)
  (file pre-test-result-file)
  (line pre-test-result-line)
  (duration pre-test-result-duration)
  (attempts pre-test-result-attempts))

(define* (make-pre-test-result name results
                              #:key (file #f) (line #f) (duration 0)
                              (attempts 1))
  "Return the result of a test named @var{name} holding @var{results}.

@var{file} and @var{line} are where the test was declared, when that is known,
@var{duration} is how many seconds it took, and @var{attempts} is how many
times the test ran - more than one when a retry was needed.  They are what a report needs to
place a test and time it: the JUnit document writes them out, and a CI
annotation uses the file and line to attach a failure to the right place in the
source."
  (make-raw-pre-test-result name results file line duration attempts))

(define-record-type assertion-result
  (make-assertion-result index output name successful)
  assertion-result?
  (index assertion-result-index)
  (output assertion-result-output)
  (name assertion-result-name)
  (successful assertion-result-successful))

(define-record-type benchmark-result
  (make-benchmark-result name results) benchmark-result?
  (name benchmark-result-name)
  (results benchmark-result-results))

(define-record-type result-and-output
  (make-result-and-output result output) result-and-output?
  (result result-and-output-result)
  (output result-and-output-output))
