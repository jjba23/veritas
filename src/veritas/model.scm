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
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
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
                         test-result-results))

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
