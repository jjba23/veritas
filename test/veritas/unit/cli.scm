;; veritas

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; veritas is free software: you can redistribute it and/or modify
;; it under the terms of the Lesser GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; veritas is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with veritas.  If not, see <https://www.gnu.org/licenses/>.

(define-module (veritas unit cli)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (veritas cli)
  #:use-module (veritas reporter)
  #:export (spec))

;; The CLI is a thin layer that only translates flags into VERITAS_*
;; environment variables.  These tests pin down its public surface without ever
;; asking it to run the test suite: --help and --version return before the
;; runner is resolved.

(define (quiet thunk)
  "Call @var{thunk} discarding its output, returning its value."
  (let ((result #f))
    (with-output-to-string (lambda ()
                             (set! result (thunk))))
    result))

(define metadata-suite
  (suite "CLI metadata"
         (test "version and man page name are present"
               (assert-not-empty #:got veritas-version)
               (assert-equal #:expect "veritas(1)" #:got veritas-man-page-name))
         ;; NOTE: assertions must be passed to `test' as direct arguments;
         ;; `test' collects its procedure-valued arguments.  We therefore use
         ;; `apply test' with a mapped list of assertions instead of `for-each'.
         (apply test "help text documents every flag"
                (map (lambda (flag)
                       (assert-contains #:expect veritas-help-text
                                        #:got flag
                                        #:name (format #f "help mentions ~a" flag)))
                     '("--scan-dir" "--test-top-dir" "--test-type"
                       "--entrypoint" "--test-module" "--reporter"
                       "--only-test" "--only-suite" "--only-tag"
                       "--shuffle" "--no-shuffle" "--benchmarks"
                       "--no-benchmarks" "--help" "--version")))
         (apply test "option grammar declares the expected options"
                (map (lambda (option)
                       (assert-true (and (assq option veritas-option-grammar) #t)
                                    #:name (format #f "~a is a known option"
                                                   option)))
                     '(scan-dir test-top-dir test-type entrypoint test-module
                                reporter only-test only-suite only-tag shuffle
                                no-shuffle benchmarks no-benchmarks help version)))))

(define driver-suite
  (suite "CLI driver"
         (test "--help exits successfully"
               (assert-equal #:expect 0
                             #:got (quiet (lambda ()
                                            (run-cli '("--help"))))))
         (test "--version exits successfully"
               (assert-equal #:expect 0
                             #:got (quiet (lambda ()
                                            (run-cli '("--version"))))))))

(define (spec)
  (veritas-run #:debug? #f metadata-suite driver-suite))
