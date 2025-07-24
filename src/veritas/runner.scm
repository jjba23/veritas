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

(define-module (veritas runner)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (ice-9 ftw)
  #:use-module (veritas prelude)
  #:export (run-tests run-test-file discover-tests))

(define remove-stat
  (match-lambda
    ((name stat)
     name)
    ((name stat children ...)
     (list name
           (map remove-stat children)))))

(define* (run-test-file #:key test-file test-type entrypoint-function)
  (let* ((_ (display (format #f "
running ~a tests from file: ~a, entrypoint: ~a

"
                             test-type test-file entrypoint-function)))
         (m (resolve-module (append '(veritas)
                                    (list (string->symbol test-type)
                                          (string->symbol (string-replace-substring
                                                           test-file ".scm" ""))))))
         (entrypoint (module-ref m
                                 (string->symbol entrypoint-function))))
    (apply entrypoint
           '())))

(define* (getenv-or var
                    #:key (default #f))
  (let* ((v (getenv (symbol->string var))))
    (if (not v) default v)))

(define* (run-tests)
  (let* ((scan-dir (getenv-or 'VERITAS_RUNNER_SCAN_DIR
                              #:default "./"))
         (test-top-dir (getenv-or 'VERITAS_RUNNER_TEST_TOP_DIR
                                  #:default "test"))
         (entrypoint-function (getenv-or 'VERITAS_RUNNER_ENTRYPOINT_FUNCTION
                                         #:default "spec"))
         (test-module (getenv-or 'VERITAS_RUNNER_TEST_MODULE
                                 #:default "veritas"))
         (test-type (getenv-or 'VERITAS_RUNNER_TEST_TYPE
                               #:default "unit"))
         (_ (begin
              (display (format #f "\nscanning directories for ~a tests..."
                               test-type))
              (display (format #f
                        "
scanning present working directory for ~a tests..."
                        test-type))
              (newline)))
         (files (remove-stat (file-system-tree scan-dir)))
         (interesting-files (flatten-list (discover-tests #:test-type
                                                          test-type
                                                          #:scan-dir scan-dir
                                                          #:test-top-dir
                                                          test-top-dir
                                                          #:test-module
                                                          test-module
                                                          #:dir files))))
    
    (if (not (null? interesting-files))
        (begin
          (display (format #f "\nfound ~a tests: ~a\n" test-type
                           interesting-files))
          (for-each (lambda (file)
                      (run-test-file #:test-file file
                                     #:test-type test-type
                                     #:entrypoint-function entrypoint-function))
                    interesting-files))
        (begin
          (display (format #f "\ncould not find any ~a tests!" test-type))))))

(define* (discover-tests #:key test-type
                         scan-dir
                         test-top-dir
                         test-module
                         dir)
  (let* ((discovery-targets (list scan-dir test-top-dir test-module)))
    (cond
      ((and (list? dir)
            (member (car dir) discovery-targets))
       (discover-tests #:test-type test-type
                       #:scan-dir scan-dir
                       #:test-top-dir test-top-dir
                       #:test-module test-module
                       #:dir (cdr dir)))
      ((and (list? dir)
            (equal? test-type
                    (car dir)))
       (flatten-list (cdr dir)))
      ((list? dir)
       (map (lambda (x)
              (discover-tests #:test-type test-type
                              #:scan-dir scan-dir
                              #:test-top-dir test-top-dir
                              #:test-module test-module
                              #:dir x)) dir))
      (else '()))))

