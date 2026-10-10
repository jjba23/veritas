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
  #:declarative? #t
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (veritas prelude)
  #:use-module (veritas veritas)
  #:use-module (veritas shuffle)
  #:export (run-tests run-test-file discover-tests))

(define remove-stat
  (match-lambda
    ((name stat)
     name)
    ((name stat children ...)
     (list name
           (map remove-stat children)))))

(define* (run-test-file #:key test-file test-type entrypoint-function)
  "Load one test file and call its entrypoint.

@var{test-file} is a path relative to the test root and @var{test-type} is the
module prefix, so @code{\"math/algebra.scm\"} with @code{unit} loads
@code{(veritas unit math algebra)}.  @var{entrypoint-function} names the
zero-argument procedure to call in that module."
  (let* ((_ (display (format #f "

running ~a tests from file: ~a, entrypoint: ~a

"
                             test-type test-file entrypoint-function)))
         (module-parts (map string->symbol
                            (string-split (string-replace-substring test-file
                                                                    ".scm" "")
                                          #\/)))
         (m (resolve-module (append '(veritas)
                                    (list (string->symbol test-type))
                                    module-parts)))
         (entrypoint (module-ref m
                                 (string->symbol entrypoint-function))))
    (apply entrypoint
           '())))

(define* (run-tests)
  "Find every test file under the scan root and run each one.

Behaviour comes from the environment:
@itemize
@item @code{VERITAS_SCAN_DIR}: Root folder to scan. Defaults to @code{\"./\"}.
@item @code{VERITAS_TEST_TOP_DIR}: The subfolder boundary identifier containing test suites. Defaults to @code{\"test\"}.
@item @code{VERITAS_ENTRYPOINT}: The signature method executed inside test targets. Defaults to @code{\"spec\"}.
@item @code{VERITAS_TEST_MODULE}: Top level test parent folder filter. Defaults to @code{\"veritas\"}.
@item @code{VERITAS_TEST_TYPE}: Category discriminator keyword used during routing. Defaults to @code{\"unit\"}.
@item @code{VERITAS_SHUFFLE_FILES}: Set to string @code{\"true\"} to shuffle execution sequences using a Fisher-Yates algorithm. Defaults to @code{\"true\"}.
@end itemize"
  (let* ((scan-dir (getenv-or 'VERITAS_SCAN_DIR
                              #:default "./"))
         (test-top-dir (getenv-or 'VERITAS_TEST_TOP_DIR
                                  #:default "test"))
         (entrypoint-function (getenv-or 'VERITAS_ENTRYPOINT
                                         #:default "spec"))
         (test-module (getenv-or 'VERITAS_TEST_MODULE
                                 #:default "veritas"))
         (test-type (getenv-or 'VERITAS_TEST_TYPE
                               #:default "unit"))
         (shuffle-files (equal? "true"
                                (getenv-or 'VERITAS_SHUFFLE_FILES
                                           #:default "true")))
         (_ (begin
              (display (format #f "\nscanning directories for ~a tests..."
                               test-type))
              (display (format #f
                        "
scanning present working directory for ~a tests..."
                        test-type))
              (newline)))
         (files (remove-stat (file-system-tree scan-dir)))
         (interesting-files* (flatten-list (discover-tests #:test-type
                                                           test-type
                                                           #:scan-dir scan-dir
                                                           #:test-top-dir
                                                           test-top-dir
                                                           #:test-module
                                                           test-module
                                                           #:dir files)))
         (interesting-files (if shuffle-files
                                (fisher-yates-shuffle interesting-files*)
                                interesting-files*)))
    
    (if (not (null? interesting-files))
        (begin
          (display (format #f "\nfound ~a tests: ~a\n" test-type
                           interesting-files))
          (for-each (lambda (file)
                      (run-test-file #:test-file file
                                     #:test-type test-type
                                     #:entrypoint-function entrypoint-function))
                    interesting-files)
          (display (format #f "\n\nfinished running tests\n")))
        (begin
          (display (format #f "\ncould not find any ~a tests!" test-type))))))

(define* (discover-tests #:key test-type
                         scan-dir
                         test-top-dir
                         test-module
                         dir)
  "Walk the cleaned file tree @var{dir} and collect candidate test files.

Descends through @var{scan-dir}, @var{test-top-dir} and @var{test-module}, then
hands the contents of a folder named @var{test-type} to
@code{flatten-test-files}."
  (let* ((discovery-targets (list scan-dir test-top-dir test-module)))
    (cond
      ;; An exhausted branch must be treated as empty, otherwise (car dir) is
      ;; called on '() - which happens whenever a leaf shares a name with one
      ;; of the discovery targets (for example a file named after the test
      ;; module).
      ((null? dir)
       '())
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
       (flatten-test-files (cdr dir)))
      ((list? dir)
       (map (lambda (x)
              (discover-tests #:test-type test-type
                              #:scan-dir scan-dir
                              #:test-top-dir test-top-dir
                              #:test-module test-module
                              #:dir x)) dir))
      (else '()))))

(define (flatten-test-files xs)
  "Turn a nested directory listing into relative @code{.scm} file paths.

Anything that is not a Scheme file is dropped.

@example
(flatten-test-files '(\"math\" ((\"algebra.scm\" ()) (\"geometry.scm\" ()))))
@result{} (\"math/algebra.scm\" \"math/geometry.scm\")
@end example"
  (cond
    ((and (pair? xs)
          (string? (car xs))
          (not (string-suffix? ".scm"
                               (car xs)))
          (not (null? (cdr xs))))
     (map (lambda (pp)
            (format #f "~a/~a"
                    (car xs) pp))
          (flatten-test-files (cdr xs))))
    ((and (pair? xs)
          (not (null? (cdr xs))))
     (map flatten-test-files xs))
    ((list? xs)
     (flatten-list (map flatten-test-files xs)))
    ((string? xs)
     xs)
    (else '())))
