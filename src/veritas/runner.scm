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
  "Dynamically resolve and execute a designated test file module.

Accepts three keyword arguments:
@itemize
@item @var{test-file}: The relative string path targeting a test definition file.
@item @var{test-type}: A descriptive categorization string used for module names (e.g., @code{\"unit\"}).
@item @var{entrypoint-function}: A string name identifying the zero-argument procedure to run.
@end itemize

Transforms the target string file path into an expected Guile module path syntax under the top-level
hierarchy prefix @code{(veritas @var{test-type} ...)}, maps the matching entrypoint, and executes it synchronously.

@example
(run-test-file #:test-file \"math/algebra.scm\"
               #:test-type \"unit\"
               #:entrypoint-function \"spec\")
;; Dynamically runs the 'spec' function inside module: (veritas unit math algebra)
@end example"
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
  "Main entry point orchestration runner for looking up, organizing, and executing tests.

Automatically configures operations by reading environment variables on execution:
@itemize
@item @code{VERITAS_SCAN_DIR}: High-level folder root used to scan files. Defaults to @code{\"./\"}.
@item @code{VERITAS_TEST_TOP_DIR}: The subfolder boundary identifier containing test suites. Defaults to @code{\"test\"}.
@item @code{VERITAS_ENTRYPOINT}: The signature method executed inside test targets. Defaults to @code{\"spec\"}.
@item @code{VERITAS_TEST_MODULE}: Top level test parent folder filter. Defaults to @code{\"veritas\"}.
@item @code{VERITAS_TEST_TYPE}: Category discriminator keyword used during routing. Defaults to @code{\"unit\"}.
@item @code{VERITAS_SHUFFLE_FILES}: Set to string @code{\"true\"} to shuffle execution sequences using a Fisher-Yates algorithm. Defaults to @code{\"true\"}.
@item @end itemize

Locates target test collections dynamically via directory traversal hooks, updates stdout buffers with operational summaries, and processes matching test suites consecutively."
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
  "Recursively traverse a cleaned directory metadata structure @var{dir} to isolate relevant test targets.

Dispatches recursive tree matching across structural listings by comparing elements against the designated configuration constraints:
@itemize
@item If directory branches match structural target strings, routes deeper down those tree boundaries.
@item If a matching folder named after @var{test-type} is reached, routes the underlying components to @code{flatten-test-files}.
@end itemize

Returns a multi-dimensional nested string list structure grouping eligible test module file locations."
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
  "Construct explicit relative file path strings from segmented subfolder list representations @var{xs}.

Evaluates list structures matching file layouts recursively to concatenate path segments. Skips or filters
out invalid files or non-Scheme structures that do not match a standard @code{\".scm\"} extension suffix notation.

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
          (flatten-list (cdr xs))))
    ((and (pair? xs)
          (not (null? (cdr xs))))
     (map flatten-test-files xs))
    ((list? xs)
     (flatten-list (map flatten-test-files xs)))
    ((string? xs)
     xs)
    (else '())))
