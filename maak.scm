;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak)
  ;; The container helpers are imported under a prefix because the
  ;; tasks below are named the way maak exposes them on the command
  ;; line -- `maak container-build' -- and a task may not shadow the
  ;; helper it calls.
  #:use-module ((maak container) #:prefix ct:))

;; ====== Maak constants ======

;; Style sheet for the generated API docs.
(define api-docs-css-url
  "https://jointhefreeworld.org/static/css/jointhefreeworld.css")

;; Where the generated API docs are deployed.
(define deploy-api-docs-dir
  "/srv/http/jointhefreeworld.org")

;; Project name, as a symbol.
(define project-name
  'veritas)

(define hot-reload-file-types
  '(scm))

;; ====== Maak tasks ======

(define (compile)
  "Compile all project source code."
  (time-machine-manifest-shell (list "guild" "compile" "-L" "./src" "-W3"
                                     "./src/**/*.scm")
                               #:verbose? #t))

(define (fmt)
  "Format the Scheme sources with `guix style'."
  (run (list "find" "." "-maxdepth" "8" "-name" "*.scm" "-type" "f"
             "-exec" "guix" "style" "-f" "{}" ";")
       #:verbose? #t))

(define (repl)
  "Development REPL."
  (time-machine-manifest-shell (list "guile" "-L" "./src"
                                     "-c" "'((@ (ares server) run-nrepl-server))'")))

(define (docs)
  "Generate the API documentation."
  (delete-file-recursively "doc")
  ;; `documenta' writes the Texinfo tree and `texi2any' renders it: two calls,
  ;; so a failure names the step that failed instead of an `&&' chain.
  (time-machine-manifest-shell (list "documenta" "api"
                                     (~ "./src/~a" project-name))
                               #:verbose? #t)
  (time-machine-manifest-shell (list "texi2any" "-v" "--html"
                                     (~ "--css-ref=~a" api-docs-css-url)
                                     "-o" "index_html"
                                     "./doc/api/index.texi")
                               #:verbose? #t)
  (mv "./index_html" "./doc/api-dist"))

(define (deploy)
  "Publish the API docs."
  (docs)
  (remkdir-p (~ "~a/api-docs/~a" deploy-api-docs-dir project-name))
  (mv "doc/api-dist/*"
      (~ "~a/api-docs/~a" deploy-api-docs-dir project-name)
      #:expand? #t)
  (delete-file-recursively "doc"))

;; The project's own test run measures coverage: the numbers are the point of
;; `maak test', and a run that measures nothing is a bug worth seeing rather
;; than a setting to remember.  `test-fast' runs the same suite without the
;; measurement, for the edit-run loop.
(define coverage-environment
  '("VERITAS_COVERAGE=true"
    ;; The test tree is the instrument, not the subject, and the vendored
    ;; fibers override is a dependency the project builds on.
    "VERITAS_COVERAGE_EXCLUDE=test:override"))

(define (test)
  "Run the test suite with coverage of the sources."
  (delete-file-recursively "reports/lcov"
                           #:expand? #t)
  (delete-file-recursively "reports/html")
  ;; The runner writes the five reports itself (LCOV, JSON, Cobertura, JaCoCo
  ;; and the one-page HTML), merged over every test file.
  (manifest-shell (list "guile" "-L" "./src" "-L" "./test"
                        "-c" "'((@ (veritas runner) run-tests))'")
                  #:env (append '("VERITAS_TEST_TYPE=unit"
                                  "VERITAS_RUN_BENCHMARKS=true")
                                coverage-environment)))

(define (test-fast)
  "Run the test suite without measuring coverage."
  (manifest-shell (list "guile" "-L" "./src" "-L" "./test"
                        "-c" "'((@ (veritas runner) run-tests))'")
                  ;; The explicit `false' wins over the test files that ask for
                  ;; coverage themselves, so this really is the quick loop.
                  #:env '("VERITAS_TEST_TYPE=unit"
                          "VERITAS_RUN_BENCHMARKS=true"
                          "VERITAS_COVERAGE=false")))

(define (coverage-report)
  "Rebuild the coverage reports from the LCOV trace of the last run."
  ;; The run already writes them; this is for rebuilding after the fact, and it
  ;; needs nothing but Guile - no `genhtml', no extra tool to install.
  (manifest-shell (list "guile" "-L" "./src"
                        "-c"
                        "'((@ (veritas coverage) regenerate-coverage-reports))'")
                  #:allow-failure? #t))

(define (validate)
  "Format, compile and test, in that order."
  (fmt)
  (compile)
  (test))

(define (hot-reload)
  "Rebuild and re-run the tests whenever a file changes."
  (time-machine-manifest-shell (list "watchexec" "'maak test'"
                                     "--clear"
                                     "--exts" (string-join
                                               (map symbol->string
                                                    hot-reload-file-types)
                                               ",")
                                     "--on-busy-update=restart"
                                     "--shell" "bash"
                                     "--watch" ".")))

(define (hr)
  "Short alias for `hot-reload'."
  (hot-reload))

(define (dev)
  "Start the hot-reload loop."
  (hot-reload))

(define (default)
  "List the available tasks."
  (list-tasks))

;; ====== Container tasks ======

(define (container-build)
  "Build the container image for veritas."
  (ct:container-build "." #:tag "veritas:latest" #:verbose? #t))

(define (container-run)
  "Run the container image for veritas."
  (ct:container-run "veritas" #:interactive? #t #:tty? #t #:verbose? #t))

(define (package-docker)
  "Build the container image for veritas."
  (container-build))
