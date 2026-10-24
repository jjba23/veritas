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
  (time-machine-manifest-shell (list "guild compile -L ./src" "-W3"
                                     "./src/**/*.scm")))

(define (fmt)
  "Format the Scheme sources with `guix style'."
  (run (list "find" "." "-maxdepth" "8" "-name" "*.scm" "-type" "f"
             "-exec" "guix" "style" "-f" "{}" ";")
       #:verbose? #t))

(define (repl)
  "Development REPL."
  (time-machine-manifest-shell '("guile -L ./src"
                                 "-c '((@ (ares server) run-nrepl-server))'")))

(define (docs)
  "Generate the API documentation."
  (delete-file-recursively "doc")
  (time-machine-manifest-shell (list (~ "documenta api ./src/~a && "
                                        project-name)
                                     (~ "texi2any -v --html --css-ref=~a -o index_html"
                                        api-docs-css-url)
                                     "./doc/api/index.texi")
                               #:verbose? #t)
  (mv "./index_html" "./doc/api-dist"))

(define (deploy)
  "Publish the API docs."
  (docs)
  (remkdir-p (~ "~a/api-docs/~a" deploy-api-docs-dir project-name))
  (mv "doc/api-dist/*"
      (~ "~a/api-docs/~a" deploy-api-docs-dir project-name))
  (delete-file-recursively "doc"))

(define (test)
  "Run the test suite."
  (delete-file-recursively "reports/lcov/coverage*"
                           #:verbose? #f)
  (delete-file-recursively "reports/html"
                           #:verbose? #f)
  ($ (list "VERITAS_TEST_TYPE=unit" "VERITAS_RUN_BENCHMARKS=true"
           "guix shell -m manifest.scm --" "guile -L ./src -L ./test"
           "-c '((@ (veritas runner) run-tests))'"))
  (coverage-report))

(define (coverage-report)
  "Build the HTML coverage report."
  (catch #t
         (lambda ()
           ($ '("genhtml reports/lcov/*.info --output-directory reports/html &> /dev/null")))
         (lambda (key . args)
           #f)))

(define (validate)
  "Format, compile and test, in that order."
  (fmt)
  (compile)
  (test))

(define (hot-reload)
  "Rebuild and re-run the tests whenever a file changes."
  (time-machine-manifest-shell (list "watchexec 'maak test'"
                                     "--clear"
                                     (~ "--exts ~a"
                                        (string-join (map symbol->string
                                                      hot-reload-file-types)
                                                     ","))
                                     "--on-busy-update=restart"
                                     "--shell bash"
                                     "--watch .")))

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
