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
  #:use-module (maak maak))

;; ====== Maak constants ======

;; The URL for the CSS style-sheet used to style API documentation.
(define api-docs-css-url
  "https://jointhefreeworld.org/static/css/jointhefreeworld.css")

;; The path to the directory where the API documentation will be deployed.
(define deploy-api-docs-dir
  "/srv/http/jointhefreeworld.org")

;; The name of the project, represented as a symbol.
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
  "Format Scheme source code files according to the Guix style guide."
  ($ '("find . -maxdepth 8 -name '*.scm'" "-type f -exec guix style -f {} \\;")))

(define (repl)
  "Development REPL."
  (time-machine-manifest-shell '("guile -L ./src"
                                 "-c '((@ (ares server) run-nrepl-server))'")))

(define (docs)
  "Generate technical API documentation for Veritas."
  (delete-file-recursively "doc")
  (time-machine-manifest-shell (list (~ "documenta api ./src/~a && "
                                        project-name)
                                     (~ "texi2any -v --html --css-ref=~a"
                                        api-docs-css-url)
                                     "./doc/api/index.texi")
                               #:verbose? #t)
  (mv "./index" "./doc/api-dist"))

(define (deploy)
  "Deploy the project's documentation."
  (docs)
  (remkdir-p (~ "~a/api-docs/~a" deploy-api-docs-dir project-name))
  (mv "doc/api-dist/*"
      (~ "~a/api-docs/~a" deploy-api-docs-dir project-name))
  (delete-file-recursively "doc"))

(define (test)
  "Run project's tests."
  (delete-file-recursively "reports/lcov/coverage*"
                           #:verbose? #f)
  (delete-file-recursively "reports/html"
                           #:verbose? #f)
  ($ (list "VERITAS_TEST_TYPE=unit" "VERITAS_RUN_BENCHMARKS=true"
           "guix shell -m manifest.scm --" "guile -L ./src -L ./test"
           "-c '((@ (veritas runner) run-tests))'"))
  (coverage-report))

(define (coverage-report)
  "Make test coverage report."
  (catch #t
         (lambda ()
           ($ '("genhtml reports/lcov/*.info --output-directory reports/html &> /dev/null")))
         (lambda (key . args)
           #f)))

(define (validate)
  "Format, compile and test."
  (fmt)
  (compile)
  (test))

(define (hot-reload)
  "Hot-reloading developer's setup."
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
  "Alias to hot-reloading."
  (hot-reload))

(define (dev)
  "Developer's setup."
  (hot-reload))

(define (default)
  "Default task to be run, list available tasks."
  ($ '("maak --list")))
