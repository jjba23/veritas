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

;; The absolute path to the directory where the main project files will be deployed.
(define deploy-dir
  "/srv/http/jointhefreeworld.org")

;; The absolute URL for the CSS style-sheet used to style API documentation.
(define api-docs-css-url
  "https://jointhefreeworld.org/static-assets/api-docs/css/style.css")

;; The absolute path to the directory where the API documentation will be deployed.
(define deploy-api-docs-dir
  "/srv/http/jointhefreeworld.org")

;; The name of the project, represented as a symbol.
(define project-name
  'veritas)

;; The Emacs Lisp script to run for publishing the manual. 
(define emacs-lisp-manual-publish-cmd
  '(progn (find-file "./docs/manual/en.org")
          (org-html-export-to-html)
          (message "finished publishing Veritas manual")))

;; ====== Maak tasks ======

(define (fmt)
  "Format Scheme source code files according to the Guix style guide."
  ($ '("find . -maxdepth 8 -name '*.scm'" "-type f -exec guix style -f {} \\;")))

(define (repl)
  "Development REPL."
  (manifest-shell '("guile -L ./src"
                    "-c '((@ (ares server) run-nrepl-server))'")))

(define (docs)
  "Generate technical API documentation for Veritas."
  (delete-file-recursively "doc")
  (manifest-shell (list (~ "documenta api ./src/~a && " project-name)
                        (~ "texi2any -v --html --css-ref=~a" api-docs-css-url)
                        "./doc/api/index.texi"))
  (mv "./index" "./doc/api-dist"))

(define (build-manual)
  "Render the project's manual."
  (manifest-shell (list (~ "emacs -Q --batch --eval '~s'"
                           emacs-lisp-manual-publish-cmd))
                  #:verbose? #t))

(define (deploy)
  "Deploy the project's documentation."
  (docs)
  (build-manual)
  (mkdir-p (~ "~a/manuals/~a" deploy-dir project-name))
  (delete-file-recursively (~ "~a/manuals/~a/index.html" deploy-dir
                              project-name))
  (mv "docs/manual/en.html"
      (~ "~a/manuals/~a/index.html" deploy-dir project-name)))

(define (test)
  "Run project's tests."
  ($ (list "VERITAS_RUN_BENCHMARKS=true" "VERITAS_TEST_TYPE=unit"
           "guix shell -m manifest.scm --" "guile -L ./src -L ./test"
           "-c '((@ (veritas runner) run-tests))'")))

(define (hot-reload)
  "Hot-reloading developer's setup."
  ($ '("direvent direvent.conf")))

(define (hr)
  "Alias to hot-reloading."
  (hot-reload))

(define (dev)
  "Developer's setup."
  (hot-reload))

(define (default)
  "Default task to be run, list available tasks."
  ($ '("maak --list")))

