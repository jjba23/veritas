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

(define-module (veritas impact)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (module-name-of-file
            module-imports
            referenced-paths
            git-changed-files
            affected-test-files
            changed-selection))

;; Running a whole suite for a one-line change is wasted time, and running
;; nothing is worse.  The files a change can reach are computed from what they
;; import: a changed module reaches every module that imports it, directly or
;; through another, and a test file that imports any of them is affected.  A
;; file name that matches a test's name counts too, because a project does not
;; always import the module it is testing by name.

(define (path-without-prefix path prefix)
  "Return @var{path} with @var{prefix} and one separator removed, when present."
  (if (and (string-prefix? prefix path)
           (> (string-length path) (string-length prefix))
           (char=? (string-ref path (string-length prefix)) #\/))
      (substring path (+ 1 (string-length prefix)) (string-length path))
      path))

(define* (module-name-of-file path #:key (root "src"))
  "Return the module @var{path} defines, as a list of strings.

@code{\"src/veritas/reporter.scm\"} is @code{(\"veritas\" \"reporter\")}.  @var{root}
is the directory the path is relative to, so a project with a different layout
can say so."
  (let* ((relative (path-without-prefix path root))
         (relative (if (string-prefix? "./" relative)
                       (substring relative 2 (string-length relative))
                       relative))
         (stem (if (string-suffix? ".scm" relative)
                   (substring relative 0 (- (string-length relative) 4))
                   relative)))
    (filter (lambda (part) (not (string-null? part)))
            (string-split stem #\/))))

(define (module-imports text)
  "Return the modules @var{text} imports with @code{#:use-module}, as lists.

Only the plain form is read - @code{(veritas reporter)} - because a
@code{#:select} list belongs to the importer, not to the dependency."
  (let ((rx (make-regexp "#:use-module[ \t\n]+\\(")))
    (let loop ((start 0) (found '()))
      (let ((match (regexp-exec rx text start)))
        (if (not match)
            (reverse found)
            (let* ((open (match:end match))
                   (close (string-index text #\) open))
                   (raw (and close (substring text open close)))
                   (clean (and raw
                               (string-delete (lambda (c)
                                                (memv c '(#\( #\))))
                                              raw)))
                   (name (and clean
                              (let loop ((tokens (string-tokenize clean))
                                         (out '()))
                                (if (or (null? tokens)
                                        (string-prefix? "#:" (car tokens)))
                                    (reverse out)
                                    (loop (cdr tokens)
                                          (cons (car tokens) out)))))))
              (loop (if close (+ close 1) (match:end match))
                    (if (and name (pair? name))
                        (cons name found)
                        found))))))))

(define (file-text path)
  "Return the contents of @var{path}, or the empty string when it cannot be read."
  (catch #t
    (lambda ()
      (call-with-input-file path
        (lambda (port)
          (set-port-encoding! port "UTF-8")
          (get-string-all port))))
    (lambda args "")))

(define (scm-files root)
  "Return the full path of every Scheme file under @var{root}.

The walk is recursive and keeps the paths whole: a module's name comes from its
path, so a basename alone would name nothing.  A @var{root} that does not exist
is no files, not an error."
  (catch #t
    (lambda ()
      (file-system-fold (lambda (path stat result) result)
                        (lambda (path stat result)
                          (if (string-suffix? ".scm" path)
                              (cons path result)
                              result))
                        (lambda (path stat result) result)
                        (lambda (path stat result) result)
                        (lambda (path stat result) result)
                        (lambda (path stat errno result) result)
                        '() root))
    (lambda args '())))

(define (module-closure modules imports changed)
  "Return @var{changed} together with every module that reaches it through imports.

@var{modules} is every module in the project and @var{imports} maps a module to
the modules it imports."
  (let loop ((known changed))
    (let ((grown
           (delete-duplicates
            (append known
                    (filter (lambda (module)
                              (any (lambda (import)
                                     (member import known))
                                   (hash-ref imports module '())))
                            modules)))))
      (if (= (length grown) (length known))
          known
          (loop grown)))))

(define (run-git-lines args)
  "Run @var{args} and return its output lines, or the empty list on failure."
  (catch #t
    (lambda ()
      (let* ((port (apply open-pipe* OPEN_READ args))
             (lines (let loop ((out '()))
                      (let ((line (read-line port)))
                        (if (eof-object? line)
                            (reverse out)
                            (loop (cons line out)))))))
        (close-pipe port)
        (filter (lambda (line) (not (string-null? line))) lines)))
    (lambda args '())))

(define* (git-changed-files #:key since)
  "Return the files a change touched, relative to the repository root.

Without @var{since}, the working tree and the index are compared with
@code{HEAD}, and untracked files are included.  With @var{since}, the working
tree is compared with that revision, which is what a CI job wants: the diff of
a branch against its base."
  (delete-duplicates
   (append
    (run-git-lines (if since
                       (list "git" "diff" "--name-only" since)
                       (list "git" "diff" "--name-only" "HEAD")))
    (run-git-lines (list "git" "ls-files" "--others"
                         "--exclude-standard")))))

(define (stem path)
  "Return the file name of @var{path} without its directory or extension."
  (let* ((base (basename path))
         (stem (if (string-suffix? ".scm" base)
                   (substring base 0 (- (string-length base) 4))
                   base)))
    stem))

(define (test-stem path)
  "Return the part of a test file's name that names what it tests."
  (let ((name (stem path)))
    (any (lambda (suffix)
           (and (string-suffix? suffix name)
                (substring name 0 (- (string-length name)
                                     (string-length suffix)))))
         '("-spec" "_spec" "-test" "_test"))))

(define (referenced-paths text)
  "Return the path-like string literals @var{text} mentions.

A test that reads a data file names it as a string, so those strings are the
other dependency edge a project has: not a module, but a file the test cannot
run without.  Only strings that look like a path are kept - one with a slash, a
dot or both - because every other literal in a test is prose."
  (let ((rx (make-regexp "\"([^\"\\n]{1,120})\"")))
    (let loop ((start 0) (found '()))
      (let ((match (regexp-exec rx text start)))
        (if (not match)
            (reverse found)
            (let* ((candidate (match:substring match 1))
                   (trimmed (string-trim-both candidate))
                   (path? (and (not (string-null? trimmed))
                               (not (string-prefix? "http" trimmed))
                               (not (string-contains trimmed " "))
                               (or (string-contains trimmed "/")
                                   (string-contains trimmed ".")))))
              (loop (match:end match)
                    (if path? (cons trimmed found) found))))))))

(define (referenced-path? text path)
  "Return @code{#t} when @var{text} names a file @var{path} ends with."
  (any (lambda (reference)
         (or (string=? reference path)
             (string-suffix? reference path)
             (string-suffix? path reference)))
       (referenced-paths text)))

(define (changed-path? file changed)
  (any (lambda (path)
         (or (string=? path file)
             (string-suffix? path file)
             (string-suffix? file path)))
       changed))

(define* (affected-test-files files #:key (src-dir "src") (changed '()))
  "Return the files from @var{files} that the changes in @var{changed} can affect.

A test is affected when it imports a module the change reaches, when the change
touched the test file itself, when a changed file's name matches the test's name,
or when the test names the changed file as a string: the last two cover a
project whose tests name their subject instead of importing it, and the data a
test cannot run without."
  (let* ((sources (scm-files src-dir))
         (modules (delete-duplicates
                   (map (lambda (path)
                          (module-name-of-file path #:root src-dir))
                        sources)))
         (imports (make-hash-table))
         (_ (for-each
             (lambda (path)
               (hash-set! imports
                          (module-name-of-file path #:root src-dir)
                          (filter (lambda (import) (member import modules))
                                  (module-imports (file-text path)))))
             sources))
         (changed-modules
          (filter (lambda (module) (member module modules))
                  (map (lambda (path)
                         (module-name-of-file path #:root src-dir))
                       (filter (lambda (path)
                                 (string-suffix? ".scm" path))
                               changed))))
         (closure (module-closure modules imports changed-modules)))
    (filter
     (lambda (file)
       (let ((text (file-text file)))
         (or (changed-path? file changed)
             (any (lambda (import) (member import closure))
                  (module-imports text))
             ;; A data file the test names: not an import, but the test cannot
             ;; run without it.
             (any (lambda (path) (referenced-path? text path)) changed)
             (any (lambda (path)
                    (let ((name (stem path)))
                      (and (> (string-length name) 3)
                           (equal? name (test-stem file)))))
                  changed))))
     files)))

(define* (changed-selection files #:key since (src-dir "src"))
  "Return the changed files and the subset of @var{files} they affect."
  (let ((changed (git-changed-files #:since since)))
    (values changed
            (affected-test-files files
                                 #:src-dir src-dir
                                 #:changed changed))))
