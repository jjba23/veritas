;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit impact)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (veritas impact)
  #:export (spec))

;; Selecting the tests a change can reach is the difference between a
;; twenty-second edit loop and a five-minute one.  The fixtures below are a
;; tiny project: a password module, a web module that imports it, and an
;; unrelated module, each with a test.

(define vc '((reporter . monotone-reporter)))

;; The fixture lives outside the project, so a run that discovers test files
;; never mistakes one of them for a test of its own.
(define root "/tmp/veritas-impact-suite")

(define (ensure-dir! path)
  (unless (file-exists? path)
    (ensure-dir! (dirname path))
    (catch #t (lambda () (mkdir path)) (lambda args #f))))

(define (write-fixture! path text)
  (ensure-dir! (dirname path))
  (call-with-output-file path
    (lambda (port)
      (display text port))))

(define (prepare!)
  (write-fixture! (format #f "~a/src/acme/password.scm" root)
                  "(define-module (acme password))\n")
  (write-fixture! (format #f "~a/src/acme/web.scm" root)
                  "(define-module (acme web)\n  #:use-module (acme password))\n")
  (write-fixture! (format #f "~a/src/acme/other.scm" root)
                  "(define-module (acme other))\n")
  (write-fixture! (format #f "~a/test/acme/unit/password-spec.scm" root)
                  "(define-module (acme test password-spec))\n")
  (write-fixture! (format #f "~a/test/acme/unit/password.scm" root)
                  "(define-module (acme test password)\n  #:use-module (acme password))\n")
  (write-fixture! (format #f "~a/test/acme/unit/web.scm" root)
                  "(define-module (acme test web)\n  #:use-module (acme web))\n")
  (write-fixture! (format #f "~a/test/acme/unit/other.scm" root)
                  "(define-module (acme test other)\n  #:use-module (acme other))\n")
  (write-fixture! (format #f "~a/test/acme/unit/unrelated.scm" root)
                  "(define-module (acme test unrelated))\n")
  ;; A test whose only dependency on the project is a data file it names.
  (write-fixture! (format #f "~a/test/acme/unit/data-reader.scm" root)
                  (format #f
                          "(define-module (acme test data-reader))\n(define data-file \"~a/data/passwords.txt\")\n"
                          root)))

(define test-files
  (map (lambda (name) (format #f "~a/test/acme/unit/~a" root name))
       '("password-spec.scm" "password.scm" "web.scm"
         "other.scm" "unrelated.scm" "data-reader.scm")))

(define (selected changed)
  (affected-test-files test-files
                       #:src-dir (format #f "~a/src" root)
                       #:changed changed))

(define impact-suite
  (suite "change-aware selection"
         #:shuffle? #f
         #:concurrent? #f
         (test "a file's module name comes from its path"
               (assert-equal #:expect '("veritas" "reporter")
                             #:got (module-name-of-file
                                    "src/veritas/reporter.scm"))
               (assert-equal #:expect '("acme" "password")
                             #:got (module-name-of-file
                                    "src/acme/password.scm")))

         (test "imports are read from a module body"
               (assert-equal
                #:expect '(("acme" "password") ("veritas" "veritas"))
                #:got (module-imports
                       "#:use-module (acme password)\n#:use-module (veritas veritas)\n")))

         (test "a changed module selects its own test and its importers"
               (assert-equal
                #:expect (list (car test-files)
                               (cadr test-files)
                               (caddr test-files))
                #:got (selected (list (format #f "~a/src/acme/password.scm"
                                              root)))))

         (test "an unrelated test is left out"
               (assert-false (member (car (last-pair test-files))
                                           (selected (list (format #f "~a/src/acme/password.scm"
                                                                   root))))))

         (test "a changed test file selects itself"
               (assert-equal
                #:expect (list (cadddr test-files))
                #:got (selected (list (cadddr test-files)))))

         (test "a test that names a changed data file is selected"
               (assert-equal
                #:expect (list (car (last-pair test-files)))
                #:got (selected (list (format #f "~a/data/passwords.txt" root)))))

         (test "a data file the test does not name selects nothing"
               (assert-equal
                #:expect '()
                #:got (selected (list (format #f "~a/data/other.txt" root)))))

         (test "no change selects nothing"
               (assert-equal #:expect '() #:got (selected '())))))

(define (spec)
  (prepare!)
  (veritas-run #:debug? #f
               #:shuffle? #f
               #:concurrent? #f
               impact-suite))
