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

(define-module (veritas unit snapshot)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:use-module (veritas snapshot)
  #:export (spec))

(define vc (list (cons 'reporter 'monotone-reporter)))

(define (ok? assertion)
  (cdr (assq 'assertion-successful (assertion vc))))

(define snapshot-dir "./tmp-snapshot-suite")

;; Guile's `(ice-9 ftw)' does not export `delete-file-recursively', so the
;; cleanup is spelled out here.  It is also what the last test checks: a
;; silently failing cleanup is how this suite first leaked its directory.
(define (entries-of dir)
  "Return the paths directly inside @var{dir}, or @code{()} when it is absent."
  (catch #t
         (lambda ()
           (map (lambda (entry) (string-append dir "/" entry))
                (remove (lambda (entry) (member entry '("." "..")))
                        (scandir dir))))
         (lambda (key . args) '())))

(define (remove-tree! dir)
  "Delete @var{dir} with everything under it, when it is there."
  (for-each (lambda (path)
              (if (file-is-directory? path)
                  (remove-tree! path)
                  (delete-file path)))
            (entries-of dir))
  (catch #t (lambda () (rmdir dir)) (lambda (key . args) #f)))

(define (clean-snapshots!)
  (remove-tree! snapshot-dir))

(define (stored name)
  "Return the value currently recorded for snapshot @var{name}."
  (call-with-input-file (snapshot-path name #:dir snapshot-dir) read))

(define snapshot-suite
  (suite "snapshots"
         #:concurrent? #f
         #:shuffle? #f
         #:before-all (lambda (vc) (clean-snapshots!))
         #:after-all (lambda (vc) (clean-snapshots!))
         (test "the first run writes the snapshot and passes"
               (lambda (vc)
                 ((assert-true (ok? (assert-snapshot #:name "greeting"
                                                     #:got '(hello world)
                                                     #:dir snapshot-dir)))
                  vc)
                 ((assert-true (file-exists? (snapshot-path "greeting"
                                                            #:dir snapshot-dir)))
                  vc)
                 ((assert-equal #:expect '(hello world)
                                #:got (stored "greeting"))
                  vc)))
         (test "the second run matches"
               (lambda (vc)
                 ((assert-true (ok? (assert-snapshot #:name "greeting"
                                                     #:got '(hello world)
                                                     #:dir snapshot-dir)))
                  vc)))
         (test "a mismatch fails and leaves the snapshot alone"
               (lambda (vc)
                 ((assert-false (ok? (assert-snapshot #:name "greeting"
                                                      #:got '(hello there)
                                                      #:dir snapshot-dir)))
                  vc)
                 ((assert-equal #:expect '(hello world)
                                #:got (stored "greeting"))
                  vc)))
         (test "an update rewrites the snapshot and passes"
               (lambda (vc)
                 (setenv "VERITAS_UPDATE_SNAPSHOTS" "true")
                 ((assert-true (ok? (assert-snapshot #:name "greeting"
                                                     #:got '(hello there)
                                                     #:dir snapshot-dir)))
                  vc)
                 (setenv "VERITAS_UPDATE_SNAPSHOTS" "false")
                 ((assert-equal #:expect '(hello there)
                                #:got (stored "greeting"))
                  vc)
                 ((assert-false (snapshot-update?)) vc)))
         (test "a procedure is called when the assertion runs"
               (lambda (vc)
                 (let ((calls 0))
                   ((assert-true (ok? (assert-snapshot #:name "a call counter"
                                                       #:got (lambda ()
                                                               (set! calls
                                                                     (+ calls 1))
                                                               calls)
                                                       #:dir snapshot-dir)))
                    vc)
                   ((assert-equal #:expect 1 #:got calls) vc)
                   ((assert-equal #:expect 1 #:got (stored "a call counter"))
                    vc))))
         (test "a snapshot that cannot be read fails instead of being rewritten"
               (lambda (vc)
                 (let ((path (snapshot-path "damaged" #:dir snapshot-dir)))
                   (catch #t
                          (lambda () (mkdir snapshot-dir))
                          (lambda (key . args) #f))
                   (call-with-output-file path
                     (lambda (port) (display "(unbalanced" port)))
                   ((assert-false (ok? (assert-snapshot #:name "damaged"
                                                        #:got 'anything
                                                        #:dir snapshot-dir)))
                    vc)
                   ((assert-equal #:expect "(unbalanced"
                                  #:got (call-with-input-file path
                                          (lambda (port)
                                            (get-string-all port))))
                    vc))))
         (test "names become file names"
               (lambda (vc)
                 ((assert-equal #:expect "./tmp-snapshot-suite/the-rendered-cart.snap"
                                #:got (snapshot-path "The rendered cart!"
                                                     #:dir snapshot-dir))
                  vc)
                 ((assert-equal #:expect "./tmp-snapshot-suite/spaces.snap"
                                #:got (snapshot-path "  spaces  "
                                                     #:dir snapshot-dir))
                  vc)))
         (test "a snapshot without a name is refused"
               (lambda (vc)
                 ((assert-true
                   (catch #t
                          (lambda () (assert-snapshot #:got 1) #f)
                          (lambda (key . args) #t)))
                  vc)))
         (test "the cleanup removes a tree, nested files included"
               (lambda (vc)
                 (let ((dir (string-append snapshot-dir "/nested/deeper")))
                   (for-each (lambda (level)
                               (catch #t (lambda () (mkdir level))
                                      (lambda (key . args) #f)))
                             (list (string-append snapshot-dir "/nested") dir))
                   (call-with-output-file (string-append dir "/leaf")
                     (lambda (port) (display "x" port)))
                   ((assert-true (file-exists? (string-append dir "/leaf"))) vc)
                   (remove-tree! snapshot-dir)
                   ((assert-equal #:expect '() #:got (entries-of dir)) vc)
                   ((assert-false (file-exists? snapshot-dir)) vc))))))

(define (spec)
  (veritas-run #:debug? #f
               #:shuffle? #f
               snapshot-suite))
