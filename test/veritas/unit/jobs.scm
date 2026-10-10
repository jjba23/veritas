;; veritas

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; veritas is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; veritas is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; Lesser GNU General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with veritas.  If not, see <https://www.gnu.org/licenses/>.

(define-module (veritas unit jobs)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:use-module (veritas runner)
  #:export (spec))

;; `--jobs' runs each file in a worker process.  Forking a process from inside a
;; test would take the test suite with it, so what is checked here is the part
;; that can be checked in place: the option itself, and the merging of what the
;; workers leave behind - their record of failing names and their JUnit data.

(define vc (list (cons 'reporter 'monotone-reporter)))

(define scratch-dir "./tmp-jobs-suite")

(define (scratch path)
  (string-append scratch-dir "/" path))

(define (remove-tree! dir)
  "Delete @var{dir} with everything under it, when it is there."
  (catch #t
         (lambda ()
           (for-each (lambda (entry)
                       (unless (member entry '("." ".."))
                         (let ((path (string-append dir "/" entry)))
                           (if (file-is-directory? path)
                               (remove-tree! path)
                               (delete-file path)))))
                     (scandir dir))
           (rmdir dir))
         (lambda (key . args) #f)))

;; Both hooks leave the tree absent: the files under test are the ones the code
;; creates, and a suite that leaks its scratch directory is a suite that fails
;; for someone else later.
(define (clean-scratch!)
  (remove-tree! scratch-dir))

(define (junit-data tests failures)
  "Return JUnit data with @var{tests} cases of which @var{failures} failed."
  (list (cons 'tests tests)
        (cons 'failures failures)
        (cons 'errors 0)
        (cons 'skipped 0)
        (cons 'xml (format #f "<testsuite name=\"s~a\"/>\n" tests))))

(define jobs-suite
  (suite "worker processes"
         #:concurrent? #f
         #:shuffle? #f
         #:before-all (lambda (vc) (clean-scratch!))
         #:after-all (lambda (vc) (clean-scratch!))
         (test "a job count is a whole number of processes"
               (lambda (vc)
                 ((assert-equal #:expect 4 #:got (parse-jobs "4")) vc)
                 ((assert-equal #:expect 1 #:got (parse-jobs "1")) vc)
                 ((assert-equal #:expect #f #:got (parse-jobs #f)) vc)
                 ;; Anything else is an error: a CI job that asked for
                 ;; parallelism and silently got none is a lie.
                 ((assert-true (catch #t (lambda () (parse-jobs "0") #f)
                                      (lambda (key . args) #t)))
                  vc)
                 ((assert-true (catch #t (lambda () (parse-jobs "two") #f)
                                      (lambda (key . args) #t)))
                  vc)
                 ((assert-true (catch #t (lambda () (parse-jobs "2.5") #f)
                                      (lambda (key . args) #t)))
                  vc)))
         (test "the records of several runs merge into one"
               (lambda (vc)
                 ;; Names are deduplicated, and the order they were found in is
                 ;; kept so a rerun reads the way the run did.
                 ((assert-equal #:expect '("a" "b")
                                #:got (merge-failed-names (list '("a")
                                                                '()
                                                                '("b" "a"))))
                  vc)
                 ;; Every run passing means nothing to rerun.
                 ((assert-equal #:expect '()
                                #:got (merge-failed-names (list '() '())))
                  vc)
                 ;; No record at all is no information, not a passing run.
                 ((assert-equal #:expect #f
                                #:got (merge-failed-names (list #f #f)))
                  vc)
                 ((assert-equal #:expect '("a")
                                #:got (merge-failed-names (list #f '("a"))))
                  vc)))
         (test "a record can be written where a worker puts it"
               (lambda (vc)
                 (let ((path (scratch "worker.last-failed.scm")))
                   ((assert-equal #:expect '("the failing one")
                                  #:got (write-last-failed-names! '("the failing one")
                                                                  #:path path))
                    vc)
                   ((assert-equal #:expect '("the failing one")
                                  #:got (read-last-failed #:path path))
                    vc)
                   ;; A path whose directory does not exist yet is made.
                   ((assert-equal #:expect '()
                                  #:got (write-last-failed-names! '()
                                                                  #:path (scratch "deep/made/up.scm")))
                    vc)
                   ((assert-true (file-exists? (scratch "deep/made/up.scm"))) vc))))
         (test "JUnit data merges by summing and concatenating"
               (lambda (vc)
                 (let ((merged (merge-junit-data (list (junit-data 2 1)
                                                       (junit-data 3 0)))))
                   ((assert-equal #:expect 5 #:got (assoc-ref merged 'tests)) vc)
                   ((assert-equal #:expect 1 #:got (assoc-ref merged 'failures)) vc)
                   ((assert-equal #:expect 0 #:got (assoc-ref merged 'errors)) vc)
                   ((assert-equal #:expect 0 #:got (assoc-ref merged 'skipped)) vc)
                   ((assert-equal #:expect
                                  (string-append "<testsuite name=\"s2\"/>\n"
                                                 "<testsuite name=\"s3\"/>\n")
                                  #:got (assoc-ref merged 'xml))
                    vc))))
         (test "JUnit data written by a worker can be read back and merged"
               (lambda (vc)
                 (let ((path (scratch "1.junit.scm")))
                   (write-junit-data (junit-data 1 1) #:path path)
                   ((assert-equal #:expect 1
                                  #:got (assoc-ref (read-junit-data path) 'tests))
                    vc)
                   ;; Missing or damaged data is nothing to merge, never an
                   ;; error that takes down the process doing the merging.
                   ((assert-false (read-junit-data (scratch "absent.junit.scm")))
                    vc)
                   (call-with-output-file (scratch "damaged.junit.scm")
                     (lambda (port) (display "(this is not junit data" port)))
                   ((assert-false (read-junit-data (scratch "damaged.junit.scm")))
                    vc))))
         (test "a JUnit document is written from merged data"
               (lambda (vc)
                 (let* ((path (scratch "merged.xml"))
                        (data (merge-junit-data (list (junit-data 2 1)
                                                      (junit-data 3 0)))))
                   (write-junit-document data #:seconds 0.5 #:path path)
                   (let ((text (call-with-input-file path get-string-all)))
                     ((assert-contains #:expect text
                                       #:got "tests=\"5\" failures=\"1\" errors=\"0\" skipped=\"0\" time=\"0.5\"")
                      vc)
                     ((assert-contains #:expect text
                                       #:got (assoc-ref data 'xml))
                      vc)))))))

(define (spec)
  (veritas-run #:debug? #f
               #:shuffle? #f
               jobs-suite))
