;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit counts)
  #:use-module (veritas veritas)
  #:use-module (veritas prelude)
  #:use-module (veritas model)
  #:export (spec))

;; The totals a run reports are read off its result tree, so the records here
;; are built by hand: what a suite, a test and an assertion each contribute, and
;; what a benchmark or a task that raised does not.

(define (assertion-result-named name passed?)
  (make-assertion-result 0 "" name passed?))

(define (pre-test-result-named name assertions)
  (make-pre-test-result name assertions))

(define (suite-result-named name children)
  (make-suite-result name children ""))

(define sample-results
  (list (suite-result-named
         "outer"
         (list (pre-test-result-named "one"
                                      (list (assertion-result-named "a" #t)))
               (suite-result-named
                "inner"
                (list (pre-test-result-named "two"
                                             (list (assertion-result-named "b"
                                                                           #t)
                                                   (assertion-result-named "c"
                                                                           #f)))))))
        ;; A loose test, outside any suite.
        (pre-test-result-named "loose"
                               (list (assertion-result-named "d" #t)))
        (make-benchmark-result "a benchmark" '())))

(define (count-of key counts)
  (assoc-ref counts key))

(define (spec)
  (veritas-run #:shuffle? #f
               #:concurrent? #f
               (suite "Global counts"
                 (test "a nested tree counts its suites, its tests and its assertions"
                       (let ((counts (result-counts sample-results)))
                         (assert-equal #:expect 2 #:got (count-of 'suites counts))
                         (assert-equal #:expect 3 #:got (count-of 'tests counts))
                         (assert-equal #:expect 4 #:got (count-of 'assertions counts))
                         (assert-equal #:expect 0 #:got (count-of 'files counts))))
                 (test "the run of a single file counts that file"
                       (assert-equal #:expect 1
                                     #:got (count-of 'files
                                                     (result-counts sample-results
                                                                    #:files 1))))
                 (test "a benchmark and a task that raised are counted as nothing"
                       (assert-equal #:expect empty-result-counts
                                     #:got (result-counts
                                            (list (make-benchmark-result "b"
                                                                         '()))))
                       (assert-equal #:expect empty-result-counts
                                     #:got (result-counts
                                            (list "boom: something went wrong"))))
                 (test "counts add up"
                       (assert-equal #:expect
                                     '((files . 0) (suites . 4) (tests . 6)
                                       (assertions . 8) (flaky . 0))
                                     #:got (add-result-counts (result-counts
                                                               sample-results)
                                                              (result-counts
                                                               sample-results))))
                 (test "the summary reads as one line"
                       (assert-equal #:expect "1 file, 2 suites, 3 tests and 4 assertions"
                                     #:got (result-counts-summary
                                            '((files . 1) (suites . 2)
                                              (tests . 3) (assertions . 4)))))
                 (test "a test that passed on a retry is counted as flaky"
                       (let ((flaky (make-pre-test-result
                                     "flaky"
                                     (list (assertion-result-named "a" #t))
                                     #:attempts 2))
                             (steady (make-pre-test-result
                                      "steady"
                                      (list (assertion-result-named "b" #t)))))
                         (assert-equal #:expect 1
                                       #:got (count-of 'flaky
                                                       (result-counts
                                                        (list flaky))))
                         (assert-equal #:expect 0
                                       #:got (count-of 'flaky
                                                       (result-counts
                                                        (list steady))))
                         (assert-contains
                          #:expect (result-counts-summary
                                    (result-counts (list flaky)))
                          #:got "flaky")))
                 (test "counts survive a trip through a file"
                       (let* ((path "reports/counts-test.scm")
                              (counts (result-counts sample-results #:files 1)))
                         (write-result-counts counts #:path path)
                         (let ((read-back (read-result-counts path)))
                           (delete-file path)
                           (assert-equal #:expect counts
                                         #:got read-back)))))))
