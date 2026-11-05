;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit rerun)
  #:use-module (veritas veritas)
  #:use-module (veritas model)
  #:use-module (veritas reporter)
  #:export (spec))

;; Result trees are built by hand here: the machinery that walks them to find
;; the names to rerun is what is under test, and building the records directly
;; is the only way to hold a run still.  A test result wraps one child result
;; rather than a list of them, which is the detail the walk got wrong first.

(define vc (list (cons 'reporter 'monotone-reporter)))

(define (assertion ok? name)
  (make-assertion-result 0 "" name ok?))

(define (a-test name ok?)
  (make-test-result 0 ""
                    (make-pre-test-result name (list (assertion ok? name)))))

(define (a-suite name children)
  (make-suite-result name children ""))

(define passing-then-failing
  (list (a-test "the passing one" #t)
        (a-test "the failing one" #f)))

;; The nesting matters: a suite holding a suite holding a test is the shape that
;; used to raise while the names were collected.
(define nested
  (list (a-suite "outer suite" (list (a-suite "inner suite" passing-then-failing)))))

(define all-passing
  (list (a-test "the passing one" #t)))

(define rerun-suite
  (suite "rerunning what failed"
         #:concurrent? #f
         #:shuffle? #f
         (test "a failing test is named and a passing one is not"
               (lambda (vc)
                 ((assert-equal #:expect '("the failing one")
                                #:got (failed-test-names passing-then-failing))
                  vc)))
         (test "names are found at any depth, through nested suites"
               (lambda (vc)
                 ((assert-equal #:expect '("the failing one")
                                #:got (failed-test-names nested))
                  vc)))
         (test "a run that passed records nothing to rerun"
               (lambda (vc)
                 ((assert-equal #:expect '()
                                #:got (failed-test-names all-passing))
                  vc)))
         (test "the record round-trips through the report file"
               (lambda (vc)
                 ((assert-equal #:expect '("the failing one")
                                #:got (write-last-failed! passing-then-failing))
                  vc)
                 ((assert-equal #:expect '("the failing one")
                                #:got (read-last-failed))
                  vc)))
         (test "an empty record is a record, so nothing is rerun"
               (lambda (vc)
                 ((assert-equal #:expect '()
                                #:got (write-last-failed! all-passing))
                  vc)
                 ((assert-equal #:expect '() #:got (read-last-failed)) vc)
                 ;; An empty list is not `#f': it means the last run passed, so
                 ;; a rerun selects nothing instead of selecting everything.
                 ((assert-false (failed-test-wanted? "the failing one"
                                                     #:only '()))
                  vc)
                 ((assert-false (failed-test-wanted? "the failing one"
                                                     #:only '("other")))
                  vc)
                 ((assert-true (failed-test-wanted? "the failing one"
                                                    #:only '("the failing one")))
                  vc)
                 ;; No record at all is the first run: everything runs.
                 ((assert-true (failed-test-wanted? "anything" #:only #f)) vc)
                 ;; A failure with no test name attached records this marker.
                 ((assert-true (failed-test-wanted? "anything" #:only '("#all")))
                  vc)))))

(define (spec)
  (veritas-run #:debug? #f
               #:shuffle? #f
               rerun-suite))
