;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit determinism)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (fibers channels)
  #:use-module (ice-9 textual-ports)
  #:use-module (veritas veritas)
  #:use-module (veritas model)
  #:use-module (veritas prelude)
  #:use-module (veritas reporter)
  #:use-module (veritas shuffle)
  #:export (spec))

;; Two runs of the same suite, with a fixed clock, have to produce the same
;; report.  That is what makes a snapshot of a run worth keeping, and what makes
;; a difference between two CI runs a real difference instead of the order a
;; hash table happened to be in.

(define vc
  (list (cons 'reporter 'monotone-reporter)))

(define (quietly thunk)
  (with-output-to-port (open-output-string) thunk))

(define (captured thunk)
  "Run @var{thunk} and return what it printed."
  (let ((port (open-output-string)))
    (with-output-to-port port thunk)
    (get-output-string port)))


;; ====== Fixtures ======

(define (flat-suite)
  (suite "flat" #:shuffle? #f
         (test "one" (assert-true #t))
         (test "two" (assert-equal #:expect 4 #:got (+ 2 2)))
         (test "three" (assert-equal #:expect 6 #:got (* 2 3)))))

(define (nested-suite)
  (suite "outer" #:shuffle? #f
         (test "before"
               (assert-equal #:expect 1 #:got 1))
         (suite "inner"
                (test "inside"
                      (assert-equal #:expect -1 #:got (- 1 2))))
         (test "after"
               (assert-equal #:expect 2 #:got 2))))

(define (second-finishes-first-suite)
  "A suite whose second child finishes before its first.

The first child waits for the second to say it is done, so completion order is
the reverse of declaration order without depending on how fast the machine is."
  (let ((started (make-channel)))
    (suite "concurrent"
           #:shuffle? #f
           (test "waits"
                 (begin (get-message started)
                        (assert-true #t)))
           (test "signals"
                 (begin (put-message started #t)
                        (assert-true #t))))))

(define (indices-of result)
  (map test-result-index (suite-result-test-results result)))

(define (documents-of thunk)
  (map (lambda (i)
         (let ((result (quietly (lambda () (thunk)))))
           (junit-report-data (list result) #:seconds 1)))
       '(0 1 2)))

(define (without-times document)
  "Return @var{document} with the measured time attributes removed.

Three runs of one suite agree on everything except how long each test took:
that is a property of the machine, not of the run, and comparing it turns the
check into a coin toss."
  (map (lambda (pair)
         (if (eq? (car pair) 'xml)
             (cons 'xml
                   (regexp-substitute/global #f " time=\"[0-9.]+\""
                                             (cdr pair) 'pre "" 'post))
             pair))
       document))

;; ====== Tests ======

(define determinism-suite
  (suite "determinism"
         #:shuffle? #f
         (test "a suite runs its tests in declaration order, run after run"
               (lambda (vc)
                 (let* ((first (indices-of ((flat-suite) vc)))
                        (second (indices-of ((flat-suite) vc))))
                   ((assert-equal #:expect 3 #:got (length first)) vc)
                   ((assert-equal #:expect second #:got first) vc))))

         (test "a child that finishes first does not jump ahead of a slow one"
               (lambda (vc)
                 ;; The children run in fibers and the second one finishes
                 ;; first, so a result list built as the fibers report puts it
                 ;; ahead and the report reads differently from run to run.
                 (let* ((suite-result
                          (quietly (lambda () ((second-finishes-first-suite)
                                               vc))))
                        (names (map (lambda (child)
                                      (pre-test-result-name
                                       (test-result-results child)))
                                    (suite-result-test-results suite-result))))
                   ((assert-equal #:expect '("waits" "signals")
                                  #:got names)
                    vc))))

         (test "three runs of one suite produce the same JUnit document"
               (lambda (vc)
                 (let ((documents (map without-times
                                       (documents-of
                                        (lambda () ((nested-suite) vc))))))
                   ((assert-equal #:expect (car documents)
                                  #:got (cadr documents))
                    vc)
                   ((assert-equal #:expect (car documents)
                                  #:got (caddr documents))
                    vc))))

         (test "a seeded shuffle gives the same order twice"
               (lambda (vc)
                 ((assert-equal
                   #:expect (fisher-yates-shuffle '(1 2 3 4 5) #:seed 7)
                   #:got (fisher-yates-shuffle '(1 2 3 4 5) #:seed 7))
                  vc)
                 ((assert-equal #:expect 5
                                #:got (length (fisher-yates-shuffle
                                               '(1 2 3 4 5) #:seed 7)))
                  vc)))

         (test "timestamps come from the clock seam"
               (lambda (vc)
                 (parameterize ((veritas-clock
                                 (lambda () "2026-01-01T00:00:00Z")))
                   ((assert-equal #:expect "2026-01-01T00:00:00Z"
                                  #:got (now))
                    vc)
                   ;; A reporter's output with a pinned clock is the same bytes
                   ;; on every run, which is what a snapshot needs.
                   (let ((once (captured (lambda ()
                                          (log-fail (list (cons 'reporter
                                                                'json-reporter))
                                                    "same"))))
                         (twice (captured (lambda ()
                                           (log-fail (list (cons 'reporter
                                                                 'json-reporter))
                                                     "same")))))
                     ((assert-contains #:expect once
                                       #:got "2026-01-01T00:00:00Z")
                      vc)
                     ((assert-equal #:expect twice #:got once) vc)))))))

(define (spec)
  (veritas-run #:debug? #f
               #:shuffle? #f
               #:concurrent? #f
               determinism-suite))
