;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit timeout)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (fibers)
  #:use-module (veritas veritas)
  #:use-module (veritas bdd)
  #:use-module (veritas model)
  #:use-module (veritas prelude)
  #:export (spec))

;; The `#:timeout' contract of `test', `suite', `benchmark' and the assertions: a
;; unit inside its deadline is reported as usual, one that overruns it is a
;; failure without a hang, and the fiber of an overrunning unit really is
;; aborted.
;;
;; The units are called directly rather than through `veritas-run', because a
;; timed out unit is *meant* to fail and `veritas-run' would exit the process.

(define validation-client
  (list (cons 'reporter
              'monotone-reporter)))

(define spin-count
  (make-atomic-box 0))

(define (spin-forever)
  "Increment the global counter forever; used to prove a fiber got killed."
  (let loop
    ()
    (atomic-box-set! spin-count
                     (+ 1
                        (atomic-box-ref spin-count)))
    (loop)))

(define (reset-spin!)
  (atomic-box-set! spin-count 0))

(define (run-quietly thunk)
  "Call @var{thunk} swallowing the output its reporter would print."
  (with-output-to-port (open-output-string) thunk))

(define (outcomes-of unit)
  "The flat list of assertion success flags of the result of @var{unit}."
  (run-quietly (lambda ()
                 (collect-assertion-successful (unit validation-client)))))

(define (name-of-first-assertion result)
  "The name of the first assertion recorded in the pre-test result @var{result}."
  (assertion-result-name (car (pre-test-result-results result))))

(define* (spin-settled? #:key (within 3.0))
  "Return @code{#t} once the spin counter stops growing.

Waits at most @var{within} seconds, sleeping (and therefore yielding) between
samples so the aborted fiber gets a chance to run.  The margin is generous: the
watchdog retries the abort for about a second, so a heavily loaded machine may
settle well after the deadline itself."
  (let loop
    ((previous (atomic-box-ref spin-count))
     (waited 0))
    (if (>= waited within) #f
        (begin
          (sleep 0.025)
          (let ((now (atomic-box-ref spin-count)))
            (if (= now previous) #t
                (loop now
                      (+ waited 0.025))))))))

(define (seconds-elapsed thunk)
  "Run @var{thunk} and return its wall clock duration in seconds."
  (let ((start (get-internal-real-time)))
    (thunk)
    (let ((end (get-internal-real-time)))
      (/ (- end start)
         (* 1.0 internal-time-units-per-second)))))

;; ====== Units within their deadline ======

(define quick-test
  (test "quick test with a deadline"
        #:timeout 5
        (assert-equal #:expect 4
                      #:got (+ 2 2)
                      #:name "2 + 2 is 4")
        (assert-true (string=? "veritas" "veritas")
                     #:name "veritas is veritas")))

(define quick-suite
  (suite "quick suite with a deadline"
         #:timeout 5
         (test "quick child"
               (assert-equal #:expect 1
                             #:got (* 1 1)))))

(define lazy-assertion
  (assert-equal #:expect 3
                #:got (lazy (+ 1 2))
                #:name "1 + 2 is 3"
                #:timeout 5))

;; ====== Units that overrun their deadline ======

;; A lifecycle hook runs while the test does, which is the simplest way to keep
;; a *test* busy from the inside.
(define overrunning-test
  (test "overrunning test"
        #:timeout 0.3
        #:before (lambda (vc)
                   (spin-forever))
        (assert-true #t)))

(define overrunning-suite
  (suite "overrunning suite"
         #:timeout 0.3
         (test "child that spins"
               #:before (lambda (vc)
                          (spin-forever))
               (assert-true #t))))

(define overrunning-benchmark
  (benchmark #:name "overrunning benchmark"
             #:runs 2
             #:timeout 0.3
             #:thunk (lambda ()
                       (spin-forever))))

;; An assertion receives its payload as a `lazy' value, evaluated when the
;; assertion runs, so it has to be self contained.
;; `assert-no-error' takes a real closure, which the counter can observe.
(define overrunning-lazy-assertion
  (assert-equal #:expect 1
                #:got (lazy (begin
                              (let loop
                                ()
                                (loop)) 1))
                #:name "spinning comparison"
                #:timeout 0.3))

(define overrunning-thunk-assertion
  (assert-no-error (lambda ()
                     (spin-forever))
                   #:name "spinning thunk"
                   #:timeout 0.3))

(define blocked-test
  (test "blocked test"
        #:timeout 0.3
        #:before (lambda (vc)
                   (sleep 30))
        (assert-true #t)))

(define overrunning-scenario
  (scenario "overrunning scenario"
            #:timeout 0.3
            (given "a spinning step"
                   #:before (lambda (vc)
                              (spin-forever))
                   (assert-true #t))))

(define raising-test
  (test "raising test"
        #:timeout 5
        #:before (lambda (vc)
                   (error "boom"))
        (assert-true #t)))

;; These tests share one spin counter and wait for the fibers they started to
;; settle, so they run one at a time: a neighbour's spinner looks exactly like a
;; fiber that outlived its deadline.  The run around them stays concurrent, so
;; there is a scheduler for the deadlines to work with.
(define timeout-suite
  (suite "deadlines"
         #:concurrent? #f
         #:shuffle? #f
         ;; Units within their deadline behave as before.
         (test "a test within its deadline passes"
               (assert-equal #:expect '(#t #t)
                             #:got (outcomes-of quick-test)))
         (test "a suite within its deadline passes"
               (assert-equal #:expect '(#t)
                             #:got (outcomes-of quick-suite)))
         (test "an assertion within its deadline passes"
               (assert-equal #:expect '(#t)
                             #:got (outcomes-of (test "lazy assertion"
                                                      lazy-assertion))))
         (test "no deadline means no deadline"
               (assert-equal #:expect '(#t)
                             #:got (outcomes-of (test "no deadline here"
                                                      (assert-true #t)))))
         ;; A unit that overruns its deadline is a failure, and the message
         ;; says so.
         (test "an overrunning test is reported as a failure"
               (begin
                 (reset-spin!)
                 (assert-equal #:expect '(#f)
                               #:got (outcomes-of overrunning-test))))
         (test "the timeout failure explains itself"
               (assert-matches #:pattern "timed out"
                               #:got (name-of-first-assertion (run-quietly (lambda ()
                                                                                   
                                                                                   
                                                                             (overrunning-test
                                                                              validation-client))))
                               #:name "the failure mentions the timeout"))
         (test "an overrunning suite is reported as a failure"
               (assert-equal #:expect '(#f)
                             #:got (outcomes-of overrunning-suite)))
         (test "an overrunning lazy assertion is reported as a failure"
               (assert-equal #:expect '(#f)
                             #:got (outcomes-of (test "lazy overrun"
                                                      overrunning-lazy-assertion))))
         (test "an overrunning thunk assertion is reported as a failure"
               (begin
                 (reset-spin!)
                 (assert-equal #:expect '(#f)
                               #:got (outcomes-of (test "thunk overrun"
                                                        overrunning-thunk-assertion)))))
         (test "an overrunning scenario is reported as a failure"
               (assert-equal #:expect '(#f)
                             #:got (outcomes-of overrunning-scenario)))
         (test "an overrunning benchmark is reported as a failure"
               (if (not (procedure? overrunning-benchmark))
                   (assert-true #t
                                #:name
                                "benchmarks are disabled in this run")
                   (begin
                     (reset-spin!)
                     (assert-equal #:expect '(#f)
                                   #:got (outcomes-of
                                          overrunning-benchmark)))))
         ;; The fibers of overrunning units are aborted, not left spinning
         ;; in the background.
         (test "the fiber of an overrunning test is aborted"
               (begin
                 (reset-spin!)
                 (outcomes-of overrunning-test)
                 (assert-true (spin-settled?)
                              #:name "the spin counter stopped growing")))
         (test "the fiber of an overrunning suite is aborted"
               (begin
                 (reset-spin!)
                 (outcomes-of overrunning-suite)
                 (assert-true (spin-settled?)
                              #:name "the spin counter stopped growing")))
         (test "the fiber of an overrunning thunk assertion is aborted"
               (begin
                 (reset-spin!)
                 (outcomes-of (test "thunk overrun"
                                    overrunning-thunk-assertion))
                 (assert-true (spin-settled?)
                              #:name "the spin counter stopped growing")))
         ;; A blocked unit must not make the caller wait: the deadline returns
         ;; promptly and the pending fiber is dropped.
         (test "a blocked unit does not stall the run"
               (let ((elapsed (seconds-elapsed (lambda ()
                                                 (outcomes-of
                                                  blocked-test)))))
                 (assert-true (< elapsed 5)
                              #:name "the deadline returned promptly")))
         ;; Timed units that raise still raise, like untimed ones.
         (test "a unit that raises still raises"
               (assert-error (lambda ()
                               (run-quietly (lambda ()
                                              (raising-test
                                               validation-client))))
                             #:name
                             "the raised error reached the caller"))))

(define (spec)
  (veritas-run #:debug? #f timeout-suite))
