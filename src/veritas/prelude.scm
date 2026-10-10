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

(define-module (veritas prelude)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 atomic)
  #:use-module ((ice-9 threads)
                #:select (call-with-new-thread))
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module ((fibers operations)
                #:select (choice-operation perform-operation wrap-operation))
  #:use-module ((fibers scheduler)
                #:select (current-scheduler scheduler-kernel-thread))
  #:use-module ((fibers timers)
                #:select (sleep-operation))
  #:use-module (veritas model)
  #:use-module (veritas reporter)
  #:export (evaluate-thunks-concurrently evaluate-thunks
                                         flatten-list
                                         capture-result-and-output
                                         getenv-or
                                         display-all-results
                                         display-assertion-results
                                         collect-assertion-successful
                                         collect-assertion-successful*
                                         call-with-timeout
                                         make-timeout-assertion-result))

(define-syntax-rule (evaluate-thunks-concurrently thunks)
  "Run @var{thunks} concurrently in fibers and collect what they return.

Blocks until every thunk has reported back through a shared channel.  A thunk
that raises sends a formatted error string instead of a value, so one failure
cannot hang the run.  Results come back in completion order, which is not the
order of @var{thunks}.

@example
(evaluate-thunks-concurrently
 (list (lambda () (sleep 1) 'delayed)
       (lambda () 'instant)))
@result{} (instant delayed)
@end example"
  (let ((channel
          (make-channel)))
    (run-fibers (lambda ()
                  (for-each (lambda (func)
                              (spawn-fiber (lambda ()
                                             ;; Report the exception instead of letting the
                                             ;; fiber die without a word.
                                             (catch #t
                                                    (lambda ()
                                                      (let ((result (func)))
                                                        (put-message channel
                                                                     result)))
                                                    (lambda (key . args)
                                                      (put-message channel
                                                                   (format #f
                                                                    "~a: ~a"
                                                                    key args)))))))
                            thunks)

                  ;; Consumed inside run-fibers: this map blocks the main fiber until
                  ;; every thunk has reported, so no #:drain? is needed.
                  (map (lambda (_)
                         (get-message channel)) thunks)))))

(define-syntax-rule (evaluate-thunks thunks)
  "Evaluate @var{thunks} one after another, keeping their order.

The result list lines up with the input list."
  (map (lambda (t)
         (apply t
                '())) thunks))

(define (flatten-list xs)
  "Flatten @var{xs} into a single list, however deeply it nests.

An atom that is not a list comes back wrapped in a one-element list."
  (cond
    ((null? xs)
     '())
    ((pair? xs)
     (append (flatten-list (car xs))
             (flatten-list (cdr xs))))
    (else (list xs))))

(define (collect-assertion-successful* xs)
  "Return the assertion outcomes found inside @var{xs}.

Knows assertion, pre-test, test and suite results, and recurses into lists.
Unspecified values, benchmarks and @code{#t} contribute nothing; an
unrecognized type raises."
  (cond
    ((unspecified? xs)
     '())
    ((assertion-result? xs)
     (assertion-result-successful xs))
    ((pre-test-result? xs)
     (map collect-assertion-successful*
          (pre-test-result-results xs)))
    ((test-result? xs)
     (collect-assertion-successful* (test-result-results xs)))
    ;; Suites can be nested (for example via suite-each or a suite holding
    ;; child suites), so recurse into their test results.
    ((suite-result? xs)
     (map collect-assertion-successful*
          (suite-result-test-results xs)))
    ((benchmark-result? xs)
     '())
    ((list? xs)
     (map collect-assertion-successful xs))
    ((equal? #t xs)
     '())
    ;; A fiber may have failed and reported its exception as a string; treat
    ;; that as a failed assertion rather than aborting the whole run.
    ((string? xs)
     #f)
    (else (error (format #f
                  "could not read assertion success from unknown type: ~a" xs)))))

(define (collect-assertion-successful xs)
  "Return the outcome of every assertion in @var{xs} as a flat list.

Booleans only: @code{#t} for a pass, @code{#f} for a failure."
  (flatten-list (collect-assertion-successful* xs)))

(define (display-assertion-results assertion-results)
  "Print the captured output of each record in @var{assertion-results}.

A record that is not an assertion gets printed as an error instead of bringing
down the reporter."
  (for-each (lambda (x)
              (catch #t
                     (lambda ()
                       (if (assertion-result? x)
                           (display (assertion-result-output x))
                           (error (format #f "not an assertion-result: ~a" x))))
                     (lambda (key . args)
                       (format #t "could not display assertion: ~a: ~a" key
                               args)))) assertion-results))

(define (display-test-results test-results)
  "Print the output and inner assertions of each record in @var{test-results}.

Walks into suites as well, including a suite that arrives wrapped in a
@code{test-result}.

This has to stay a procedure.  As a macro it would re-expand the recursive call
forever and eat memory instead of printing the report."
  (for-each (lambda (x)
              (cond
                ((unspecified? x)
                 '())
                ((suite-result? x)
                 (begin
                   (display (suite-result-output x))
                   (display-test-results (suite-result-test-results x)))
                 '())
                ((test-result? x)
                 (begin
                   (catch #t
                          (lambda ()
                            (display (test-result-output x)))
                          (lambda (key . args)
                            (format #t "~a: ~a" key args)))
                   (let ((inner (test-result-results x)))
                     (cond
                       ((pre-test-result? inner)
                        (display-assertion-results (pre-test-result-results
                                                    inner)))
                       ((suite-result? inner)
                        (display (suite-result-output inner))
                        (display-test-results (suite-result-test-results inner)))
                       (else '()))))
                 '())
                (else '()))) test-results))

(define (display-all-results vc all-results)
  "Print and log every test outcome inside @var{all-results} using reporter client @var{vc}.

Iterates over the compilation of @var{all-results}. Evaluates types dynamically:
@itemize
@item @code{suite-result}: Displays suite output, followed by inner test results.
@item @code{pre-test-result}: Logs the test identity via @var{vc} and displays inner assertions.
@item @code{benchmark-result}: Dispatches to @code{log-benchmark}.
@end itemize

Throws an explicit error if an unrecognized result type is processed."
  (for-each (lambda (x)
              (cond
                ((suite-result? x)
                 (begin
                   (display (suite-result-output x))
                   (display-test-results (suite-result-test-results x))))
                ((pre-test-result? x)
                 (begin
                   (log-test vc
                             (pre-test-result-name x))
                   (display-assertion-results (pre-test-result-results x))))
                ((benchmark-result? x)
                 (log-benchmark vc x))
                ;; A fiber may report a captured exception as a string; surface
                ;; it as an error instead of crashing the reporter.
                ((string? x)
                 (log-error vc
                            (format #f "task raised an error: ~a" x)))
                (else (error (format #f
                              "could not display result of unknown type: ~a" x)))))
            all-results))

(define* (getenv-or var
                    #:key (default #f))
  "Return environment variable @var{var}, or @var{default} when it is unset.

@var{var} is a symbol.  Anything that goes wrong while reading it yields
@var{default} as well."
  (catch #t
         (lambda ()
           (let* ((v (getenv (symbol->string var))))
             (if (not v) default v)))
         (lambda (key . args)
           (display (format #f
                     "could not read environment variable ~a, return default: ~a
error: ~a: ~a"
                     var
                     default
                     key
                     args)) default)))

(define* (capture-result-and-output thunk)
  "Call @var{thunk} and return a @code{result-and-output} record.

The record holds what @var{thunk} returned together with everything it printed,
so a test's output can be reported next to its result."
  (let* ((output-port (open-output-string))
         (result (with-output-to-port output-port thunk))
         (the-output (get-output-string output-port))
         (_ (close-port output-port)))
    (make-result-and-output result the-output)))

;;; ====== Timeouts ======
;;
;; A timeout has to do two things: report a hung test, and stop it from running
;; on in the background.  The test runs in its own fiber; when the deadline
;; passes, a watchdog aborts that fiber, unwinding it back to the prompt around
;; the test.
;;
;; The abort must come from outside the fiber.  A system async marked on the
;; scheduler's kernel thread runs at the next safe point of whichever fiber is
;; current, so a fiber marking itself would only interrupt itself.  The watchdog
;; is therefore a plain kernel thread, and the marker it sends carries the
;; test's prompt tag: %active-timeout makes the async a no-op unless it lands in
;; the timed test, so no other fiber on the scheduler is disturbed.
;;
;; The caller waits on a channel with a deadline, not on the test itself.  A test
;; blocked in a syscall is never resumed by the abort, so waiting on it would
;; bring back the hang the timeout exists to prevent.

(define %active-timeout
  ;; Parameterized to the prompt tag of the timed task while it runs.  That is
  ;; what keeps the async abort from landing anywhere else.
  (make-parameter #f))

(define %timeout-wait-interval-microseconds
  ;; How often the watchdog looks at the clock before the deadline.  Guile
  ;; threads have no reliable timed wait, so it polls instead; the caller keeps
  ;; its own precise deadline, this only decides when a runaway fiber is aborted.
  5000)

(define %timeout-wait-interval-milliseconds
  5)

(define %timeout-scan-interval-microseconds
  ;; How often the watchdog retries the abort.  The first async can land on a
  ;; sibling fiber while the timed one is suspended, so one try is not enough.
  10000)

(define %timeout-scan-attempts
  ;; Cap on those retries, so a task that never runs again cannot keep a
  ;; watchdog thread alive forever.
  100)

(define* (call-with-timeout seconds thunk
                            #:key (on-timeout #f))
  "Run @var{thunk} with an optional deadline of @var{seconds} seconds.

When @var{seconds} is a positive real number the work runs in its own fiber and,
if it has not finished by then, the fiber is aborted and the result of calling
@var{on-timeout} is returned instead.  When @var{seconds} is @code{#f} (the
default, meaning no deadline) @var{thunk} is simply called and its result is
returned.

@var{thunk} must return a single value.  The caller never waits for more than
@var{seconds}, and the runaway fiber is terminated rather than left running.

@example
(call-with-timeout 5
                   (lambda () (expensive-computation))
                   #:on-timeout (lambda () 'gave-up))
@end example"
  (cond
    ((or (not seconds)
         (<= seconds 0))
     (thunk))
    ((current-scheduler)
     (%call-with-timeout/timed seconds thunk on-timeout))
    (else
     ;; Nothing schedules fibers on this thread yet (for example a sequential
     ;; run); start a small scheduler for the duration of the deadline.
     (run-fibers (lambda ()
                   (%call-with-timeout/timed seconds thunk on-timeout))
                 #:parallelism 1))))

(define* (%call-with-timeout/timed seconds thunk on-timeout)
  "Run @var{thunk} in its own fiber, aborting it after @var{seconds} seconds."
  (let* ((scheduler (current-scheduler))
         (kernel-thread (scheduler-kernel-thread scheduler))
         (tag (make-prompt-tag "veritas-timeout"))
         (channel
           (make-channel))
         (finished? (make-atomic-box #f)))
    ;; Watchdog: waits out the deadline, then aborts the task's fiber.  It
    ;; stops as soon as the task reported back, so an idle watchdog never
    ;; outlives the call by more than one poll.
    (call-with-new-thread (lambda ()
                            (let wait
                              ((left (inexact->exact (ceiling (* 1000 seconds)))))
                              (unless (or (atomic-box-ref finished?)
                                          (<= left 0))
                                (usleep %timeout-wait-interval-microseconds)
                                (wait (- left
                                         %timeout-wait-interval-milliseconds))))
                            (let abort
                              ((attempts 0))
                              (when (and (< attempts %timeout-scan-attempts)
                                         (not (atomic-box-ref finished?)))
                                (system-async-mark (lambda ()
                                                     (when (eq? (%active-timeout)
                                                                tag)
                                                       (atomic-box-set!
                                                        finished? #t)
                                                       (abort-to-prompt tag
                                                                        'timeout)))
                                                   kernel-thread)
                                (usleep %timeout-scan-interval-microseconds)
                                (abort (1+ attempts))))))
    ;; The task itself, in its own fiber, inside the prompt the watchdog
    ;; aborts to.  Exceptions are forwarded to the caller and re-raised there,
    ;; so a timed task fails exactly like an untimed one.
    (spawn-fiber (lambda ()
                   (catch #t
                          (lambda ()
                            (call-with-prompt tag
                                              (lambda ()
                                                (parameterize ((%active-timeout
                                                                tag))
                                                  (let ((value (thunk)))
                                                    (atomic-box-set! finished?
                                                     #t)
                                                    (put-message channel
                                                                 (cons 'value
                                                                       value)))))
                                              ;; Only reached through the watchdog's abort.
                                              (lambda (continuation value)
                                                (atomic-box-set! finished? #t)
                                                (put-message channel
                                                             (cons 'aborted
                                                                   value)))))
                          (lambda (key . args)
                            (atomic-box-set! finished? #t)
                            (put-message channel
                                         (cons 'error
                                               (cons key args)))))))
    (let ((outcome (perform-operation (choice-operation (get-operation channel)
                                                        (wrap-operation (sleep-operation
                                                                         seconds)
                                                                        (lambda ()
                                                                          (cons 'deadline
                                                                           #f)))))))
      (unless (eq? 'deadline
                   (car outcome))
        (atomic-box-set! finished? #t))
      (case (car outcome)
        ((value)
         (cdr outcome))
        ((aborted deadline)
         (if on-timeout
             (on-timeout) #f))
        (else (apply throw
                     (cdr outcome)))))))

(define* (make-timeout-assertion-result #:key vc name timeout)
  "Build the failed @code{assertion-result} reported for a timeout.

@var{name} is the timed task, @var{timeout} the seconds it was given and
@var{vc} the client used to report it.  A timeout fails the run exactly like any
other failed assertion."
  (let ((message (format #f "~a timed out after ~a seconds"
                         (if name name "task") timeout)))
    (make-assertion-result 0
                           (with-output-to-string (lambda ()
                                                    (log-fail vc message)))
                           message #f)))
