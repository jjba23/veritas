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
  "Evaluate a list of @var{thunks} concurrently using Guile Fibers.

Each zero-argument procedure in the list @var{thunks} is spawned inside its own
concurrent fiber thread. This macro blocks the execution of the calling fiber
until all spawned thunks have reported their results back through a shared channel.

Exceptions raised within individual fibers are caught safely to avoid hanging or
silent termination, returning a formatted error string instead.

Note: The output list order corresponds to the exact completion order of the
fibers, meaning it is non-deterministic.

@example
(evaluate-thunks-concurrently
 (list (lambda () (sleep 1) 'delayed)
       (lambda () 'instant)))
@result{} (instant delayed)
@end example"
  (let ((channel
          (make-channel)))
    (run-fibers (lambda ()
                  ;; Spawn producers
                  (for-each (lambda (func)
                              (spawn-fiber (lambda ()
                                             ;; Catch exceptions so the fiber doesn't die silently
                                             (catch #t
                                                    (lambda ()
                                                      (let ((result (func)))
                                                        (put-message channel
                                                                     result)))
                                                    (lambda (key . args)
                                                      ;; Send the error down the channel to prevent hanging
                                                      (put-message channel
                                                                   (format #f
                                                                    "~a: ~a"
                                                                    key args)))))))
                            thunks)

                  ;; Consume messages INSIDE the run-fibers block
                  ;;
                  ;; Note: #:drain? #t is not needed because the `map` loop implicitly
                  ;; blocks the main fiber until all thunks have reported back.
                  (map (lambda (_)
                         (get-message channel)) thunks)))))

(define-syntax-rule (evaluate-thunks thunks)
  "Evaluate a list of @var{thunks} sequentially.

Applies each zero-argument procedure in the list @var{thunks} one after another
in a synchronous manner.

Returns a list containing the evaluation results of each thunk preserving the original
order of the input list.

@example
(evaluate-thunks (list (lambda () (+ 1 1)) (lambda () (* 2 3))))
@result{} (2 6)
@end example"
  (map (lambda (t)
         (apply t
                '())) thunks))

(define (flatten-list xs)
  "Recursively flatten a nested list structure @var{xs}.

Traverses the given structure and flattens all nested sub-lists into a continuous,
one-dimensional list of elements. If @var{xs} is empty, returns an empty list. If
it is a non-list atom, returns it wrapped inside a single-element list.

@example
(flatten-list '((1 2) (3 (4 5)) 6))
@result{} (1 2 3 4 5 6)
@end example"
  (cond
    ((null? xs)
     '())
    ((pair? xs)
     (append (flatten-list (car xs))
             (flatten-list (cdr xs))))
    (else (list xs))))

(define (collect-assertion-successful* xs)
  "Recursive procedure to extract success values from test data @var{xs}.

Dispatches dynamically on the type of @var{xs} to look for assertion results:
@itemize
@item If it is an assertion result, extracts its success state boolean.
@item If it is a pre-test or test result, recursively extracts outcomes from internal fields.
@item Nested lists are mapped over recursively.
@item Unspecified values, benchmark results, and explicit @code{#t} values return empty lists.
@end itemize

Raises an error if an unknown object structure is encountered."
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
  "Collect and flatten all assertion success statuses found in @var{xs}.

Processes the arbitrary test result tree or list structure @var{xs}, extracts all underlying
assertion outcomes, and returns a flat list containing only booleans indicating success
(@code{#t}) or failure (@code{#f}).

@example
(collect-assertion-successful my-test-suite-record)
@result{} (#t #t #f #t)
@end example"
  (flatten-list (collect-assertion-successful* xs)))

(define (display-assertion-results assertion-results)
  "Display the captured text outputs for a list of @var{assertion-results}.

Iterates over @var{assertion-results}. For each item, validates that it is an assertion record
and dumps its tracked standard output directly to the current output port. Errors during processing
are safely intercepted and logged to stdout without crashing the reporter framework."
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
  "Display outputs and nested assertions for a list of @var{test-results}.

Iterates through @var{test-results}. If an item is a valid test result, it prints the
captured test output, and then processes its internal pre-test assertion data structures
by forwarding them to @code{display-assertion-results}. Nested suites (including a
suite nested inside a suite, which arrives wrapped in a @code{test-result}) are
recursed into so their tests are reported as well.

This is a procedure, not a macro: the recursion happens while reporting at run
time.  Expressing it as a macro would expand the recursive call again and again
and never terminate, exhausting memory instead of printing the report."
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
  "Lookup environment variable @var{var}, returning @var{default} if absent.

Converts the symbol @var{var} into a string to query the operating system's environment.
If the variable does not exist, or if an exception occurs during lookup, the specified
@var{default} keyword value is returned.

@example
(getenv-or 'GUILE_LOAD_PATH #:default \"/usr/share/guile\")
@result{} \"/usr/share/guile\"
@end example"
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
  "Execute @var{thunk}, capturing both its return value and standard output.

Dynamically binds the standard output port to a string port while running @var{thunk}.
Returns a custom @code{result-and-output} object record packing the thunk's evaluation value
and any textual stdout written during runtime. Closes the internal string port safely upon completion.

@example
(capture-result-and-output (lambda () (display \"In execution...\") 100))
@result{} #<result-and-output result: 100 output: \"In execution...\">
@end example"
  (let* ((output-port (open-output-string))
         (result (with-output-to-port output-port thunk))
         (the-output (get-output-string output-port))
         (_ (close-port output-port)))
    (make-result-and-output result the-output)))

;;; ====== Timeouts ======
;;
;; A timeout turns a hanging test into a reported failure while making sure the
;; concurrent machine that ran it does not keep running in the background.  The
;; work happens in its own fiber; when the deadline passes a watchdog aborts
;; that fiber, which unwinds it back to the prompt installed around the task.
;;
;; Two details are worth spelling out.
;;
;; First, the abort cannot be triggered from a fiber: a system async marked on
;; the scheduler's kernel thread runs at the next safe point of whichever fiber
;; is running, so a fiber that marks itself would only interrupt itself.  The
;; watchdog is therefore a regular kernel thread, and the async it marks
;; carries the task's prompt tag; `%active-timeout' makes the async a no-op
;; unless it lands inside the timed task, so preemption of the other fibers in
;; the same scheduler is never disturbed.
;;
;; Second, the caller does not wait on the task itself but on a channel with a
;; deadline.  A task blocked in a syscall or suspended on a channel is never
;; resumed by the abort, so waiting on it would reintroduce the very hang the
;; timeout is meant to remove.

(define %active-timeout
  ;; Bound (via `parameterize') to the prompt tag of the timed task while that
  ;; task runs.  This is what makes the asynchronous abort safe.
  (make-parameter #f))

(define %timeout-wait-interval-microseconds
  ;; How often the watchdog checks the clock (and whether the task is done)
  ;; while the deadline has not passed yet.  Guile threads have no reliable
  ;; timed wait, so the deadline is polled for; the caller enforces its own
  ;; deadline precisely and independently, this only bounds when the runaway
  ;; fiber is aborted.
  5000)

(define %timeout-wait-interval-milliseconds
  5)

(define %timeout-scan-interval-microseconds
  ;; How often the watchdog retries the abort while the task has not finished.
  ;; The retry matters when the first async is delivered to a sibling fiber
  ;; (the timed one being suspended at the time).
  10000)

(define %timeout-scan-attempts
  ;; Upper bound on those retries, so a task that never runs again cannot keep
  ;; a watchdog thread alive forever.
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
  "Build a failing @code{assertion-result} describing a timeout.

@var{name} is the timed task, @var{timeout} the seconds it was given, and
@var{vc} the reporter client used to format the failure.  The result is
reported like any other failed assertion, so a run in which a task exceeded its
@code{#:timeout} exits with a non-zero status."
  (let ((message (format #f "~a timed out after ~a seconds"
                         (if name name "task") timeout)))
    (make-assertion-result 0
                           (with-output-to-string (lambda ()
                                                    (log-fail vc message)))
                           message #f)))
