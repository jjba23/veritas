;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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
                                         empty-result-counts
                                         add-result-counts
                                         result-counts
                                         result-counts-summary
                                         call-with-timeout
                                         call-with-fiber-safe-wind
                                         make-timeout-assertion-result
                                         current-collector
                                         make-collector
                                         collector?
                                         collect-in!
                                         collector-items
                                         deduplicate-values
                                         procedure-required-arity
                                         nullary-procedure?
                                         flatten-cases))

(define-syntax-rule (evaluate-thunks-concurrently thunks)
  "Run @var{thunks} concurrently in fibers and collect what they return.

Blocks until every thunk has reported back through a shared channel.  A thunk
that raises sends a formatted error string instead of a value, so one failure
cannot hang the run.  Results come back in the order of @var{thunks}, not in
completion order: two runs of one suite have to report the same lines, and
which fiber finished first is not part of a result.

@example
(evaluate-thunks-concurrently
 (list (lambda () (sleep 1) 'delayed)
       (lambda () 'instant)))
@result{} (delayed instant)
@end example"
  (let ((channel (make-channel))
        (pending thunks))
    (run-fibers (lambda ()
                  (let spawn ((rest pending) (position 0))
                    (unless (null? rest)
                      (spawn-fiber (lambda ()
                                     ;; Report the exception instead of letting
                                     ;; the fiber die without a word, and say
                                     ;; where the answer belongs.
                                     (put-message channel
                                                  (cons position
                                                        (catch #t
                                                               (car rest)
                                                               (lambda (key . args)
                                                                 (format #f
                                                                         "~a: ~a"
                                                                         key
                                                                         args)))))))
                      (spawn (cdr rest) (+ position 1))))

                  ;; Consumed inside run-fibers: this drain blocks the main
                  ;; fiber until every thunk has reported, so no #:drain? is
                  ;; needed.  The position each answer carries is what puts the
                  ;; results back in the order they were asked for.
                  (let drain ((left (length pending)) (collected '()))
                    (if (zero? left)
                        (map cdr (sort collected
                                       (lambda (a b) (< (car a) (car b)))))
                        (drain (- left 1)
                               (cons (get-message channel)
                                     collected))))))))

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

;; ====== Counting what a run did ======
;;
;; The totals of a run are read off the result tree rather than bumped by a
;; counter while it runs: the tree is what the report is printed from, so the
;; numbers and the report cannot disagree.

(define empty-result-counts
  '((files . 0) (suites . 0) (tests . 0) (assertions . 0) (flaky . 0)))

(define (count-of-kind kind n)
  "Return a count alist holding @var{n} under @var{kind} and zero elsewhere."
  (map (lambda (key)
         (cons key (if (eq? key kind) n 0)))
       '(files suites tests assertions flaky)))

(define (add-result-counts a b)
  "Return the sum of the count alists @var{a} and @var{b}."
  (map (lambda (key)
         (cons key (+ (assoc-ref a key) (assoc-ref b key))))
       '(files suites tests assertions flaky)))

(define (flaky-test-result? result)
  "Return @code{#t} when @var{result} passed only after a retry.

A test that failed and then passed is a pass, and also the one thing a run
should report separately: it is the test that will fail again, and a total that
counts it as a clean pass is a total that hides it."
  (and (pre-test-result? result)
       (> (pre-test-result-attempts result) 1)
       (let ((outcomes (collect-assertion-successful
                        (pre-test-result-results result))))
         (and (pair? outcomes)
              (every (lambda (outcome) outcome) outcomes)))))

(define (result-counts* result)
  "Return the counts of @var{result} itself, without a file of its own."
  (cond
    ((unspecified? result)
     empty-result-counts)
    ((assertion-result? result)
     (count-of-kind 'assertions 1))
    ((pre-test-result? result)
     (add-result-counts
      (add-result-counts (count-of-kind 'tests 1)
                         (result-counts* (pre-test-result-results result)))
      (count-of-kind 'flaky (if (flaky-test-result? result) 1 0))))
    ((suite-result? result)
     (add-result-counts (count-of-kind 'suites 1)
                        (result-counts* (suite-result-test-results result))))
    ;; A test result wraps one child result - the assertions of a test or the
    ;; child suites of a suite - and counts what it wraps, not itself.
    ((test-result? result)
     (result-counts* (test-result-results result)))
    ((benchmark-result? result)
     empty-result-counts)
    ((list? result)
     (fold add-result-counts empty-result-counts
           (map result-counts* result)))
    ((equal? #t result)
     empty-result-counts)
    ;; A task that raised arrives as the error string; the failure is reported
    ;; on its own, and there is no suite, test or assertion to count.
    ((string? result)
     empty-result-counts)
    (else (error (format #f
                  "could not count results of unknown type: ~a" result)))))

(define* (result-counts result #:key (files 0))
  "Return the files, suites, tests and assertions @var{result} records.

@var{result} is a result record or a list of them, at any depth: an assertion
counts as one assertion, a test as one test, a suite as one suite, and a suite
or a test that holds further results counts what it holds as well.  @var{files}
is how many test files the results stand for - one for the run of a single file -
and is added to the @code{files} count.

A benchmark measures instead of asserting, and a task that raised arrives as the
error string, so neither contributes a test or an assertion of its own.

@example
(result-counts (list a-suite a-test) #:files 1)
@result{} ((files . 1) (suites . 1) (tests . 3) (assertions . 7))
@end example"
  (add-result-counts (result-counts* result)
                     (count-of-kind 'files files)))

(define (count-noun n noun)
  "Return @var{n} with @var{noun}, plural unless @var{n} is one."
  (format #f "~a ~a~a" n noun (if (= 1 n) "" "s")))

(define (result-counts-summary counts)
  "Return @var{counts} as one readable line.

@example
(result-counts-summary '((files . 2) (suites . 3) (tests . 12) (assertions . 48)))
@result{} \"2 files, 3 suites, 12 tests and 48 assertions\"
@end example"
  (let ((flaky (or (assoc-ref counts 'flaky) 0)))
    (format #f "~a, ~a, ~a and ~a~a"
            (count-noun (assoc-ref counts 'files) "file")
            (count-noun (assoc-ref counts 'suites) "suite")
            (count-noun (assoc-ref counts 'tests) "test")
            (count-noun (assoc-ref counts 'assertions) "assertion")
            (if (> flaky 0)
                (format #f "; ~a flaky" (count-noun flaky "test"))
                ""))))

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

(define (display-test-results vc test-results)
  "Print the output and inner assertions of each record in @var{test-results}.

Walks into suites as well, including a suite that arrives wrapped in a
@code{test-result}.  @var{vc} is the reporter client, used to report a child
whose thunk raised - an exception arrives here as a string, and dropping it
would hide a failure of a suite's own child.

This has to stay a procedure.  As a macro it would re-expand the recursive call
forever and eat memory instead of printing the report."
  (for-each (lambda (x)
              (cond
                ((unspecified? x)
                 '())
                ((suite-result? x)
                 (begin
                   (display (suite-result-output x))
                   (display-test-results vc
                                         (suite-result-test-results x)))
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
                        (display-test-results vc
                                              (suite-result-test-results
                                               inner)))
                       ;; A test whose thunk raised reports its exception as a
                       ;; string.  Say so instead of dropping the failing child
                       ;; of a suite without a word.
                       ((string? inner)
                        (log-error vc
                                   (format #f "task raised an error: ~a"
                                           inner)))
                       (else '()))))
                 '())
                ((string? x)
                 (log-error vc
                            (format #f "task raised an error: ~a" x))
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
                   (display-test-results vc
                                         (suite-result-test-results x))))
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

;;; ====== What a body builds ======
;;
;; A test or suite body is a sequence of expressions, not a list of cases, and
;; only the value of its last expression survives:
;;
;;   (let ((x 2))
;;     (assert-equal #:expect 2 #:got x)   ; built and thrown away
;;     (assert-true (even? x)))            ; the only one the runner sees
;;
;; Bodies are therefore evaluated with a collector bound, and every case -
;; assertion, test, suite or benchmark - is recorded as it is built.  A body
;; form that is just an assertion still works, through its value: registration
;; and the value are merged, by identity, when the form is expanded.
;;
;; An assertion that the body itself runs (through `value', `ok?' or a case
;; lambda, say) is marked as consumed, so using an assertion as the input of
;; another one does not turn it into a case of its own.

(define current-collector
  ;; The collector of the body being evaluated, or #f outside one.
  (make-parameter #f))

(define (make-collector)
  "Return a fresh collector: a mutable list head of recorded items."
  (cons 'veritas-collector '()))

(define (collector? value)
  "Return @code{#t} when @var{value} is a collector made by @code{make-collector}."
  (and (pair? value)
       (eq? 'veritas-collector (car value))))

(define (collect-in! item)
  "Record @var{item} in the active collector, and return @var{item}.

Does nothing else when no collector is bound, which is what building a case
outside a body - at load time, or inside an assertion that runs later - looks
like.  @code{#f} is never recorded: a case a filter dropped is not a case."
  (let ((collector (current-collector)))
    (when (and item
               (collector? collector))
      (set-cdr! collector (cons item (cdr collector))))
    item))

(define (collector-items collector)
  "Return what @var{collector} recorded, in the order the items were built."
  (if (not (collector? collector)) '()
      (reverse (cdr collector))))

(define (deduplicate-values values)
  "Return @var{values} without the entries that are the same object as an earlier one.

Identity, not equality, decides: the same case built twice is two cases, while
the same case both recorded and returned by one form is one."
  (let loop ((rest values) (seen '()) (out '()))
    (cond
      ((null? rest) (reverse out))
      ((memq (car rest) seen) (loop (cdr rest) seen out))
      (else (loop (cdr rest) (cons (car rest) seen) (cons (car rest) out))))))

(define %fiber-safe-wind
  ;; `(fibers scheduler)' exports `dynamic-wind*', which brackets a form like
  ;; `dynamic-wind' but leaves the guards alone while a fiber is suspended.
  ;; Older Fibers versions do not have it, so plain `dynamic-wind' is the
  ;; fallback: it runs the guards across a suspension, which is wrong, but it
  ;; never stops a module from loading.
  (false-if-exception
   (module-ref (resolve-module '(fibers scheduler)) 'dynamic-wind*)))

(define (call-with-fiber-safe-wind before thunk after)
  "Call @var{thunk} between @var{before} and @var{after}, safely under fibers.

Fibers suspends a task by unwinding to the scheduler and resumes it by winding
back in, so a plain @code{dynamic-wind} runs its guards again every time the
task yields - and a port operation is enough to yield.  A guard that has a side
effect - a module binding swapped, a directory removed, a VM engine changed -
would then fire in the middle of the thunk.  @code{dynamic-wind*} from
@code{(fibers scheduler)} skips the guards during those round trips and runs
them when the extent is really entered and really left, which is what every
bracketing helper here wants.

@example
(call-with-fiber-safe-wind
 (lambda () (module-set! module 'now 0))
 (lambda () (read-the-clock))
 (lambda () (module-set! module 'now original)))
@end example"
  ((or %fiber-safe-wind dynamic-wind) before thunk after))

(define (procedure-required-arity proc)
  "Return how many arguments @var{proc} needs, or @code{#f} when that is unknown.

Guile reports a procedure's arity as the list @code{(required optional rest?)},
or as several values on older versions; both are read here."
  (and (procedure? proc)
       (catch #t
              (lambda ()
                (call-with-values (lambda ()
                                    (procedure-minimum-arity proc))
                                  (lambda (first . rest)
                                    (if (pair? first)
                                        (car first)
                                        first))))
              (lambda (key . args) #f))))

(define (nullary-procedure? proc)
  "Return @code{#t} when @var{proc} can be called with no arguments at all.

A body form is wrapped in such a thunk by the DSL, and a thunk of this shape is
therefore a body still to be evaluated rather than a case to run."
  (eqv? 0 (procedure-required-arity proc)))

(define* (flatten-cases value case? expand #:optional (depth 0))
  "Return the cases @var{value} stands for, in order, leaving out what is not one.

@var{case?} recognises a case, @var{expand} evaluates a thunk-shaped body form,
lists (of any depth) are walked, and anything else - an unspecified value, a
number, a nested task in a place that wants assertions - contributes nothing.
@var{depth} bounds the recursion, so a thunk that keeps returning thunks is a
value that is ignored rather than a run that never ends."
  (cond
    ((> depth 64) '())
    ((null? value) '())
    ((pair? value)
     (if (list? value)
         (append-map (lambda (element)
                       (flatten-cases element case? expand (1+ depth)))
                     value)
         '()))
    ((case? value) (list value))
    ((nullary-procedure? value) (expand value (1+ depth)))
    (else '())))

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
