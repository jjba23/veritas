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

(define-module (veritas veritas)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (veritas reporter)
  #:use-module (veritas assert)
  #:use-module (veritas prelude)
  #:use-module (veritas shuffle)
  #:use-module (veritas model)
  #:use-module (veritas generator)
  #:use-module (statprof)
  #:use-module (system vm coverage)
  #:re-export (generate-integer generate-char
                                generate-list
                                generate-symbol
                                generate-string
                                assert-equal
                                assert-eq
                                assert-eqv
                                assert-true
                                assert-false
                                assert-error
                                assert-no-error
                                assert-approximate
                                assert-not-approximate
                                assert-not-eq
                                assert-not-eqv
                                assert-not-equal
                                assert-contains
                                assert-not-contains
                                assert-matches
                                assert-not-matches
                                assert-raises
                                assert-empty
                                assert-not-empty
                                assert-length
                                assert-in-range
                                assert-one-of
                                is-approximate?
                                proba
                                aequalis
                                idem
                                aequivalens
                                non-aequalis
                                non-idem
                                non-aequivalens
                                verus
                                falsus
                                propinquus?
                                propinquus
                                non-propinquus
                                errat
                                non-errat
                                suscitat
                                continet
                                non-continet
                                congruit
                                non-congruit
                                inanis
                                non-inanis
                                longitudo
                                intra
                                unus-ex
                                evaluate-thunks-concurrently
                                collect-assertion-successful
                                call-with-timeout
                                fisher-yates-shuffle)
  #:export (test veritas-run suite benchmark do-if-string-contained))

;; ====== Parameters of Environment variables ======
(define veritas-only-test
  (make-parameter (getenv-or 'VERITAS_ONLY_TEST
                             #:default #f)))

(define veritas-only-suite
  (make-parameter (getenv-or 'VERITAS_ONLY_SUITE
                             #:default #f)))

(define veritas-only-tag
  (make-parameter (getenv-or 'VERITAS_ONLY_TAG
                             #:default #f)))

(define veritas-run-benchmarks?
  (make-parameter (let ((should-run (getenv-or 'VERITAS_RUN_BENCHMARKS
                                               #:default "true")))
                    (or (equal? "true" should-run)
                        (equal? "1" should-run)))))

(define* (tag-matches? #:key tags restriction)
  "Return @code{#t} when @var{tags} satisfies the tag @var{restriction}.

When @var{restriction} is @code{#f} every set of tags is accepted.  Otherwise
the test runs when any tag contains @var{restriction} (case-insensitively)."
  (or (not restriction)
      (any (lambda (tag)
             (string-contains (string-downcase (format #f "~a" tag))
                              (string-downcase restriction))) tags)))

(define* (do-if-string-contained #:key name restriction result)
  "Execute filtering on @var{name} against an execution substring @var{restriction}.

Performs a case-insensitive validation check. If @var{restriction} is specified
and matched anywhere inside @var{name}, returns @var{result}. If no restriction filter
is configured (@code{#f}), bypasses checks and cleanly forwards @var{result}.

Otherwise, if a restriction is specified but not found inside the name string, returns @code{#f}.

@example
(do-if-string-contained #:name \"User Authentication Unit Spec\"
                        #:restriction \"Auth\"
                        #:result 'run-me)
@result{} run-me
@end example"
  (if (not restriction) result
      (if (string-contains (string-downcase name)
                           (string-downcase restriction)) result #f)))

(define (get-cases-for-test xs)
  "Parse argument list @var{xs} to filter executable assertion case procedures.

Iterates through trailing rest arguments from a @code{test} block initialization.
Safely strips out embedded lifecycle keyword options (such as @code{#:before} or
@code{#:after}) alongside their sequential parameter expressions, extracting a
flat list containing purely test-case execution lambda functions.

@example
(get-cases-for-test (list #:before my-hook (lambda (vc) #t)))
@result{} (#<procedure ...>)
@end example"
  (cond
    ((null? xs)
     '())
    ((equal? #:before (car xs))
     (get-cases-for-test (cdr (cdr xs))))
    ((equal? #:after (car xs))
     (get-cases-for-test (cdr (cdr xs))))
    ((list? xs)
     (filter procedure? xs))
    (else xs)))

;; ====== Core EDSL (embedded domain specific language) ======
(define* (with-task-timeout timeout proc on-timeout)
  "Wrap task procedure @var{proc} with an optional @var{timeout}.

When @var{timeout} is a positive number of seconds the returned procedure runs
@var{proc} under @code{call-with-timeout}: if the task overruns its deadline its
fiber is aborted and @var{on-timeout} is called with the validation client to
build the result to report.  When @var{timeout} is @code{#f} @var{proc} is
returned untouched."
  (if (not timeout) proc
      (lambda (vc)
        (call-with-timeout timeout
                           (lambda ()
                             (proc vc))
                           #:on-timeout (lambda ()
                                          (on-timeout vc))))))

(define* (task-timeout-result vc name timeout)
  "Build the result reported when a test or benchmark exceeds its @var{timeout}."
  (make-pre-test-result name
                        (list (make-timeout-assertion-result #:vc vc
                                                             #:name name
                                                             #:timeout timeout))))

(define* (suite-timeout-result vc name timeout)
  "Build the result reported when a suite exceeds its @var{timeout}."
  (make-suite-result name
                     (list (make-test-result 0 ""
                                             (make-pre-test-result name
                                                                   (list (make-timeout-assertion-result
                                                                          #:vc
                                                                          vc
                                                                          #:name
                                                                          name
                                                                          #:timeout
                                                                          timeout)))))
                     ""))

(define* (test name
               #:key (debug? #f)
               (shuffle? #t)
               (skip? #f)
               (timeout #f)
               (tags '())
               (before #f)
               (after #f)
               #:rest cases)
  "Construct an isolated, executable unit test block named @var{name}.

Defines individual specification properties via keyword parameters:
@itemize
@item @var{debug?}: When @code{#t}, prints raw assertion results dump structures directly to standard out.
@item @var{shuffle?}: Instigates randomized execution ordering of internal cases via Fisher-Yates shuffle hooks.
@item @var{skip?}: Completely aborts blocks from running when set to true.
@item @var{timeout}: A number of seconds the whole test may take, including its @var{before} and @var{after} hooks, before its fiber is aborted and the test is reported as a failure.  @code{#f} (the default) means no deadline.
@item @var{before}: A lifecycle function hook that accepts a validation client (@var{vc}) executed prior to evaluations.
@item @var{after}: A lifecycle function hook that accepts a validation client (@var{vc}) executed post evaluations.
@end itemize

Returns an execution closure capturing the system context, which evaluates all internal
assertion cases sequentially. Standard output written during assertion tracking is safely bound,
captured, and returned inside an encapsulated @code{pre-test-result} data structure.

@example
(test \"Addition Validation\"
      #:before (lambda (vc) (display \"Initializing...\"))
      (assert-equal #:expect 4 #:got (+ 2 2)))
@end example"
  (let* ((func (lambda (vc)
                 (log-test vc name)
                 (let* ((procedure-cases (get-cases-for-test cases))
                        (indexed-cases (map (lambda (case-proc i)
                                              (list i case-proc))
                                            (if shuffle?
                                                (fisher-yates-shuffle
                                                 procedure-cases)
                                                procedure-cases)
                                            (iota (length procedure-cases))))
                        (assertion-thunks (map (lambda (indexed-case)
                                                 (let* ((idx (car indexed-case))
                                                        (assertion-proc (cadr
                                                                         indexed-case))
                                                        (result-and-output (capture-result-and-output (lambda ()
                                                                                                        
                                                                                                        
                                                                                                        (assertion-proc
                                                                                                         vc)))))
                                                   (lambda ()
                                                     (make-assertion-result
                                                      idx
                                                      (result-and-output-output
                                                       result-and-output)
                                                      (assoc-ref (result-and-output-result
                                                                  result-and-output)
                                                                 'assertion-name)
                                                      (assoc-ref (result-and-output-result
                                                                  result-and-output)
                                                                 'assertion-successful)))))
                                               indexed-cases))
                        (_ (if (not before)
                               '()
                               (before vc)))
                        (test-results (evaluate-thunks assertion-thunks))
                        (_ (if (not after)
                               '()
                               (after vc))))
                   
                   (when debug?
                     (newline)
                     (write test-results)
                     (newline))

                   (make-pre-test-result name test-results)))))
    (if skip? #f
        (if (not (tag-matches? #:tags tags
                               #:restriction (veritas-only-tag))) #f
            (do-if-string-contained #:name name
                                    #:restriction (veritas-only-test)
                                    #:result (with-task-timeout timeout func
                                                                (lambda (vc)
                                                                  (task-timeout-result
                                                                   vc name
                                                                   timeout))))))))

(define (get-tests-for-suite xs)
  "Parse trailing list elements @var{xs} to separate executable child components for suites.

Safely filters out global module setup keywords (such as @code{#:before-all} or
@code{#:after-all}) along with their referenced functions, generating a list
containing purely runnable internal @code{test} or macro-generated lambda definitions.

@example
(get-tests-for-suite (list #:before-all setup-proc (lambda (vc) 'test-proc)))
@result{} (#<procedure ...>)
@end example"
  (cond
    ((null? xs)
     '())
    ((equal? #:before-all (car xs))
     (get-tests-for-suite (cdr (cdr xs))))
    ((equal? #:after-all (car xs))
     (get-tests-for-suite (cdr (cdr xs))))
    ((list? xs)
     (filter procedure? xs))
    (else xs)))

(define* (suite name
                #:key (debug? #f)
                (shuffle? #t)
                (skip? #f)
                (timeout #f)
                (tags '())
                (concurrent? #t)
                (before-all #f)
                (after-all #f)
                #:rest tests)
  "Aggregate multiple relevant test definitions inside a structured module container named @var{name}.

Supports high-level execution choreography parameters:
@itemize
@item @var{debug?}: Prints full suite compilation dumps directly to stdout when set to @code{#t}.
@item @var{shuffle?}: Shuffles the sequential order of embedded child tests to isolate side effects.
@item @var{skip?}: Inhibits execution of this suite block completely when evaluated as true.
@item @var{timeout}: A number of seconds the suite may take, including its @var{before-all} and @var{after-all} hooks and every nested test, before its fiber is aborted and the suite is reported as a failure.  @code{#f} (the default) means no deadline.
@item @var{concurrent?}: Controls concurrent scheduling. When @code{#t}, uses asynchronous Guile Fiber blocks to execute tests concurrently.
@item @var{before-all}: A global hook procedure executed once prior to launching any child tests.
@item @var{after-all}: A global hook procedure executed once following the completion of all nested tests.
@end itemize

Returns a composite function wrapper mapping across standard validation configurations.
Captures and isolation-binds all textual test output streams inside a single consolidated @code{suite-result} signature record."
  (let* ((func (lambda (vc)
                 (let* ((procedure-tests (get-tests-for-suite tests))
                        (indexed-tests (map (lambda (test-proc i)
                                              (list i test-proc))
                                            (if shuffle?
                                                (fisher-yates-shuffle
                                                 procedure-tests)
                                                procedure-tests)
                                            (iota (length procedure-tests))))
                        (test-thunks (map (lambda (indexed-test)
                                            (let* ((idx (car indexed-test))
                                                   (test-proc (cadr
                                                               indexed-test))
                                                   (output-port (open-output-string)))
                                              (lambda ()
                                                (let* ((result (with-output-to-port
                                                                output-port
                                                                (lambda ()
                                                                  (test-proc
                                                                   vc))))
                                                       (test-output (get-output-string
                                                                     output-port))
                                                       (_ (close-port
                                                           output-port)))
                                                  (make-test-result idx
                                                   test-output result)))))
                                          indexed-tests))
                        (_ (begin
                             (unless (null? test-thunks)
                               (log-suite vc name))
                             (if (not before-all)
                                 '()
                                 (before-all vc))))
                        (test-results (if concurrent?
                                          (evaluate-thunks-concurrently
                                           test-thunks)
                                          (evaluate-thunks test-thunks)))
                        (_ (if (not after-all)
                               '()
                               (after-all vc)))
                        (final-suite-data (make-suite-result name test-results
                                                             "")))
                   
                   (when debug?
                     (newline)
                     (write final-suite-data)
                     (newline)) final-suite-data))))
    (if skip? #f
        (if (not (tag-matches? #:tags tags
                               #:restriction (veritas-only-tag))) #f
            (do-if-string-contained #:name name
                                    #:restriction (veritas-only-suite)
                                    #:result (with-task-timeout timeout func
                                                                (lambda (vc)
                                                                  (suite-timeout-result
                                                                   vc name
                                                                   timeout))))))))

(define* (benchmark #:key (skip? #f)
                    name
                    runs
                    thunk
                    (timeout #f))
  "Register a micro-benchmarking task block to assess execution latency characteristics.

Accepts configuration settings:
@itemize
@item @var{skip?}: When true, prevents the execution loop of this benchmarking definition.
@item @var{name}: Descriptive tracking label used for diagnostics reporting.
@item @var{runs}: An integer count specifying how many consecutive times to invoke the execution routine.
@item @var{thunk}: A zero-argument lambda wrap containing the code sequence targeted for optimization evaluation.
@item @var{timeout}: A number of seconds all @var{runs} together may take before the benchmarking fiber is aborted and the benchmark is reported as a failure.  @code{#f} (the default) means no deadline.
@end itemize

Utilizes Guile's native statistical profiler engine (@code{statprof}) to capture performance statistics and call-stack diagnostics across loop evaluations. Returns a @code{benchmark-result} record mapping details for each index."

  (let* ((func (lambda (vc)
                 (make-benchmark-result name
                                        (map (lambda (i)
                                               (let* ((sp (with-output-to-string (lambda ()
                                                                                   
                                                                                   
                                                                                   (statprof (lambda ()
                                                                                               
                                                                                               
                                                                                               (thunk)
                                                                                               #f))))))
                                                 `((run-index unquote i)
                                                   (run-details unquote sp))))
                                             (iota runs))))))
    ;; Tag the closure so the runner can recognize benchmarks and execute
    ;; them outside of the concurrent fiber scheduler.  Guile's `statprof'
    ;; relies on a process-global SIGPROF handler, so benchmarking while
    ;; other tasks run concurrently corrupts its profiler state and yields
    ;; broken results.
    (if skip? #f
        (if (veritas-run-benchmarks?)
            (let ((timed (with-task-timeout timeout func
                                            (lambda (vc)
                                              (task-timeout-result vc name
                                                                   timeout)))))
              (set-procedure-property! timed
                                       'veritas-task-type
                                       'benchmark) timed) #f))))

(define (benchmark-task? proc)
  "Return @code{#t} when @var{proc} is a task built by @code{benchmark}."
  (and (procedure? proc)
       (eq? 'benchmark
            (procedure-property proc
                                'veritas-task-type))))

(define (%make-reporter dsl-input)
  "Internal configuration resolution rule to identify and map reporter interfaces.

Dynamically evaluates whether a logger definition override exists inside the environment data profiles
via @code{VERITAS_REPORTER}. If found, converts it to a symbol configuration identifier; otherwise, defaults
to falling back on the explicit runtime variable configuration provided by @var{dsl-input}."
  (let* ((env-var (getenv-or 'VERITAS_REPORTER
                             #:default #f)))
    (cond
      ((not (equal? #f dsl-input))
       dsl-input)
      ((not (equal? #f env-var))
       (string->symbol env-var))
      (else dsl-input))))

(define (%prepare-task-thunks tasks shuffle? vc)
  "Filter and orchestrate user positional @var{tasks} into execution thunks.

Captures all nested trace output blocks written during module tests to standard out
and saves them cleanly back inside structural results configurations. Returns a
sequential flat list containing purely ready-to-run thunk closures."
  (let* ((valid-tasks (filter procedure? tasks))
         (ordered-tasks (if shuffle?
                            (fisher-yates-shuffle valid-tasks) valid-tasks)))
    (map (lambda (task-proc)
           (let* ((output-port (open-output-string)))
             (lambda ()
               (let* ((result (with-output-to-port output-port
                                                   (lambda ()
                                                     (task-proc vc))))
                      (task-output (get-output-string output-port))
                      (_ (close-port output-port)))
                 (cond
                   ((suite-result? result)
                    (set-suite-result-output! result task-output))
                   (else '())) result)))) ordered-tasks)))

(define (%execute-tasks regular-thunks benchmark-thunks concurrent?)
  "Execute compiled @var{regular-thunks} and @var{benchmark-thunks} with explicit VM orchestration.

Regular test/suite @var{regular-thunks} respect @var{concurrent?} by spreading
evaluations over Fibers setups or running single-threaded execution stacks.

Benchmark thunks, however, are *always* run sequentially: Guile's @code{statprof}
uses a process-global @code{SIGPROF} handler, so benchmarking concurrently with
other tasks corrupts the profiler state and produces spurious failures.  Running
them serially also keeps benchmark timings meaningful (no CPU contention from
sibling fibers).

Returns the aggregated raw execution results collection."
  (append (if concurrent?
              (evaluate-thunks-concurrently regular-thunks)
              (evaluate-thunks regular-thunks))
          (evaluate-thunks benchmark-thunks)))

(define (%extract-test-results all-results)
  "Recursively normalize mixed-type results arrays into sequential flat test sheets.

A thunk running inside a fiber may fail; @code{evaluate-thunks-concurrently}
captures the exception and reports it as a string.  Instead of aborting with a
confusing @code{Unknown evaluation structure} error, such entries are surfaced as
failed assertions so the run reports the real failure and exits non-zero."
  (map (lambda (sr)
         (cond
           ((suite-result? sr)
            (suite-result-test-results sr))
           ((pre-test-result? sr)
            sr)
           ((benchmark-result? sr)
            '())
           ((string? sr)
            (make-assertion-result 0 ""
                                   (format #f "task raised an error: ~a" sr)
                                   #f))
           (else (make-assertion-result 0 ""
                                        (format #f
                                         "Unknown evaluation structure parsed: ~a"
                                         sr) #f)))) all-results))

(define (%write-coverage-report coverage-data vc)
  "Write internal compiled VM @var{coverage-data} records safely into an LCOV trace file."
  (let* ((t (gettimeofday))
         (report-name (string-append "reports/lcov/coverage-"
                                     (format #f "~a-~a"
                                             (car t)
                                             (cdr t)) ".info")))
    (call-with-output-file report-name
      (lambda (port)
        (coverage-data->lcov coverage-data port)))
    (log-info vc
              (format #f "Code coverage report written to ~a" report-name))))

(define (%process-execution-lifecycle vc all-results task-count start-time
                                      debug?)
  "Gather execution metrics, print logger telemetry dashboards, and handle failure states."
  (let* ((test-results (%extract-test-results all-results))
         (assertion-results (collect-assertion-successful test-results))
         (end-time (gettimeofday))
         (seconds-elapsed (- (car end-time)
                             (car start-time)))
         (microseconds-elapsed (- (cdr end-time)
                                  (cdr start-time))))
    
    (display-all-results vc all-results)
    (log-info vc
              (format #f "ran ~a tasks in: ~a seconds, ~a microseconds"
                      task-count seconds-elapsed microseconds-elapsed))

    (when debug?
      (log-debug vc
                 (format #f "assertion-results: ~a" assertion-results)))

    (when (member #f assertion-results)
      (log-error vc "\naborting due to error in test case...\n")
      (exit 1))))

;; ====== Entry-point function ======
(define* (veritas-run #:key (shuffle? #t)
                      (concurrent? #t)
                      (debug? #f)
                      (reporter #f)
                      (with-coverage? #f)
                      #:rest tasks)
  "Top-level execution runtime wrapper engine designed to orchestrate test execution runs.

Evaluates positional task variables sequentially or concurrently, providing orchestration hooks:
@itemize
@item @var{shuffle?}: Shuffles top-level block entry components randomly before runner routing.
@item @var{concurrent?}: Runs distinct suite closures asynchronously using lightweight concurrent fibers.
@item @var{debug?}: Provides developer trace visibility parameters to stdout streams.
@item @var{reporter}: Selects the designated printing formatter design structure.
@item @var{with-coverage?}: Whether to measure test code coverage and produce an LCOV report.
@end itemize"
  (let* ((start-time (gettimeofday))
         (made-reporter (%make-reporter reporter))
         (vc (list (cons 'reporter made-reporter)))
         (valid-tasks (filter procedure? tasks))
         ;; Benchmarks are kept apart from regular tasks so they can be run
         ;; sequentially (see %execute-tasks).
         (benchmark-tasks (filter benchmark-task? valid-tasks))
         (regular-tasks (remove benchmark-task? valid-tasks))
         (regular-thunks (%prepare-task-thunks regular-tasks shuffle? vc))
         (benchmark-thunks (%prepare-task-thunks benchmark-tasks shuffle? vc))
         (task-count (+ (length regular-thunks)
                        (length benchmark-thunks))))
    (catch #t
           (lambda ()
             (mkdir "reports")
             (mkdir "reports/lcov"))
           (lambda (key . args)
             #f))
    (call-with-values (lambda ()
                        (if with-coverage?
                            (with-code-coverage (lambda ()
                                                  (%execute-tasks
                                                   regular-thunks
                                                   benchmark-thunks
                                                   concurrent?)))
                            ;; Force a two-value return matching the signature
                            (values #f
                                    (%execute-tasks regular-thunks
                                                    benchmark-thunks
                                                    concurrent?))))
                      (lambda (coverage-data all-results)
                        (when (and with-coverage? coverage-data)
                          (%write-coverage-report coverage-data vc))

                        (%process-execution-lifecycle vc all-results
                                                      task-count start-time
                                                      debug?)

                        all-results))))
