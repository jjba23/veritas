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

;; ====== Environment variables ======
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
  "Run @var{result} when @var{name} matches @var{restriction}.

The check is case-insensitive, and a restriction counts as a match when it
appears anywhere in @var{name}.  A @code{#f} restriction accepts everything;
one that does not appear gives @code{#f}."
  (if (not restriction) result
      (if (string-contains (string-downcase name)
                           (string-downcase restriction)) result #f)))

(define (drop-keyword-arguments xs)
  "Return @var{xs} with its @code{#:keyword value} pairs removed.

Guile's @code{#:rest} keeps the keyword arguments in the rest list alongside the
positional ones, so a @code{test} or @code{suite} body arrives here carrying its
flags, tags and lifecycle hooks.  Only the values that follow them matter
anywhere else, and a hook is a procedure just like a case is, so the pairs have
to go before the procedures can be picked out."
  (cond
    ((null? xs)
     '())
    ((keyword? (car xs))
     (drop-keyword-arguments (cddr xs)))
    (else (cons (car xs)
                (drop-keyword-arguments (cdr xs))))))

(define (get-cases-for-test xs)
  "Return the assertion cases of a @code{test} body @var{xs}.

Keyword arguments such as @code{#:before} and @code{#:after} are dropped along
with the value that follows them, leaving only the cases."
  (filter procedure?
          (drop-keyword-arguments xs)))

;; ====== Core DSL ======
(define* (with-task-timeout timeout proc on-timeout)
  "Return @var{proc} wrapped in a @var{timeout}, or untouched when @code{#f}.

A positive @var{timeout} runs @var{proc} under @code{call-with-timeout}; if the
task overruns, its fiber is aborted and @var{on-timeout} builds the result that
gets reported."
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
  "Define a test named @var{name}.

The arguments that follow are the assertion cases.  @var{before} and @var{after}
run around them, @var{timeout} caps the whole test in seconds, hooks included,
@var{skip?} drops it from the run, @var{shuffle?} randomizes the case order and
@var{tags} decide which filter may select it.

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
                                                                         indexed-case)))
                                                   ;; Capture when the thunk runs, so the
                                                   ;; #:before hook has already run.
                                                   (lambda ()
                                                     (let ((result-and-output (capture-result-and-output (lambda ()
                                                                                                           
                                                                                                           
                                                                                                           (assertion-proc
                                                                                                            vc)))))
                                                       (make-assertion-result
                                                        idx
                                                        (result-and-output-output
                                                         result-and-output)
                                                        (assoc-ref (result-and-output-result
                                                                    result-and-output)
                                                                   'assertion-name)
                                                        (assoc-ref (result-and-output-result
                                                                    result-and-output)
                                                                   'assertion-successful))))))
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
  "Return the child tests and suites of a @code{suite} body @var{xs}.

Keyword arguments such as @code{#:before-all} and @code{#:after-all} are dropped
along with the value that follows them, leaving only the children."
  (filter procedure?
          (drop-keyword-arguments xs)))

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
  "Define a suite named @var{name} that groups the tests and suites after it.

Children run concurrently unless @var{concurrent?} is @code{#f}, and in shuffled
order unless @var{shuffle?} is @code{#f}.  @var{before-all} and @var{after-all}
run once around them, @var{timeout} caps the whole suite in seconds including
those hooks, @var{skip?} drops it from the run and @var{tags} decide which filter
may select it."
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
  "Define a benchmark named @var{name} that times @var{thunk} over @var{runs} runs.

Statistics come from Guile's @code{statprof}, so benchmarks are reported
separately from tests.  @var{timeout} caps all the runs together in seconds and
@var{skip?} drops the benchmark from the run."

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
  "Return the reporter to use for this run.

@var{dsl-input} wins when it is given (that is the @code{#:reporter} keyword of
the run), otherwise @code{VERITAS_REPORTER} is read.  When neither is set the
answer is @code{#f}, and @code{dispatch-log} falls back to the emoji reporter."
  (let* ((env-var (getenv-or 'VERITAS_REPORTER
                             #:default #f)))
    (cond
      ((not (equal? #f dsl-input))
       dsl-input)
      ((not (equal? #f env-var))
       (string->symbol env-var))
      (else dsl-input))))

(define (%prepare-task-thunks tasks shuffle? vc)
  "Turn @var{tasks} into a flat list of ready-to-run thunks.

Nothing runs here yet: each task is only wrapped so that its output can be
captured later."
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
  "Run @var{regular-thunks} and @var{benchmark-thunks}, returning the raw results.

Regular thunks follow @var{concurrent?} and are spread over fibers, or run one
after another.  Benchmarks always run sequentially: @code{statprof} uses a
process-wide @code{SIGPROF} handler, so running them next to other tasks corrupts
the profile, and a timing measured under CPU contention means nothing anyway."
  (append (if concurrent?
              (evaluate-thunks-concurrently regular-thunks)
              (evaluate-thunks regular-thunks))
          (evaluate-thunks benchmark-thunks)))

(define (%extract-test-results all-results)
  "Flatten the raw @var{all-results} into a list of test results.

A thunk that failed inside a fiber shows up here as a string.  It becomes a
failed assertion, so the run reports the real error and exits non-zero instead of
dying on an \"unknown evaluation structure\"."
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
  "Write the VM @var{coverage-data} to an LCOV trace file.

@var{vc} is used to log where the report was written."
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
  "Print the report for @var{all-results} and exit 1 if anything failed."
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
  "Run @var{tasks}, the top-level tests, suites and benchmarks.

@var{shuffle?} randomizes the order of the tasks and @var{concurrent?} runs them
in parallel fibers.  @var{reporter} names a reporter, otherwise
@code{VERITAS_REPORTER} decides.  @var{with-coverage?} writes an LCOV report and
@var{debug?} prints the raw results."
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
