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
  #:use-module (statprof)
  #:re-export (assert-equal assert-eqv
                            assert-true
                            assert-false
                            assert-error
                            assert-no-error
                            assert-approximate
                            assert-not-approximate
                            assert-not-eq
                            assert-not-eqv
                            assert-not-equal
                            is-approximate?
                            evaluate-thunks-concurrently
                            collect-assertion-successful
                            fisher-yates-shuffle)
  #:export (test veritas-run suite benchmark do-if-string-contained))

;; ====== Parameters of Environment variables ======
(define veritas-only-test
  (make-parameter (getenv-or 'VERITAS_ONLY_TEST
                             #:default #f)))

(define veritas-only-suite
  (make-parameter (getenv-or 'VERITAS_ONLY_SUITE
                             #:default #f)))

(define veritas-run-benchmarks?
  (make-parameter (let ((should-run (getenv-or 'VERITAS_RUN_BENCHMARKS
                                               #:default "true")))
                    (or (equal? "true" should-run)
                        (equal? "1" should-run)))))

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
(define* (test name
               #:key (debug? #f)
               (shuffle? #t)
               (skip? #f)
               (before #f)
               (after #f)
               #:rest cases)
  "Construct an isolated, executable unit test block named @var{name}.

Defines individual specification properties via keyword parameters:
@itemize
@item @var{debug?}: When @code{#t}, prints raw assertion results dump structures directly to standard out.
@item @var{shuffle?}: Instigates randomized execution ordering of internal cases via Fisher-Yates shuffle hooks.
@item @var{skip?}: Completely aborts blocks from running when set to true.
@item @var{before}: A lifecycle function hook that accepts a validation client (@var{vc}) executed prior to evaluations.
@item @var{after}: A lifecycle function hook that accepts a validation client (@var{vc}) executed post evaluations.
@end itemize

Returns an execution closure capturing the system context, which evaluates all internal
assertion cases sequentially. Standard output written during assertion tracking is safely bound,
captured, and returned inside an encapsulated @code{pre-test-result} data structure.

@example
(test \"Addition Validation\"
      #:before (lambda (vc) (display \"Initializing...\"))
      (lambda (vc) (assert-equal vc 4 (+ 2 2))))
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
        (do-if-string-contained #:name name
                                #:restriction (veritas-only-test)
                                #:result func))))

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
        (do-if-string-contained #:name name
                                #:restriction (veritas-only-suite)
                                #:result func))))

(define* (benchmark #:key (skip? #f) name runs thunk)
  "Register a micro-benchmarking task block to assess execution latency characteristics.

Accepts configuration settings:
@itemize
@item @var{skip?}: When true, prevents the execution loop of this benchmarking definition.
@item @var{name}: Descriptive tracking label used for diagnostics reporting.
@item @var{runs}: An integer count specifying how many consecutive times to invoke the execution routine.
@item @var{thunk}: A zero-argument lambda wrap containing the code sequence targeted for optimization evaluation.
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
    (if skip? #f
        (if (veritas-run-benchmarks?) func #f))))

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

;; ====== Entry-point function ======
(define* (veritas-run #:key (shuffle? #t)
                      (concurrent? #t)
                      (debug? #f)
                      (reporter #f)
                      #:rest tasks)
  "Top-level execution runtime wrapper engine designed to orchestrate test execution runs.

Evaluates positional task variables sequentially or concurrently, providing orchestration hooks:
@itemize
@item @var{shuffle?}: Shuffles top-level block entry components randomly before runner routing.
@item @var{concurrent?}: Runs distinct suite closures asynchronously using lightweight concurrent fibers.
@item @var{debug?}: Provides developer trace visibility parameters to stdout streams.
@item @var{reporter}: Selects the designated printing formatter design structure (e.g. @code{'emoji-reporter}, @code{'json-reporter}, @code{'monotone-reporter}).
@end itemize

Measures elapsed microsecond time spans accurately using system clocks. Upon execution loop completion,
recursively scans assertion lists; if any assertion validation indicates a failure (@code{#f}),
intercepts execution control and terminates the system process immediately by invoking an explicit system exit command (@code{exit 1})."

  (let* ((start-test-time (gettimeofday))
         (start-seconds (car start-test-time))
         (start-microseconds (cdr start-test-time))
         (valid-tasks (filter procedure? tasks))
         (ordered-tasks (if shuffle?
                            (fisher-yates-shuffle valid-tasks) valid-tasks))
         (made-reporter (%make-reporter reporter))
         (vc (list (cons 'reporter made-reporter)))
         (task-thunks (map (lambda (task-proc)
                             (let* ((output-port (open-output-string)))
                               (lambda ()
                                 (let* ((result (with-output-to-port
                                                 output-port
                                                 (lambda ()
                                                   (task-proc vc))))
                                        (task-output (get-output-string
                                                      output-port))
                                        (_ (close-port output-port)))
                                   (cond
                                     ((suite-result? result)
                                      (set-suite-result-output! result
                                                                task-output))
                                     (else '()))

                                   result)))) ordered-tasks))
         (all-results (if concurrent?
                          (evaluate-thunks-concurrently task-thunks)
                          (evaluate-thunks task-thunks)))
         (test-results (map (lambda (sr)
                              (cond
                                ((suite-result? sr)
                                 (suite-result-test-results sr))
                                ((pre-test-result? sr)
                                 sr)
                                ((benchmark-result? sr)
                                 '())
                                (else (error (format #f
                                              "could not read test results from unknown type: ~a"
                                              sr))))) all-results))
         (assertion-results (collect-assertion-successful test-results))
         (end-test-time (gettimeofday))
         (end-seconds (car end-test-time))
         (end-microseconds (cdr end-test-time))
         (seconds-elapsed (- end-seconds start-seconds))
         (microseconds-elapsed (- end-microseconds start-microseconds)))
    
    (display-all-results vc all-results)
    (log-info vc
              (format #f "ran ~a tasks in: ~a seconds, ~a microseconds"
                      (length valid-tasks) seconds-elapsed
                      microseconds-elapsed))
    (when debug?
      (log-debug vc
                 (format #f "assertion-results: ~a" assertion-results)))

    (when (member #f assertion-results)
      (log-error vc "\naborting due to error in test case...\n")
      (exit 1))

    all-results))
