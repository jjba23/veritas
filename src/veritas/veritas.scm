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
  #:use-module (veritas coverage)
  #:use-module (statprof)
  #:re-export (generate-integer generate-char
                                generate-list
                                generate-symbol
                                generate-string
                                make-assertion
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
                                fisher-yates-shuffle
                                fresh-seed)
  #:export (test suite %test %suite
                  split-keyword-arguments
                  defer-body-forms
                  veritas-run benchmark do-if-string-contained
                  current-seed
                  seeded-shuffle
                  failed-test-names
                  failed-test-wanted?
                  read-last-failed
                  write-last-failed!
                  veritas-only-failed
                  veritas-last-failed-path
                  write-last-failed-names!
                  merge-failed-names
                  veritas-rerun-all-marker
                  veritas-current-file
                  veritas-junit-data-path
                  veritas-coverage-data-path
                  veritas-result-counts
                  veritas-result-counts-data-path
                  write-result-counts
                  read-result-counts))

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

(define (seed-from value)
  "Coerce @var{value} into a seed, or return @code{#f} when there is none.

A keyword argument arrives as a number and an environment variable as a string,
so both are accepted."
  (cond
    ((not value) #f)
    ((number? value) (inexact->exact (truncate value)))
    ((string? value) (let ((n (string->number value)))
                       (and n (inexact->exact (truncate n)))))
    (else #f)))

(define veritas-seed
  (make-parameter (seed-from (getenv-or 'VERITAS_SEED
                                        #:default #f))))

(define* (current-seed #:key seed)
  "Return the seed this run shuffles with, choosing one when none was set.

@var{seed} (a number or a numeric string) wins when given, otherwise the value
of @code{VERITAS_SEED} is used, and when neither is set a fresh seed is drawn
from the platform.  The chosen seed is remembered, so the order the runner
gives the test files and every order chosen here come from one seed, and the run
can be replayed by setting @code{VERITAS_SEED} to the seed that was reported.

@example
(current-seed #:seed 7)
@result{} 7
@end example"
  (let ((chosen (or (seed-from seed)
                    (veritas-seed)
                    (fresh-seed))))
    (veritas-seed chosen)
    chosen))

(define (seeded-shuffle xs)
  "Return @var{xs} in the order this run's seed prescribes.

Both the runner, which shuffles test files, and @code{veritas-run}, which
shuffles tasks, tests and cases, shuffle through this, so a single seed
reproduces a whole run."
  (fisher-yates-shuffle xs
                        #:seed (current-seed)))

;; ====== Rerunning what failed ======
;;
;; The names that failed are written next to the other reports, and read back
;; when VERITAS_ONLY_FAILED is set, so a failing test can be rerun on its own.

(define veritas-last-failed-path
  (make-parameter (getenv-or 'VERITAS_LAST_FAILED_PATH
                             #:default "reports/last-failed.scm")))

(define veritas-rerun-all-marker
  "#all")

(define* (read-last-failed #:key (path (veritas-last-failed-path)))
  "Return the test names recorded as failing at @var{path}, or @code{#f}.

@var{path} defaults to the record of the whole run.  @code{#f} means there is no
record of a previous run: the file is missing or unreadable, and nothing is
filtered."
  (if (file-exists? path)
      (catch #t
             (lambda ()
               (call-with-input-file path read))
             (lambda (key . args) #f))
      #f))

(define veritas-only-failed
  (make-parameter (let ((flag (getenv-or 'VERITAS_ONLY_FAILED
                                         #:default #f)))
                    (and (or (equal? "true" flag)
                             (equal? "1" flag))
                         (read-last-failed)))))

;; Set (in a worker process, after the fork) when the run is one of several and
;; the report is to be handed to the process that merges them.  It holds the
;; path to write the JUnit data to instead of the document.

;; The test file the current run is reading, as a path from the project root.
;; `veritas-run' is called from inside a loaded test file, so the runner tells
;; it which file that was; a spec called directly leaves it unset.  Every test
;; result records it, which is what lets a report point at a file.

(define veritas-current-file
  (make-parameter #f))

(define veritas-junit-data-path
  (make-parameter #f))

;; Set (in a worker process, after the fork) when the run is one of several and
;; its coverage is to be merged by the process that runs the other files.  It
;; holds the path of an LCOV fragment instead of the report of the whole run.

(define veritas-coverage-data-path
  (make-parameter #f))

;; Set (in a worker process, after the fork) when the run is one of several and
;; its totals are to be handed to the process that runs the other files.  It
;; holds the path the counts are written to instead of being added to the tally
;; of this process.

(define veritas-result-counts-data-path
  (make-parameter #f))

;; The files, suites, tests and assertions of every file this process has run so
;; far.  Every run adds its own counts here, so the runner can print one global
;; total once the last file is in.

(define veritas-result-counts
  (make-parameter empty-result-counts))

(define* (write-result-counts counts
                               #:key (path "reports/counts.scm"))
  "Write @var{counts} to @var{path} so another process can add them up.

An alist is plain Scheme data, so @code{read} restores it exactly: this is how a
worker process hands its totals to the process that runs the other files.
Returns @var{path}."
  (ensure-parent-directories path)
  (call-with-output-file path
    (lambda (port)
      (write counts port)
      (newline port)))
  path)

(define (read-result-counts path)
  "Return the counts recorded at @var{path}, or @code{#f}.

@code{#f} means there is nothing to add up: the file is missing, unreadable or
not the alist @code{write-result-counts} writes."
  (if (file-exists? path)
      (catch #t
             (lambda ()
               (let ((counts (call-with-input-file path read)))
                 (if (and (list? counts)
                          (assoc-ref counts 'assertions))
                     counts #f)))
             (lambda (key . args) #f))
      #f))

(define* (failed-test-wanted? name #:key (only (veritas-only-failed)))
  "Return @code{#t} when @var{name} should run in a last-failed rerun.

@var{only} is the record of failing names to honour; it defaults to the one read
from @code{reports/last-failed.scm} when @code{VERITAS_ONLY_FAILED} asked for a
rerun.  @code{#f} means there is no record, so everything runs; an empty list
means the last run passed, so nothing does."
  ;; A boolean, not the match `member' happens to return.
  (if (or (not only)
          (member name only)
          (member veritas-rerun-all-marker only))
      #t #f))

(define (result-failed? result)
  "Return @code{#t} when @var{result} records a failure, at any depth."
  (cond
    ((assertion-result? result) (not (assertion-result-successful result)))
    ((pre-test-result? result)
     (any result-failed? (pre-test-result-results result)))
    ;; A test result holds one child result, not a list of them.
    ((test-result? result) (result-failed? (test-result-results result)))
    ((suite-result? result)
     (any result-failed? (suite-result-test-results result)))
    ((string? result) #t)
    (else #f)))

(define (test-names-under result)
  "Return the name of every test recorded under @var{result}, at any depth."
  (cond
    ((pre-test-result? result) (list (pre-test-result-name result)))
    ((test-result? result) (test-names-under (test-result-results result)))
    ((suite-result? result) (append-map test-names-under
                                        (suite-result-test-results result)))
    (else '())))

(define (failed-names-of result)
  "Return the rerunnable names that failed under @var{result}."
  (cond
    ;; A task that raised carries no test name, so the only safe rerun is
    ;; every test there is.
    ((string? result) (list veritas-rerun-all-marker))
    ((suite-result? result)
     (let ((children (append-map failed-names-of
                                 (suite-result-test-results result))))
       (if (and (null? children) (result-failed? result))
           (test-names-under result) children)))
    ((test-result? result) (failed-names-of (test-result-results result)))
    ((pre-test-result? result)
     (if (result-failed? result) (list (pre-test-result-name result)) '()))
    (else '())))

(define (failed-test-names all-results)
  "Return the names that failed in @var{all-results}, for a last-failed rerun.

Every test whose assertions failed is named.  A suite that failed as a whole,
through its own deadline for instance, contributes the names of the tests it
holds, because a suite name is not something a rerun can select.  A failure with
no test name attached at all contributes @code{#all}, which makes the next run
run everything rather than silently run nothing.  Names come back in the order
recorded, without duplicates."
  (delete-duplicates (append-map failed-names-of all-results)))

(define* (write-last-failed-names! names #:key (path (veritas-last-failed-path)))
  "Record @var{names} at @var{path} as the tests a rerun should select.

An empty list is written when everything passed, which is what makes the next
@code{VERITAS_ONLY_FAILED} run do nothing instead of rerunning the world.  A
trouble with the file is swallowed: the run has already happened."
  (catch #t
         (lambda ()
           (ensure-parent-directories path)
           (call-with-output-file path
             (lambda (port)
               (write names port)
               (newline port)))
           names)
         ;; Recording what failed is a convenience: an unwritable report
         ;; directory must not take down a run that has already happened.
         (lambda (key . args) '())))

(define (write-last-failed! all-results)
  "Record the names that failed, so the next run can rerun just those.

Each worker process writes its own record - the path is a parameter - and the
@code{--jobs} parent merges them into the run's, which is why the writing and the
merging are separate."
  (catch #t
         (lambda ()
           (write-last-failed-names! (failed-test-names all-results)))
         (lambda (key . args) '())))

(define (merge-failed-names records)
  "Combine what several runs recorded as failing into one record.

@var{records} holds one record per run: a list of names, or @code{#f} when a run
left no record at all.  When every run recorded an empty list the result is an
empty list, so a parallel run that passed does not look like a run to rerun.
When no run left a record the result is @code{#f}, which stands for no
information rather than for nothing having failed.  Names are deduplicated and
keep the order they were found in."
  (let ((known (filter list? records)))
    (if (null? known)
        #f
        (delete-duplicates (append-map (lambda (names) names) known)))))

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
  "Return the body forms of a @code{test} body @var{xs}.

Keyword arguments such as @code{#:before} and @code{#:after} are dropped along
with the value that follows them, leaving one entry per body form: a thunk (the
@code{test} macro wraps every form in one), an assertion already built, or a list
of either."
  (drop-keyword-arguments xs))

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
                                                             #:timeout timeout))
                        #:file (veritas-current-file)
                        #:duration timeout))

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

;; ====== Reading a body ======
;;
;; A test or suite body is a sequence of expressions, so the value of its last
;; one was all the runner used to see.  In
;;
;;   (let ((account (make-account 100)))
;;     (given "an account with 100" ...)
;;     (when-step "a withdrawal" ...)
;;     (then "the balance holds" ...))
;;
;; two of the three steps were built and thrown away, and in
;;
;;   (let ((x 2))
;;     (assert-equal #:expect 2 #:got x)
;;     (display "checked"))
;;
;; the assertion was never run at all: no error, no report entry, a passing
;; test.  Bodies are therefore read for what they *build*.  The `test' and
;; `suite' macros wrap every body form in a thunk, the thunk is evaluated with a
;; collector bound, and every case built along the way is recorded as it is
;; built.  Lists of cases, nested thunks, and helper procedures that build cases
;; are understood as well, so a body can be structured however the test reads
;; best.

(define (keyword-syntax? stx)
  "Return @code{#t} when @var{stx} is a keyword, such as @code{#:timeout}."
  (keyword? (syntax->datum stx)))

(define (split-keyword-arguments args)
  "Split @var{args}, the syntax of an argument list, into its parts.

Two values come back: the @code{#:keyword value} pairs in the order they were
written, and the forms that are not part of a pair, also in order.  The
@code{test} and @code{suite} macros use it to tell a body from the flags around
it, wherever those flags were written."
  (let loop ((rest args) (keywords '()) (body '()))
    (syntax-case rest ()
      (()
       (values (reverse keywords) (reverse body)))
      ((first remaining ...)
       (if (keyword-syntax? #'first)
           (syntax-case #'(remaining ...) ()
             ((value after ...)
              (loop #'(after ...)
                    (cons #'value (cons #'first keywords))
                    body))
             (()
              (error "a keyword argument needs a value, and this one has none:"
                     (syntax->datum #'first))))
           (loop #'(remaining ...) keywords (cons #'first body)))))))

(define (defer-body-forms forms)
  "Wrap each of @var{forms} in a thunk, so a body is evaluated when its task runs.

Deferring is what makes a body form contribute both its own value and the cases
it builds: the form is evaluated inside the body's collector, at run time,
instead of while the test file is being loaded."
  (map (lambda (form) #`(lambda () #,form)) forms))

(define (veritas-task? value)
  "Return @code{#t} when @var{value} is a test, suite or benchmark."
  (and (procedure? value)
       (procedure-property value 'veritas-task-type)))

(define (veritas-assertion? value)
  "Return @code{#t} when @var{value} was built by an assertion constructor."
  (and (procedure? value)
       (procedure-property value 'veritas-assertion)))

(define (assertion-case? value)
  "Return @code{#t} when @var{value} is an assertion for a test body to run.

Assertions built by Veritas are tagged.  A plain procedure of one argument is
taken as a case too, which is how hand-written case lambdas -
@code{(lambda (vc) ((assert-true #t) vc))} - have always been written."
  (and (procedure? value)
       (not (veritas-task? value))
       (or (veritas-assertion? value)
           (not (nullary-procedure? value)))))

(define (task-case? value)
  "Return @code{#t} when @var{value} is a task for a suite to run.

Tasks built by Veritas are tagged; a procedure that takes the validation client
counts as one as well."
  (and (procedure? value)
       (not (veritas-assertion? value))
       (or (veritas-task? value)
           (not (nullary-procedure? value)))))

(define (consumed? value)
  "Return @code{#t} when @var{value} is an assertion that has already run.

An assertion a body runs itself is an input to another case rather than a case
of its own, which is what @code{(assert-true (ok? (assert-equal ...)))} means."
  (and (veritas-assertion? value)
       (procedure-property value 'veritas-assertion-ran)))

(define* (expand-body-form form case? expand #:optional (depth 0))
  "Return the cases @var{form} stands for, or builds while it runs.

A form that is a thunk - what the DSL makes of every body form - is evaluated
with a collector bound, so every case it builds is kept.  What it returns counts
as well, and an assertion it ran itself is left out.  @var{depth} bounds how far
thunks of thunks are followed."
  (if (not (nullary-procedure? form))
      (flatten-cases form case? expand depth)
      (let ((collector (make-collector)))
        (let ((value (parameterize ((current-collector collector))
                       (form))))
          (deduplicate-values (append (filter case?
                                              (remove consumed?
                                                      (collector-items
                                                       collector)))
                                      (flatten-cases value case? expand
                                                     depth)))))))

(define* (expand-test-form form #:optional (depth 0))
  "Return the assertion cases the test body form @var{form} stands for."
  (expand-body-form form assertion-case? expand-test-form depth))

(define* (expand-task-form form #:optional (depth 0))
  "Return the tasks the suite body form @var{form} stands for."
  (expand-body-form form task-case? expand-task-form depth))

(define (assertion-field result key)
  "Return what the assertion @var{result} says about @var{key}, or @code{#f}.

An assertion that reports no name, or none at all, answers @code{#f} here rather
than raising: a case is allowed to be terse, not to take the run down."
  (if (and (list? result)
           (assq key result))
      (cdr (assq key result)) #f))

(define* (run-assertion assertion index vc)
  "Run one @var{assertion} case and return the @code{assertion-result} it made.

@var{index} is the position the case has in its test, and @var{vc} is the client
its output is reported through.  A case that answers with something other than
an assertion result is a failure with a reason, not a failure in silence."
  (let* ((capture (capture-result-and-output (lambda ()
                                               (assertion vc))))
         (result (result-and-output-result capture))
         (output (result-and-output-output capture)))
    (if (and (list? result)
             (assq 'assertion-successful result))
        (make-assertion-result index output
                               (assertion-field result 'assertion-name)
                               (assertion-field result 'assertion-successful))
        (let ((message (format #f
                               "the case returned ~s, which is not an assertion result"
                               result)))
          (make-assertion-result index
                                 (string-append output
                                                (with-output-to-string (lambda ()
                                                                         (log-fail
                                                                          vc message))))
                                 message #f)))))

(define (seconds-since start)
  "Return how many seconds have passed since @var{start}, a @code{gettimeofday}."
  (let ((end (gettimeofday)))
    (+ (- (car end) (car start))
       (/ (- (cdr end) (cdr start)) 1000000.0))))

(define (run-test-forms forms vc)
  "Run the assertions each of @var{forms} stands for, numbering them as they run.

A form is expanded, and what it built is run, before the next form is looked at:
state a body prepares between two assertions is in place when the second one
runs, and the output of one form is never reported out of order."
  (let loop ((forms forms) (index 0) (out '()))
    (if (null? forms) (reverse out)
        (let* ((cases (expand-test-form (car forms)))
               (results (map (lambda (assertion i)
                               (run-assertion assertion i vc))
                             cases (iota (length cases) index))))
          (loop (cdr forms)
                (+ index (length cases))
                (append (reverse results) out))))))

(define* (no-assertions-result vc name)
  "Return the failed assertion reported for a test body that built none.

A body that runs no assertions at all looks exactly like a passing test, so it
is reported as a failure instead: either the test checks nothing, or its checks
are built where the runner cannot see them."
  (let ((message (format #f
                         "the body of ~a built no assertions"
                         (if name name "the test"))))
    (make-assertion-result 0
                           (with-output-to-string (lambda ()
                                                    (log-fail vc message)))
                           message #f)))

(define* (as-task kind task)
  "Tag @var{task}, a procedure the DSL built, as a task of @var{kind}, and record it.

Recording it is what lets a body form that builds several tasks - the body of a
@code{let} around several tests, say - contribute every one of them.  @code{#f},
which is what a filter that dropped the task looks like, is passed through and
never recorded."
  (when (procedure? task)
    (set-procedure-property! task 'veritas-task-type kind))
  (collect-in! task))

(define-syntax test
  (lambda (stx)
    (syntax-case stx ()
      ((_ name args ...)
       (call-with-values (lambda ()
                           (split-keyword-arguments #'(args ...)))
         (lambda (keywords body)
           (with-syntax (((keyword ...) keywords)
                         ((thunk ...) (defer-body-forms body)))
             #'(%test name keyword ... thunk ...))))))))

(define* (%test name
                #:key (debug? #f)
                (shuffle? #f)
                (skip? #f)
                (timeout #f)
                (tags '())
                (before #f)
                (after #f)
                #:rest cases)
  "Define a test named @var{name}; the procedure behind the @code{test} macro.

The arguments after @var{name} are the body.  Each form is evaluated when the
test runs, and every assertion built while it is evaluated is a case: an
assertion, a list of them, a thunk of them, or a helper procedure that builds
them are all understood.  @var{before} and @var{after} run around the body,
@var{timeout} caps the whole test in seconds, hooks included, @var{skip?} drops
it from the run and @var{tags} decide which filter may select it.

A body is read in order, because a body is a sequence: one form prepares what
the next one checks.  @var{shuffle?} asks for the order of the body forms to be
randomized anyway, which is a way to find checks that depend on each other by
accident.

@example
(test \"Addition Validation\"
      #:before (lambda (vc) (display \"Initializing...\"))
      (assert-equal #:expect 4 #:got (+ 2 2)))
@end example"
  (let* ((func (lambda (vc)
                 (log-test vc name)
                 (let* ((started (gettimeofday))
                        (forms (get-cases-for-test cases))
                        (ordered (if shuffle?
                                     (fisher-yates-shuffle forms)
                                     forms))
                        (_ (if (not before)
                               '()
                               (before vc)))
                        (test-results (run-test-forms ordered vc))
                        (_ (if (not after)
                               '()
                               (after vc)))
                        (test-results (if (null? test-results)
                                          (list (no-assertions-result vc name))
                                          test-results)))
                   (when debug?
                     (newline)
                     (write test-results)
                     (newline))
                   (make-pre-test-result name test-results
                                         #:file (veritas-current-file)
                                         #:duration (seconds-since started))))))
    (if (or skip?
            (not (failed-test-wanted? name))) #f
        (if (not (tag-matches? #:tags tags
                               #:restriction (veritas-only-tag))) #f
            (as-task 'test (do-if-string-contained #:name name
                                                  #:restriction (veritas-only-test)
                                                  #:result (with-task-timeout
                                                               timeout func
                                                               (lambda (vc)
                                                                 (task-timeout-result
                                                                  vc name
                                                                  timeout)))))))))

(define (get-tests-for-suite xs)
  "Return the body forms of a @code{suite} body @var{xs}.

Keyword arguments such as @code{#:before-all} and @code{#:after-all} are dropped
along with the value that follows them, leaving one entry per child form: a
thunk (the @code{suite} macro wraps every form in one), a test or suite already
built, or a list of either."
  (drop-keyword-arguments xs))

(define-syntax suite
  (lambda (stx)
    (syntax-case stx ()
      ((_ name args ...)
       (call-with-values (lambda ()
                           (split-keyword-arguments #'(args ...)))
         (lambda (keywords body)
           (with-syntax (((keyword ...) keywords)
                         ((thunk ...) (defer-body-forms body)))
             #'(%suite name keyword ... thunk ...))))))))

(define* (%suite name
                 #:key (debug? #f)
                 (shuffle? #t)
                 (skip? #f)
                 (timeout #f)
                 (tags '())
                 (concurrent? #t)
                 (before-all #f)
                 (after-all #f)
                 #:rest tests)
  "Define a suite named @var{name}; the procedure behind the @code{suite} macro.

The arguments after @var{name} are the children.  Each form is evaluated when
the suite runs, and every test, suite or benchmark built while it is evaluated is
a child: one already built, a list of them, or a thunk of them are all
understood.  Children run concurrently unless @var{concurrent?} is @code{#f}, and
in shuffled order unless @var{shuffle?} is @code{#f}.  @var{before-all} and
@var{after-all} run once around them, @var{timeout} caps the whole suite in
seconds including those hooks, @var{skip?} drops it from the run and @var{tags}
decide which filter may select it."
  (let* ((func (lambda (vc)
                 (let* ((forms (get-tests-for-suite tests))
                        (children (append-map expand-task-form forms))
                        (ordered (if shuffle?
                                     (fisher-yates-shuffle children)
                                     children))
                        (indexed-tests (map (lambda (test-proc i)
                                              (list i test-proc))
                                            ordered
                                            (iota (length ordered))))
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
                                                                    test-output
                                                                    result)))))
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
                     (newline))
                   final-suite-data))))
    (if skip? #f
        (if (not (tag-matches? #:tags tags
                               #:restriction (veritas-only-suite))) #f
            (as-task 'suite (do-if-string-contained #:name name
                                                   #:restriction (veritas-only-suite)
                                                   #:result (with-task-timeout
                                                                timeout func
                                                                (lambda (vc)
                                                                  (suite-timeout-result
                                                                   vc name
                                                                   timeout)))))))))

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
            (as-task 'benchmark (with-task-timeout timeout func
                                  (lambda (vc)
                                    (task-timeout-result vc name
                                                         timeout)))) #f))))

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
                            (seeded-shuffle valid-tasks) valid-tasks)))
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

(define* (%publish-coverage-report! report vc
                                    #:key (dir #f) (html-dir #f) (sources #f))
  "Write the coverage @var{report} of this run and log where it went.

@var{vc} is used to log the summary and the paths written.  In a worker process
the report is left as an LCOV fragment for the process that merges the files of
the run; otherwise it is merged with what the earlier files of this run measured
and the LCOV, HTML, JSON, Cobertura and JaCoCo reports are rewritten.
@var{sources} names the roots the XML reports keep their filenames relative to."
  (let ((data-path (veritas-coverage-data-path)))
    (if data-path
        (write-lcov-file report data-path)
        (let ((merged (publish-coverage! report
                                         #:dir dir
                                         #:html-dir html-dir
                                         #:sources sources
                                         #:log (lambda (message)
                                                 (log-info vc message)))))
          (when (null? (coverage-report-files merged))
            ;; Silence is what made a broken coverage run look like a covered
            ;; one, so say out loud that nothing was measured.
            (log-info vc
                      (format #f
                              "coverage collected no source file; set VERITAS_COVERAGE_INCLUDE to the source directories of the project (currently ~a)"
                              (string-join (coverage-include-paths) ":"))))))))

(define (%process-execution-lifecycle vc all-results task-count start-time
                                      debug?)
  "Print the report for @var{all-results} and exit 1 if anything failed."
  (let* ((test-results (%extract-test-results all-results))
         (assertion-results (collect-assertion-successful test-results))
         ;; What this file held.  A file is one file whether its entry point ran
         ;; a handful of tests or a thousand.
         (counts (result-counts all-results #:files 1))
         (end-time (gettimeofday))
         (seconds-elapsed (- (car end-time)
                             (car start-time)))
         (microseconds-elapsed (- (cdr end-time)
                                  (cdr start-time))))
    
    (display-all-results vc all-results)
    ;; GitHub reads annotations from the log and the totals from the step
    ;; summary; both are written from the same results the JUnit document is.
    (when (and (equal? 'github-reporter
                       (assoc-ref vc 'reporter))
               (github-actions?))
      (write-github-annotations all-results)
      (append-github-summary-row counts
                                 #:name (or (veritas-current-file)
                                            "run")))
    (when (equal? 'junit-reporter
                  (assoc-ref vc 'reporter))
      (let ((data-path (veritas-junit-data-path)))
        (if data-path
            ;; A worker process: hand the report to the process that runs the
            ;; other files, so one document can be merged out of them all.
            (write-junit-data (junit-report-data all-results
                                                 #:seconds (+ seconds-elapsed
                                                              (/ microseconds-elapsed
                                                                 1000000.0)))
                              #:path data-path)
            (let ((path (getenv-or 'VERITAS_JUNIT_PATH
                                   #:default "reports/junit/veritas-junit.xml")))
              (write-junit-report all-results
                                  #:seconds (+ seconds-elapsed
                                               (/ microseconds-elapsed 1000000.0))
                                  #:path path)
              (log-info vc
                        (format #f "JUnit XML report written to ~a" path))))))
    ;; The totals of this file, and - unless this process is one worker of
    ;; several - the tally the runner reports once the last file is in.
    (let ((counts-path (veritas-result-counts-data-path)))
      (if counts-path
          (write-result-counts counts #:path counts-path)
          (veritas-result-counts (add-result-counts counts
                                                    (veritas-result-counts)))))
    ;; Leaving the failures on disk is what lets the next run rerun just those.
    (write-last-failed! all-results)
    (log-info vc
              (format #f "ran ~a tasks in: ~a seconds, ~a microseconds"
                      task-count seconds-elapsed microseconds-elapsed))
    (log-info vc (format #f "counted ~a" (result-counts-summary counts)))

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
                      (coverage-dir #f)
                      (coverage-html-dir #f)
                      (coverage-include #f)
                      (coverage-exclude #f)
                      (seed #f)
                      #:rest tasks)
  "Run @var{tasks}, the top-level tests, suites and benchmarks.

@var{shuffle?} randomizes the order of the tasks and @var{concurrent?} runs them
in parallel fibers.  @var{reporter} names a reporter, otherwise
@code{VERITAS_REPORTER} decides.  @var{seed} makes the shuffle reproducible, and
the seed actually used is reported, so a failing order can be replayed by
setting @code{VERITAS_SEED} to it.  @var{with-coverage?} measures coverage -
@code{VERITAS_COVERAGE=true} asks for it too - and @var{debug?} prints the raw
results.

Coverage is written where @var{coverage-dir} (default
@code{VERITAS_COVERAGE_DIR}, @code{\"reports/lcov\"}) says, with the HTML report
in @var{coverage-html-dir} (default @code{VERITAS_COVERAGE_HTML_DIR},
@code{\"reports/html\"}).  Only the files under @var{coverage-include} (default
@code{VERITAS_COVERAGE_INCLUDE}, the current directory) are reported, minus the
ones under @var{coverage-exclude} (default @code{VERITAS_COVERAGE_EXCLUDE})."
  (let* ((start-time (gettimeofday))
         (made-reporter (%make-reporter reporter))
         (vc (list (cons 'reporter made-reporter)))
         ;; One seed for the whole run: the runner remembers it too, so the order
         ;; of the test files and the order within them come from the same seed.
         (run-seed (and shuffle?
                        (current-seed #:seed seed)))
         (valid-tasks (filter procedure? tasks))
         ;; Benchmarks are kept apart from regular tasks so they can be run
         ;; sequentially (see %execute-tasks).
         (benchmark-tasks (filter benchmark-task? valid-tasks))
         (regular-tasks (remove benchmark-task? valid-tasks))
         (regular-thunks (%prepare-task-thunks regular-tasks shuffle? vc))
         (benchmark-thunks (%prepare-task-thunks benchmark-tasks shuffle? vc))
         (task-count (+ (length regular-thunks)
                        (length benchmark-thunks)))
         (coverage? (and (not (coverage-disabled-by-environment?))
                         (or with-coverage?
                             (coverage-enabled-by-environment?)))))
    ;; No report directory is made here: every writer (LCOV, JSON, the XML
    ;; documents, the HTML page, the JUnit document and the record of failing
    ;; tests) creates the directories it needs, so a run without coverage does
    ;; not leave an empty `reports/lcov' behind.
    (when run-seed
      (log-info vc
                (format #f
                        "shuffled with seed ~a (replay with VERITAS_SEED=~a)"
                        run-seed run-seed)))
    (call-with-values (lambda ()
                        (if coverage?
                            ;; The report comes first, the results of the run
                            ;; after it, so neither can be forgotten.  Benchmarks
                            ;; stay outside the measured extent: the hook costs
                            ;; a procedure call per instruction, which would
                            ;; distort a measurement whose whole point is
                            ;; timing, and a benchmark is not the code a test
                            ;; suite covers.
                            (call-with-values
                             (lambda ()
                               (collect-coverage
                                (lambda ()
                                  (%execute-tasks regular-thunks
                                                  '() concurrent?))
                                #:include coverage-include
                                #:exclude coverage-exclude))
                             (lambda (report regular-results)
                               (values report
                                       (append regular-results
                                               (%execute-tasks '()
                                                               benchmark-thunks
                                                               concurrent?)))))
                            (values #f
                                    (%execute-tasks regular-thunks
                                                    benchmark-thunks
                                                    concurrent?))))
                      (lambda (coverage-report all-results)
                        (when (and coverage? coverage-report)
                          (%publish-coverage-report! coverage-report
                                                     vc #:dir coverage-dir
                                                     #:html-dir
                                                     coverage-html-dir
                                                     #:sources
                                                     coverage-include))

                        (%process-execution-lifecycle vc all-results
                                                      task-count start-time
                                                      debug?)

                        all-results))))
