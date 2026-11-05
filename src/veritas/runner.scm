;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas runner)
  #:declarative? #t
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (veritas prelude)
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:use-module (veritas coverage)
  #:use-module (veritas shuffle)
  #:use-module (veritas impact)
  #:use-module (veritas trace)
  #:export (run-tests run-test-file discover-tests
                    select-changed-files
                    parse-partition
                    partition-files
                    parse-jobs
                    run-test-files
                    run-test-file-guarded
                    merge-worker-counts
                    merge-worker-coverage
                    display-result-counts))

;; Where the worker processes of `--jobs' leave their captures, their record of
;; failing names and their JUnit data, for the parent to print and merge.
(define worker-directory "reports/workers")

(define remove-stat
  (match-lambda
    ((name stat)
     name)
    ((name stat children ...)
     (list name
           (map remove-stat children)))))

(define (test-file-path file test-type)
  "Return where @var{file} of @var{test-type} lives, from the project root.

A path that already names its directories is taken as it is; a bare file name,
which is what the runner discovers, is placed under the test tree."
  (if (or (string-prefix? "/" file)
          (string-prefix? "test/" file))
      file
      (string-append "test/veritas/" test-type "/" file)))

(define* (run-test-file #:key test-file test-type entrypoint-function)
  "Load one test file and call its entrypoint.

@var{test-file} is a path relative to the test root and @var{test-type} is the
module prefix, so @code{\"math/algebra.scm\"} with @code{unit} loads
@code{(veritas unit math algebra)}.  @var{entrypoint-function} names the
zero-argument procedure to call in that module."
  (let* ((_ (display (format #f "

running ~a tests from file: ~a, entrypoint: ~a

"
                             test-type test-file entrypoint-function)))
         (module-parts (map string->symbol
                            (string-split (string-replace-substring test-file
                                                                    ".scm" "")
                                          #\/)))
         (m (resolve-module (append '(veritas)
                                    (list (string->symbol test-type))
                                    module-parts)))
         (entrypoint (module-ref m
                                 (string->symbol entrypoint-function))))
    ;; The run records this path in every test result, so a JUnit document or a
    ;; CI annotation can name the file a test came from.
    (parameterize ((veritas-current-file (test-file-path test-file test-type)))
      (apply entrypoint '()))))

(define (passing-exit? status)
  "Return @code{#t} when the exit @var{status} is the status of a passing exit."
  (or (eq? status #t) (eq? status #f) (eqv? status 0)))

(define* (run-test-file-guarded #:key test-file test-type entrypoint-function)
  "Run one test file, returning @code{#t} when it passed and @code{#f} when not.

@code{veritas-run} reports a failing file by exiting the process, and a file
that cannot even be loaded raises; either would end the whole single-process run
and take the files after it, and the totals of the run, with it.  Both are
caught here and turned into a returned answer, with what went wrong printed
against the file."
  (catch #t
         (lambda ()
           (run-test-file #:test-file test-file
                          #:test-type test-type
                          #:entrypoint-function entrypoint-function)
           #t)
         (lambda (key . args)
           (cond
             ((eq? key 'quit)
              ;; `exit' is a `quit' exception in Guile; the status says whether
              ;; the file left as a pass or as a failure.
              (if (passing-exit? (and (pair? args) (car args)))
                  #t
                  (begin
                    (display (format #f "\nthe file ~a failed\n" test-file))
                    #f)))
             (else
              ;; A file that cannot be loaded is a failure, not a pass.
              (display (format #f "\nthe file ~a raised ~a: ~s\n"
                               test-file key args))
              #f)))))

(define (junit-reporter-requested?)
  "Return @code{#t} when this run asked for the JUnit reporter."
  (equal? (getenv "VERITAS_REPORTER") "junit-reporter"))

(define (seconds-since start)
  "Return how many seconds have passed since @var{start}, a @code{gettimeofday}."
  (let ((end (gettimeofday)))
    (+ (- (car end) (car start))
       (/ (- (cdr end) (cdr start)) 1000000.0))))

(define (single-process-junit-paths count)
  "Return one JUnit fragment path per file of a single-process run."
  (map (lambda (i)
         (format #f "reports/workers/single-~a.junit.scm" i))
       (iota count)))

(define* (write-merged-single-junit paths #:key since)
  "Merge the JUnit data at @var{paths} into the document of the whole run.

A single-process run loads every test file in turn, and each @code{veritas-run}
finishes with a document of its own.  Written straight to
@code{VERITAS_JUNIT_PATH} the last file would overwrite the report of every file
before it, and a CI job would publish one file's worth of results; merging them
here is what makes the document the whole run's.  @var{since} is when the run
started, so the document's total time is the wall clock of the run."
  (let ((datas (filter-map read-junit-data paths)))
    (when (pair? datas)
      (let ((path (getenv-or 'VERITAS_JUNIT_PATH
                             #:default "reports/junit/veritas-junit.xml")))
        (write-junit-document (merge-junit-data datas)
                              #:seconds (seconds-since since)
                              #:path path)
        (display (format #f "\nJUnit XML report written to ~a\n" path))))
    (for-each (lambda (path)
                (catch #t (lambda () (delete-file path))
                       (lambda (key . args) #f)))
              paths)))

(define* (run-test-files files
                         #:key test-type entrypoint-function (junit-data-paths '()))
  "Run @var{files} one after another in this process, returning the failed ones.

A file that fails no longer ends the run: the files after it still run, so the
run reports every failure and its global totals before it exits, which is what
@code{--jobs} already did for the files it spread over workers.

When @var{junit-data-paths} is given, the @var{i}th file leaves its JUnit report
at that path as data instead of writing it as the run's document, so the caller
can merge one document out of all of them."
  (let loop ((fs files) (paths junit-data-paths) (failed '()))
    (if (null? fs)
        (reverse failed)
        (let* ((file (car fs))
               (path (and (pair? paths) (car paths)))
               (run (lambda ()
                      (run-test-file-guarded
                       #:test-file file
                       #:test-type test-type
                       #:entrypoint-function entrypoint-function)))
               (passed (if path
                           (parameterize ((veritas-junit-data-path path))
                             (run))
                           (run))))
          (loop (cdr fs)
                (if (pair? paths) (cdr paths) '())
                (if passed failed (cons file failed)))))))

(define* (run-test-files-here files #:key test-type entrypoint-function)
  "Run @var{files} in this process, print the totals, and exit 1 if any failed.

Printing the totals before the exit is the point: a run that is failing is
exactly when its numbers matter, and the failures are named together rather than
one file at a time."
  (let* ((started (gettimeofday))
         (junit? (junit-reporter-requested?))
         (junit-paths (if junit?
                          (single-process-junit-paths (length files))
                          '()))
         (failed (run-test-files files
                                 #:test-type test-type
                                 #:entrypoint-function entrypoint-function
                                 #:junit-data-paths junit-paths)))
    (when junit?
      (write-merged-single-junit junit-paths #:since started))
    (display-result-counts (veritas-result-counts))
    (if (null? failed)
        (display (format #f "\n\nfinished running tests\n"))
        (begin
          (display (format #f "\n\n~a of ~a files failed: ~{~a~^, ~}\n"
                           (length failed) (length files) failed))
          (exit 1)))))

(define (parse-jobs value)
  "Return the number of worker processes @var{value} asks for, or @code{#f}.

@code{#f} or an absent value means the run stays in one process.  A value that is
not a whole number of processes is an error rather than a silent fallback,
because a CI job that asked for parallelism and got none should be told.

@example
(parse-jobs \"4\")
@result{} 4
@end example"
  (if (not value)
      #f
      (let ((n (string->number value)))
        (if (and n
                 (integer? n)
                 (>= n 1))
            n
            (error "VERITAS_JOBS must be a whole number of worker processes, 1 or more, got"
                   value)))))

(define (make-directory! path)
  "Create @var{path}, doing nothing when it is already there."
  (catch #t
         (lambda () (mkdir path))
         (lambda (key . args) #f)))

(define* (run-file-in-worker #:key file index test-type entrypoint-function)
  "Fork a worker process for @var{file} and return its process id.

The child runs one file with @code{run-test-file} and writes its stdout and
stderr to a capture file of its own, so far-apart processes never interleave in
the terminal.  Two parameters keep the child's bookkeeping to itself: the record
of failing names (@var{index}-numbered, for the parent to merge) and the path its
JUnit report is left as data on.  A failing file exits its own child, which is
why a failure cannot stop the files scheduled after it."
  (let* ((base (format #f "~a/~a" worker-directory index))
         (capture (string-append base ".out"))
         (names-path (string-append base ".last-failed.scm"))
         (junit-path (string-append base ".junit.scm"))
         (counts-path (string-append base ".counts.scm"))
         (coverage-path (string-append base ".lcov.info"))
         (port (open-file capture "w")))
    ;; Nothing must be buffered before the fork, or the child would flush the
    ;; parent's words into its own capture file.
    (flush-all-ports)
    (let ((pid (primitive-fork)))
      (if (zero? pid)
          (begin
            (veritas-last-failed-path names-path)
            (veritas-junit-data-path junit-path)
            (veritas-result-counts-data-path counts-path)
            (veritas-coverage-data-path coverage-path)
            (dup2 (fileno port) 1)
            (dup2 (fileno port) 2)
            ;; Line buffering, so the parent following this file sees each line
            ;; as it is written instead of in blocks when the child exits.
            (setvbuf (current-output-port) 'line)
            (catch #t
                   (lambda ()
                     (run-test-file #:test-file file
                                    #:test-type test-type
                                    #:entrypoint-function entrypoint-function))
                   (lambda (key . args)
                     ;; `exit' is a `quit' exception in Guile, and `veritas-run'
                     ;; exits 1 when a file fails.  Swallowing that here would
                     ;; hand the parent a passing worker for a failing file, so
                     ;; it is thrown on to the top level, which exits with the
                     ;; status the file asked for.
                     (if (eq? key 'quit)
                         (apply throw key args)
                         (begin
                           ;; A file that cannot even be loaded is a failure,
                           ;; not a pass: one process would have aborted the
                           ;; whole run on it, and silence here would hide it.
                           (format (current-error-port)
                                   "~%the worker for ~a raised ~a: ~s~%"
                                   file key args)
                           (flush-all-ports)
                           (exit 1)))))
            (flush-all-ports)
            ;; Reaching this line means the file passed, since a failing one
            ;; left through `exit' inside the `quit' above.
            (exit 0))
          (begin (close-port port) pid)))))

(define (worker-path index suffix)
  "Return the path of one of a worker's files, @var{index} standing for the file."
  (format #f "~a/~a~a" worker-directory index suffix))

(define (worker-failed? status)
  "Return @code{#t} when the wait @var{status} of a worker is a failure.

A worker killed by a signal has no exit value, and that counts as a failure
rather than as a worker that never ran."
  (let ((code (and status (status:exit-val status))))
    (or (not code) (not (zero? code)))))

(define (worker-record index failed?)
  "Return what the worker at @var{index} recorded as failing, or @code{#f}.

A worker that failed without leaving a record - it was killed, or its file left
before the runner wrote anything - is taken to mean the whole run should be
rerun, because nothing else says what went wrong and a record that reads like a
passing run would make the next @code{--only-failed} run select nothing."
  (let ((record (read-last-failed #:path (worker-path index ".last-failed.scm"))))
    (if (and failed? (not (list? record)))
        (list veritas-rerun-all-marker)
        record)))

(define (merge-worker-junit indexes)
  "Return the JUnit data of the workers at @var{indexes} merged into one, or #f.

@code{#f} means no worker wrote any, which is what a run without the JUnit
reporter looks like."
  (let ((datas (filter-map (lambda (i)
                             (read-junit-data (worker-path i ".junit.scm")))
                           indexes)))
    (if (null? datas) #f (merge-junit-data datas))))

(define (merge-worker-counts indexes)
  "Return the totals of the workers at @var{indexes} added up.

A worker that left no counts - it failed before its file ran - contributes
nothing, because there is nothing to count."
  (fold (lambda (i total)
          (let ((counts (read-result-counts (worker-path i ".counts.scm"))))
            (if counts
                (add-result-counts total counts)
                total)))
        empty-result-counts indexes))

(define (display-result-counts counts)
  "Print the global totals of a run: its files, suites, tests and assertions."
  (display (format #f "\nglobal totals: ~a\n"
                   (result-counts-summary counts))))

(define (merge-worker-coverage indexes)
  "Return the coverage of the workers at @var{indexes} merged into one report.

A worker that was not asked for coverage left no fragment, and a run without
coverage merges to an empty report, which is then left alone."
  (merge-lcov-files (map (lambda (i) (worker-path i ".lcov.info")) indexes)))

(define worker-poll-microseconds
  ;; How often the parent looks at the capture it is following.  Short enough
  ;; that a line shows up as it is written, long enough not to cost anything.
  20000)

(define (tail-from path offset)
  "Return @code{(text . offset)}: what @var{path} holds after byte @var{offset}.

The empty string comes back when there is nothing new, and also when the capture
cannot be read at all - a worker whose capture is not there yet has simply not
written anything.  The offset returned is in bytes, so a file whose output holds
multi-byte characters is still followed exactly."
  (if (file-exists? path)
      (catch #t
             (lambda ()
               (call-with-input-file path
                 (lambda (port)
                   (seek port offset SEEK_SET)
                   (let ((text (get-string-all port)))
                     (cons text (ftell port))))))
             (lambda (key . args) (cons "" offset)))
      (cons "" offset)))

(define (pump-worker-output index offsets)
  "Print whatever the worker at @var{index} has written since it was last read.

@var{offsets} remembers how far each worker has been printed, so the same bytes
are never shown twice and none are skipped."
  (let* ((read (tail-from (worker-path index ".out")
                          (vector-ref offsets index)))
         (text (car read)))
    (vector-set! offsets index (cdr read))
    (unless (string-null? text)
      (display text)
      ;; Unbuffered, or the parent would hold the run's own progress back.
      (force-output (current-output-port)))))

(define (follow-workers! count statuses offsets)
  "Print the workers' output in file order, streaming whatever is ready.

The earliest file that has not been printed in full is the only one whose output
is shown, which is what keeps the log reading exactly as a sequential run's log
would.  While that worker runs, its capture is followed, so its lines arrive as
it writes them; the next file is taken up the moment that worker is finished,
which is what makes the output arrive during the run instead of at the end.

A file that is finished without ever having printed anything gets a line of its
own: a worker that died before it printed looks the same otherwise."
  (let loop ((i 0))
    (when (< i count)
      (pump-worker-output i offsets)
      (when (vector-ref statuses i)
        (when (zero? (vector-ref offsets i))
          (display (format #f "\nthe worker for file ~a produced no output\n" i)))
        (loop (+ i 1))))))

(define (clear-worker-directory)
  "Remove the worker captures and records once they have been merged."
  (if (file-exists? worker-directory)
      (catch #t
             (lambda ()
               (for-each (lambda (entry)
                           (unless (member entry '("." ".."))
                             (delete-file (string-append worker-directory "/"
                                                         entry))))
                         (scandir worker-directory))
               (rmdir worker-directory))
             (lambda (key . args) #f))))

(define* (run-tests-in-workers files #:key jobs test-type entrypoint-function)
  "Run each of @var{files} in a worker process, @var{jobs} of them at a time.

One file per process, so the files really do run on several cores and a file
that exits the run - which is how a failing file reports itself - exits only its
own worker.

The output is streamed rather than collected: each worker writes to a capture of
its own and the parent follows the earliest file it has not printed in full, so
the log arrives while the run is going and still reads in file order.  The
merging of what the workers recorded as failing and of their JUnit reports
happens once the last worker is in, and the run exits 1 if any worker failed."
  (make-directory! "reports")
  (make-directory! worker-directory)
  (let* ((indexed (map cons files (iota (length files))))
         (count (length files))
         (statuses (make-vector count #f))
         (offsets (make-vector count 0))
         (start-time (gettimeofday)))
    (let loop ((remaining indexed)
               (in-flight '()))
      (cond
        ((and (null? remaining) (null? in-flight)) '())
        ((and (pair? remaining) (< (length in-flight) jobs))
         (let* ((entry (car remaining))
                (index (cdr entry))
                (pid (run-file-in-worker #:file (car entry)
                                         #:index index
                                         #:test-type test-type
                                         #:entrypoint-function entrypoint-function)))
           (loop (cdr remaining) (append in-flight (list (cons pid index))))))
        (else
         ;; Waiting for whichever worker finishes first rather than for the
         ;; oldest one stops a slow file holding up the queue behind it.  The
         ;; wait is polled so that the file being followed can be streamed while
         ;; the others run; a blocking wait would show nothing until it returned.
         (let* ((waited (waitpid -1 WNOHANG))
                (pid (car waited)))
           (if (zero? pid)
               (begin (follow-workers! count statuses offsets)
                      (usleep worker-poll-microseconds)
                      (loop remaining in-flight))
               (let ((entry (assq pid in-flight)))
                 (when entry
                   ;; `WNOHANG' reports no status for a child that has not
                   ;; finished, hence the check on the pid above.
                   (vector-set! statuses (cdr entry) (cdr waited)))
                 (follow-workers! count statuses offsets)
                 (loop remaining
                       (remove (lambda (p) (= (car p) pid)) in-flight))))))))
    ;; Everything has been reaped; this drains the last file's last bytes.
    (follow-workers! count statuses offsets)
    (let* ((indexes (map cdr indexed))
           (failed? (lambda (i) (worker-failed? (vector-ref statuses i))))
           (failures (filter failed? indexes))
           (records (map (lambda (i) (worker-record i (failed? i))) indexes))
           (names (merge-failed-names records))
           (junit (merge-worker-junit indexes))
           (coverage (merge-worker-coverage indexes))
           (end-time (gettimeofday))
           ;; The wall clock of the whole parallel run, to the microsecond: it
           ;; belongs in the report and in the JUnit document's total time.
           (seconds (+ (- (car end-time) (car start-time))
                       (/ (- (cdr end-time) (cdr start-time)) 1000000.0))))
      ;; The reports go down before the exit status, so a failed run leaves the
      ;; record a rerun and a CI dashboard need.
      (cond ((list? names) (write-last-failed-names! names))
            ((pair? failures)
             (write-last-failed-names! (list veritas-rerun-all-marker)))
            ;; No records and no failures: every file passed without reaching
            ;; the writer, which is still nothing to rerun.
            (else (write-last-failed-names! '())))
      (when junit
        (let ((path (getenv-or 'VERITAS_JUNIT_PATH
                               #:default "reports/junit/veritas-junit.xml")))
          (write-junit-document junit #:seconds seconds #:path path)
          (display (format #f "\nJUnit XML report written to ~a\n" path))))
      ;; One coverage report out of the fragments of every worker, so `--jobs'
      ;; measures the same code as a single process would.
      (unless (null? (coverage-report-files coverage))
        (publish-coverage! coverage
                           #:log (lambda (message)
                                   (display (format #f "\n~a\n" message)))))
      ;; One total out of every worker: the files ran in processes of their
      ;; own, so nothing else adds their counts up.
      (display-result-counts (merge-worker-counts indexes))
      (display (format #f
                       "\n\n~a files in ~a worker processes: ~a passed, ~a failed in ~,2f seconds\n"
                       (length files) jobs
                       (- (length files) (length failures))
                       (length failures) seconds))
      (clear-worker-directory)
      (if (pair? failures)
          (exit 1)
          (begin (display (format #f "\nfinished running tests\n"))
                 #t)))))


(define* (select-changed-files files
                                #:key test-type changed? since (src-dir "src"))
  "Return the files of @var{files} a change can reach.

Without @var{changed?} or @var{since} every file comes back, and the run is the
whole suite it always was.  With them, the changed files are read from git, the
modules they reach are followed through imports, and only the test files that
import one of those modules - or that the change touched - come back.

A change that touches nothing returns every file rather than none: a run is
never silently empty, because an empty run looks exactly like a passing one."
  (if (not (or changed? since))
      files
      (let* ((paths (map (lambda (file)
                           (test-file-path file test-type))
                         files))
             (changed (git-changed-files #:since since)))
        (if (null? changed)
            (begin
              (display "\nchange-aware run: nothing changed, running every test file\n")
              files)
            (let ((selected (affected-test-files paths
                                                 #:src-dir src-dir
                                                 #:changed changed)))
              (let ((names (filter (lambda (file)
                                     (member (test-file-path file test-type)
                                             selected))
                                   files)))
                (display (format #f
                                 "\nchange-aware run: ~a changed file(s), ~a of ~a test files selected~{\n  ~a~}\n"
                                 (length changed) (length names) (length files)
                                 names))
                names))))))

(define* (run-tests)
  "Find every test file under the scan root and run each one.

Behaviour comes from the environment:
@itemize
@item @code{VERITAS_SCAN_DIR}: Root folder to scan. Defaults to @code{\"./\"}.
@item @code{VERITAS_TEST_TOP_DIR}: The subfolder boundary identifier containing test suites. Defaults to @code{\"test\"}.
@item @code{VERITAS_ENTRYPOINT}: The signature method executed inside test targets. Defaults to @code{\"spec\"}.
@item @code{VERITAS_TEST_MODULE}: Top level test parent folder filter. Defaults to @code{\"veritas\"}.
@item @code{VERITAS_TEST_TYPE}: Category discriminator keyword used during routing. Defaults to @code{\"unit\"}.
@item @code{VERITAS_SHUFFLE_FILES}: Set to string @code{\"true\"} to shuffle execution sequences using a Fisher-Yates algorithm. Defaults to @code{\"true\"}.
@item @code{VERITAS_SEED}: the seed every shuffle of this run uses, reported when a run is shuffled so a failing order can be replayed.
@item @code{VERITAS_PARTITION}: @code{INDEX/TOTAL} to run only one slice of the discovered files, which is how a run is split across CI machines.
@item @code{VERITAS_JOBS}: how many worker processes to run the discovered files in.  Absent or @code{1} means one process, as before; @code{2} and up run one file per process and merge their output, their record of failures and their JUnit reports.
@item @code{VERITAS_COVERAGE}: @code{\"true\"} measures code coverage even for a test file that does not ask for it, and the worker fragments are merged into one report.
@end itemize"
  (let* ((scan-dir (getenv-or 'VERITAS_SCAN_DIR
                              #:default "./"))
         (jobs (parse-jobs (getenv-or 'VERITAS_JOBS
                                      #:default #f)))
         (test-top-dir (getenv-or 'VERITAS_TEST_TOP_DIR
                                  #:default "test"))
         (entrypoint-function (getenv-or 'VERITAS_ENTRYPOINT
                                         #:default "spec"))
         (test-module (getenv-or 'VERITAS_TEST_MODULE
                                 #:default "veritas"))
         (test-type (getenv-or 'VERITAS_TEST_TYPE
                               #:default "unit"))
         (changed? (equal? "true"
                           (getenv-or 'VERITAS_CHANGED
                                      #:default "false")))
         (since (getenv-or 'VERITAS_SINCE #:default #f))
         (src-dir (getenv-or 'VERITAS_SRC_DIR #:default "src"))
         (tracing? (equal? "true"
                           (getenv-or 'VERITAS_TRACE
                                      #:default "false")))
         (listing? (equal? "true"
                           (getenv-or 'VERITAS_LIST
                                      #:default "false")))
         (shuffle-files (equal? "true"
                                (getenv-or 'VERITAS_SHUFFLE_FILES
                                           #:default "true")))
         (partition (parse-partition (getenv-or 'VERITAS_PARTITION
                                                #:default #f)))
         (_ (begin
              ;; A trace describes one run: it starts empty, and nothing is
              ;; recorded unless this run asked for it.
              (veritas-tracing? tracing?)
              (veritas-trace-frames '())
              ;; The tally is per run, so a second run in the same process
              ;; starts from zero rather than from the counts of the first.
              (veritas-result-counts empty-result-counts)
              (display (format #f "\nscanning directories for ~a tests..."
                               test-type))
              (display (format #f
                        "
scanning present working directory for ~a tests..."
                        test-type))
              (newline)))
         (files (remove-stat (file-system-tree scan-dir)))
         (interesting-files* (flatten-list (discover-tests #:test-type
                                                           test-type
                                                           #:scan-dir scan-dir
                                                           #:test-top-dir
                                                           test-top-dir
                                                           #:test-module
                                                           test-module
                                                           #:dir files)))
         ;; Sorted before anything else: a partition has to be deterministic, so
         ;; the split cannot depend on the order the directory walk happened to
         ;; produce.  Shuffling comes after the split, so each partition still
         ;; gets a random order from the run's one seed.
         (sorted-files (sort interesting-files* string<?))
         (partitioned-files (if partition
                                (partition-files sorted-files
                                                 (car partition)
                                                 (cdr partition))
                                sorted-files))
         ;; Only the files a change can reach, when the run asked for that.
         ;; The split and the shuffle still describe one deterministic order of
         ;; whatever is left.
         (partitioned-files (select-changed-files partitioned-files
                                                 #:test-type test-type
                                                 #:changed? changed?
                                                 #:since since
                                                 #:src-dir src-dir))
         ;; One seed for the files, drawn here so it can be reported: a shuffle
         ;; that cannot be replayed is a failure nobody can act on.
         (file-seed (and shuffle-files (current-seed)))
         (interesting-files (if shuffle-files
                                (fisher-yates-shuffle partitioned-files
                                                      #:seed file-seed)
                                partitioned-files)))
    (when shuffle-files
      (display (format #f
                       "\nshuffled the files with seed ~a (replay with VERITAS_SEED=~a)\n"
                       file-seed file-seed)))
    (when partition
      (display (format #f
                       "\nrunning partition ~a/~a: ~a of ~a files (~a elsewhere)\n"
                       (car partition) (cdr partition)
                       (length partitioned-files)
                       (length sorted-files)
                       (- (length sorted-files)
                          (length partitioned-files)))))
    
    (if listing?
        ;; The selection, on stdout, one file per line, with nothing run: a
        ;; script or a CI job can choose what to do with it.
        (begin
          (for-each (lambda (file) (display (format #f "~a\n" file)))
                    interesting-files)
          #t)
        (if (not (null? interesting-files))
        (begin
          (display (format #f "\nfound ~a tests: ~a\n" test-type
                           interesting-files))
          (if (and jobs (> jobs 1))
              (run-tests-in-workers interesting-files
                                    #:jobs jobs
                                    #:test-type test-type
                                    #:entrypoint-function entrypoint-function)
              (run-test-files-here interesting-files
                                   #:test-type test-type
                                   #:entrypoint-function entrypoint-function)))
        (begin
          (display (format #f "\ncould not find any ~a tests!" test-type)))))))

(define* (discover-tests #:key test-type
                         scan-dir
                         test-top-dir
                         test-module
                         dir)
  "Walk the cleaned file tree @var{dir} and collect candidate test files.

Descends through @var{scan-dir}, @var{test-top-dir} and @var{test-module}, then
hands the contents of a folder named @var{test-type} to
@code{flatten-test-files}."
  (let* ((discovery-targets (list scan-dir test-top-dir test-module)))
    (cond
      ;; An exhausted branch must be treated as empty, otherwise (car dir) is
      ;; called on '() - which happens whenever a leaf shares a name with one
      ;; of the discovery targets (for example a file named after the test
      ;; module).
      ((null? dir)
       '())
      ((and (list? dir)
            (member (car dir) discovery-targets))
       (discover-tests #:test-type test-type
                       #:scan-dir scan-dir
                       #:test-top-dir test-top-dir
                       #:test-module test-module
                       #:dir (cdr dir)))
      ((and (list? dir)
            (equal? test-type
                    (car dir)))
       (flatten-test-files (cdr dir)))
      ((list? dir)
       (map (lambda (x)
              (discover-tests #:test-type test-type
                              #:scan-dir scan-dir
                              #:test-top-dir test-top-dir
                              #:test-module test-module
                              #:dir x)) dir))
      (else '()))))

(define (parse-partition value)
  "Return the @code{(index . total)} a partition string describes, or @code{#f}.

A partition is written @code{\"2/3\"}: this machine runs the second of three
slices.  A malformed or impossible partition raises instead of quietly running
everything, because that would hide a misconfigured CI job."
  (if (not value)
      #f
      (let ((parts (string-split value #\/)))
        (if (and (= 2 (length parts))
                 (string->number (car parts))
                 (string->number (cadr parts)))
            (let ((index (inexact->exact (truncate (string->number (car parts)))))
                  (total (inexact->exact (truncate (string->number (cadr parts))))))
              (if (and (>= index 1)
                       (>= total 1)
                       (<= index total))
                  (cons index total)
                  (error "VERITAS_PARTITION is out of range, expected INDEX/TOTAL with 1 <= INDEX <= TOTAL, got"
                         value)))
            (error "VERITAS_PARTITION must look like INDEX/TOTAL, got"
                   value)))))

(define (partition-files files index total)
  "Return the files of partition @var{index} of @var{total} from @var{files}.

The split is round-robin over @var{files}, so the same project always splits the
same way and the partitions together are exactly @var{files}.

@example
(partition-files '(\"a\" \"b\" \"c\" \"d\" \"e\") 2 3)
@result{} (\"b\" \"e\")
@end example"
  (let loop ((fs files)
             (i 0)
             (out '()))
    (cond ((null? fs) (reverse out))
          ((= (modulo i total) (- index 1)) (loop (cdr fs) (+ i 1)
                                                  (cons (car fs) out)))
          (else (loop (cdr fs) (+ i 1) out)))))

(define (flatten-test-files xs)
  "Turn a nested directory listing into relative @code{.scm} file paths.

Anything that is not a Scheme file is dropped.

@example
(flatten-test-files '(\"math\" ((\"algebra.scm\" ()) (\"geometry.scm\" ()))))
@result{} (\"math/algebra.scm\" \"math/geometry.scm\")
@end example"
  (cond
    ((and (pair? xs)
          (string? (car xs))
          (not (string-suffix? ".scm"
                               (car xs)))
          (not (null? (cdr xs))))
     (map (lambda (pp)
            (format #f "~a/~a"
                    (car xs) pp))
          (flatten-test-files (cdr xs))))
    ((and (pair? xs)
          (not (null? (cdr xs))))
     (map flatten-test-files xs))
    ((list? xs)
     (flatten-list (map flatten-test-files xs)))
    ((string? xs)
     xs)
    (else '())))
