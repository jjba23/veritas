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
  #:use-module (veritas shuffle)
  #:export (run-tests run-test-file discover-tests
                    parse-partition
                    partition-files
                    parse-jobs))

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
    (apply entrypoint
           '())))

(define (parse-jobs value)
  "Return the number of worker processes @var{value} asks for, or @code{#f}.

@code{#f} or an absent value means the run stays in one process.  A value that is
not a whole number of processes is an error rather than a silent fallback,
because a CI job that asked for parallelism and got none should be told.

@example
(parse-jobs "4")
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
         (port (open-file capture "w")))
    ;; Nothing must be buffered before the fork, or the child would flush the
    ;; parent's words into its own capture file.
    (flush-all-ports)
    (let ((pid (primitive-fork)))
      (if (zero? pid)
          (begin
            (veritas-last-failed-path names-path)
            (veritas-junit-data-path junit-path)
            (dup2 (fileno port) 1)
            (dup2 (fileno port) 2)
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

(define (print-worker-capture index)
  "Print what the worker at @var{index} wrote, in place, as if it had run here."
  (let ((capture (worker-path index ".out")))
    (if (file-exists? capture)
        (let ((text (call-with-input-file capture get-string-all)))
          (if (string-null? text)
              (display (format #f "\nthe worker for file ~a produced no output\n"
                               index))
              (display text)))
        (display (format #f "\nthe worker for file ~a left no output\n" index)))))

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
own worker.  The parent waits for a worker before starting the next file, prints
the captures in the order the files were scheduled, merges what the workers
recorded as failing and their JUnit reports, and exits 1 when any worker failed."
  (make-directory! "reports")
  (make-directory! worker-directory)
  (let* ((indexed (map cons files (iota (length files))))
         (statuses (make-vector (length files) #f))
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
         ;; Wait for whichever worker finishes first, not for the oldest one:
         ;; a slow file must not hold up the queue behind it.
         (let* ((waited (waitpid -1))
                (pid (car waited))
                (status (cdr waited))
                (entry (assq pid in-flight)))
           (when entry
             (vector-set! statuses (cdr entry) status))
           (loop remaining (remove (lambda (p) (= (car p) pid)) in-flight))))))
    (let* ((indexes (map cdr indexed))
           (failed? (lambda (i) (worker-failed? (vector-ref statuses i))))
           (failures (filter failed? indexes))
           (records (map (lambda (i) (worker-record i (failed? i))) indexes))
           (names (merge-failed-names records))
           (junit (merge-worker-junit indexes))
           (end-time (gettimeofday))
           ;; The wall clock of the whole parallel run, to the microsecond: it
           ;; belongs in the report and in the JUnit document's total time.
           (seconds (+ (- (car end-time) (car start-time))
                       (/ (- (cdr end-time) (cdr start-time)) 1000000.0))))
      (for-each print-worker-capture indexes)
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
         (shuffle-files (equal? "true"
                                (getenv-or 'VERITAS_SHUFFLE_FILES
                                           #:default "true")))
         (partition (parse-partition (getenv-or 'VERITAS_PARTITION
                                                #:default #f)))
         (_ (begin
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
    
    (if (not (null? interesting-files))
        (begin
          (display (format #f "\nfound ~a tests: ~a\n" test-type
                           interesting-files))
          (if (and jobs (> jobs 1))
              (run-tests-in-workers interesting-files
                                    #:jobs jobs
                                    #:test-type test-type
                                    #:entrypoint-function entrypoint-function)
              (begin
                (for-each (lambda (file)
                            (run-test-file #:test-file file
                                           #:test-type test-type
                                           #:entrypoint-function entrypoint-function))
                          interesting-files)
                (display (format #f "\n\nfinished running tests\n")))))
        (begin
          (display (format #f "\ncould not find any ~a tests!" test-type))))))

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
