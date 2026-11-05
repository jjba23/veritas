;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Code coverage for Veritas.
;;
;; Guile collects execution counts by firing a VM hook on every instruction it
;; retires.  That hook only exists in the `debug' VM engine: in the `regular'
;; engine the same code runs silently, which is why a run that merely calls
;; `(system vm coverage)' records no hits at all.  Every collection here
;; therefore happens inside `with-debug-vm'.
;;
;; What comes out of the VM is an IP-to-count table and a census of the source
;; locations of every loaded program.  It is turned into a report - one record
;; per source file, holding the execution count of each instrumented line - and
;; that report can be written as LCOV, as a self-contained HTML report, or as
;; JSON, and merged with other reports (the files of a `--jobs' run, say).
;;
;;; Code:

(define-module (veritas coverage)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ftw)
  #:use-module (json)
  #:use-module (system vm coverage)
  #:use-module (system vm vm)
  #:use-module (veritas prelude)
  #:export (collect-coverage
            with-debug-vm
            coverage-data->report
            coverage-file?
            make-coverage-file
            coverage-file-path
            coverage-file-lines
            coverage-file-covered
            coverage-file-instrumented
            coverage-file-percentage
            coverage-file-line-count
            coverage-report?
            make-coverage-report
            empty-coverage-report
            coverage-report-files
            coverage-report-covered
            coverage-report-instrumented
            coverage-report-percentage
            coverage-report-line-count
            coverage-report-summary-string
            merge-coverage-reports
            merge-lcov-files
            lcov-string
            write-lcov
            write-lcov-file
            read-lcov
            read-lcov-file
            write-coverage-json
            write-html-report
            write-cobertura-xml
            write-jacoco-xml
            regenerate-coverage-reports
            publish-coverage!
            coverage-enabled-by-environment?
            coverage-disabled-by-environment?
            coverage-include-paths
            coverage-exclude-paths
            coverage-path-included?
            normalize-coverage-path
            coverage-directory
            coverage-html-directory
            coverage-lcov-path
            coverage-json-path
            coverage-html-path
            coverage-cobertura-path
            coverage-jacoco-path))

;; ====== Reports ======

;; A report is a list of files and a file is a list of lines.  Everything else
;; is derived, so a report can be built by a coverage run, read back from an
;; LCOV file, or assembled by hand in a test.

(define-record-type <coverage-file>
  (%make-coverage-file path lines)
  coverage-file?
  ;; Absolute path of the source file.
  (path coverage-file-path)
  ;; Ascending list of (line-number . execution-count), line numbers 1-based.
  (lines coverage-file-lines))

(define (make-coverage-file path lines)
  "Return the coverage of @var{path}, given its @var{lines}.

@var{lines} is an alist of @code{(line-number . execution-count)}.  It is
sorted by line number here, so every report has one shape however it was built."
  (%make-coverage-file path
                       (sort lines (lambda (a b) (< (car a) (car b))))))

(define (coverage-file-line-count file line)
  "Return how often @var{line} of @var{file} ran, or @code{#f} when it holds no code.

A line that was instrumented but never reached counts 0; a line with no code at
all - a comment, a blank line - has no entry and gives @code{#f}."
  (let ((entry (assv line (coverage-file-lines file))))
    (and entry (cdr entry))))

(define (coverage-file-covered file)
  "Return how many instrumented lines of @var{file} ran at least once."
  (count (lambda (entry) (> (cdr entry) 0)) (coverage-file-lines file)))

(define (coverage-file-instrumented file)
  "Return how many lines of @var{file} hold code that could be counted."
  (length (coverage-file-lines file)))

(define (percentage covered instrumented)
  "Return @var{covered} out of @var{instrumented} as a percentage.

Nothing to measure reads as @code{0.0}, so an empty report cannot be mistaken for
a fully covered one."
  (if (zero? instrumented)
      0.0
      (* 100.0 (/ covered instrumented))))

(define (coverage-file-percentage file)
  "Return the percentage of @var{file}'s instrumented lines that ran."
  (percentage (coverage-file-covered file)
              (coverage-file-instrumented file)))

(define-record-type <coverage-report>
  (%make-coverage-report files)
  coverage-report?
  (files coverage-report-files))

(define (make-coverage-report files)
  "Return a report holding @var{files}, sorted by path.

@var{files} may hold a number of @code{coverage-file} records for the same path;
they are merged, keeping the highest count seen for each line, so a report read
from several LCOV files can be handed to this directly."
  (merge-coverage-reports (%make-coverage-report (list))
                          (%make-coverage-report files)))

(define (empty-coverage-report)
  "Return a report that holds no file at all."
  (%make-coverage-report '()))

(define (coverage-report-covered report)
  "Return how many instrumented lines of @var{report} ran at least once."
  (reduce + 0 (map coverage-file-covered (coverage-report-files report))))

(define (coverage-report-instrumented report)
  "Return how many lines of @var{report} hold code that could be counted."
  (reduce + 0 (map coverage-file-instrumented (coverage-report-files report))))

(define (coverage-report-percentage report)
  "Return the percentage of @var{report}'s instrumented lines that ran."
  (percentage (coverage-report-covered report)
              (coverage-report-instrumented report)))

(define (coverage-report-line-count report path line)
  "Return how often @var{line} of @var{path} ran in @var{report}, or @code{#f}.

@var{path} has to be the absolute path the report holds."
  (let ((file (find (lambda (file)
                      (string=? (coverage-file-path file) path))
                    (coverage-report-files report))))
    (and file (coverage-file-line-count file line))))

(define* (coverage-report-summary-string report #:key (title "Code coverage"))
  "Return @var{report} as the one line a run prints.

@example
\"Code coverage: 82.4% of 340 lines (280 covered) in 12 files\"
@end example"
  (format #f "~a: ~,1f% of ~a lines (~a covered) in ~a file~p"
          title
          (coverage-report-percentage report)
          (coverage-report-instrumented report)
          (coverage-report-covered report)
          (length (coverage-report-files report))
          (length (coverage-report-files report))))

;; ====== Merging ======

(define (merge-file-lines first second)
  "Return the lines of two records of the same file, keeping the higher count."
  (let ((counts (make-hash-table)))
    (for-each (lambda (entry)
                (hash-set! counts (car entry) (cdr entry)))
              (coverage-file-lines first))
    (for-each (lambda (entry)
                (let ((line (car entry))
                      (count (cdr entry)))
                  (hash-set! counts line
                             (max count (hash-ref counts line 0)))))
              (coverage-file-lines second))
    (sort (hash-map->list cons counts)
          (lambda (a b) (< (car a) (car b))))))

(define (merge-coverage-files first second)
  "Return one record for the same file seen by @var{first} and @var{second}."
  (make-coverage-file (coverage-file-path first)
                      (merge-file-lines first second)))

(define (merge-coverage-reports . reports)
  "Return one report holding every file of @var{reports}.

The line counts of a file present in several reports are merged by taking the
highest count of each line: a line that ran in any of the runs was covered."
  (let ((by-path (make-hash-table)))
    (for-each (lambda (report)
                (for-each (lambda (file)
                            (let* ((path (coverage-file-path file))
                                   (seen (hash-ref by-path path #f)))
                              (hash-set! by-path path
                                         (if seen
                                             (merge-coverage-files seen file)
                                             file))))
                          (coverage-report-files report)))
              reports)
    (%make-coverage-report
     (sort (hash-map->list (lambda (path file) file) by-path)
           (lambda (a b) (string<? (coverage-file-path a)
                                 (coverage-file-path b)))))))

;; ====== Paths and filtering ======

(define* (absolute-path path #:key (base (getcwd)))
  "Return @var{path} against @var{base}, with @code{.} and @code{..} resolved.

A relative path is taken as relative to @var{base}; a path that already starts
at the root is kept.  The result is textual, so it works for a file that has
been deleted since it was compiled."
  (let* ((raw (if (absolute-file-name? path)
                  path
                  (in-vicinity base path)))
         (parts (filter (lambda (part)
                          (and (not (string-null? part))
                               (not (string=? part "."))))
                        (string-split raw #\/))))
    (string-append "/"
                   (string-join (reverse
                                 (fold (lambda (part out)
                                         (if (string=? part "..")
                                             (if (null? out) out (cdr out))
                                             (cons part out)))
                                       '()
                                       parts))
                                "/"))))

(define* (normalize-coverage-path name #:key (base (getcwd)))
  "Return the source file @var{name} points at.

The VM records source files the way the compiler saw them: an absolute path for
a system library, a path relative to the current directory for a project file,
or a path relative to one of Guile's load path entries.  Each is resolved here,
in that order, so a report always holds absolute paths and two spellings of one
file cannot end up as two rows."
  (let* ((direct (absolute-path name #:base base))
         (found (and (not (absolute-file-name? name))
                     (not (file-exists? direct))
                     (search-path %load-path name))))
    (if found
        (absolute-path found #:base base)
        direct)))

(define (path-under? path prefix)
  "Return @code{#t} when @var{path} is @var{prefix} or lives inside it."
  (or (string=? path prefix)
      (string-prefix? (string-append prefix "/") path)))

(define* (coverage-path-included? path #:key (include '()) (exclude '()))
  "Return @code{#t} when @var{path} belongs in a coverage report.

@var{include} is a list of directory prefixes; an empty list means every file.
@var{exclude} is a list of prefixes that win over it, so the tests of a project
can be left out of the coverage of the code they exercise."
  (and (or (null? include)
           (any (lambda (prefix) (path-under? path prefix)) include))
       (not (any (lambda (prefix) (path-under? path prefix)) exclude))))

(define* (path-list->prefixes value #:key (base (getcwd)))
  "Return @var{value} as a list of absolute directory prefixes.

@var{value} may be a list of strings or a colon-separated string, which is the
shape an environment variable arrives in."
  (let ((parts (cond
                 ((not value) '())
                 ((string? value) (string-split value #\:))
                 ((list? value) value)
                 (else (list (format #f "~a" value))))))
    (map (lambda (part)
           (absolute-path part #:base base))
         (filter (lambda (part)
                   (and (string? part) (not (string-null? part))))
                 parts))))

(define* (coverage-include-paths #:key (include #f) (base (getcwd)))
  "Return the prefixes a report is limited to.

An explicit @var{include} wins; otherwise @code{VERITAS_COVERAGE_INCLUDE} is
read, and when neither is set the current directory is the boundary, which is
what keeps Guile, Fibers and the other dependencies out of the report."
  (cond
    (include (path-list->prefixes include #:base base))
    ((getenv-or 'VERITAS_COVERAGE_INCLUDE #:default #f) =>
     (lambda (value) (path-list->prefixes value #:base base)))
    (else (list (absolute-path (getcwd) #:base base)))))

(define* (coverage-exclude-paths #:key (exclude #f) (base (getcwd)))
  "Return the prefixes left out of a report.

An explicit @var{exclude} wins, otherwise @code{VERITAS_COVERAGE_EXCLUDE} is
read.  Nothing is excluded by default."
  (cond
    (exclude (path-list->prefixes exclude #:base base))
    ((getenv-or 'VERITAS_COVERAGE_EXCLUDE #:default #f) =>
     (lambda (value) (path-list->prefixes value #:base base)))
    (else '())))

(define* (coverage-enabled-by-environment?
          #:key (value (getenv-or 'VERITAS_COVERAGE #:default #f)))
  "Return @code{#t} when @var{value} (the environment by default) asks for coverage."
  (and value (member value '("true" "1" "yes")) #t))

(define* (coverage-disabled-by-environment?
          #:key (value (getenv-or 'VERITAS_COVERAGE #:default #f)))
  "Return @code{#t} when @var{value} (the environment by default) refuses coverage.

An explicit refusal beats a test file that asks for measurement with
@code{#:with-coverage? #t}: a task that says `maak test-fast' or passes
@code{--no-coverage} means it, and it is the only way to run a file that always
measures without paying for it."
  (and value (member value '("false" "0" "no")) #t))

;; ====== Collecting ======

(define* (with-debug-vm thunk #:key (engine 'debug))
  "Call @var{thunk} on a VM whose engine is @var{engine}, then put it back.

Guile fires the VM hooks that count instructions only in the @code{debug}
engine, so this is what makes coverage - and any other hook - record anything at
all.  The engine that was current before is restored even when @var{thunk}
raises, which keeps the switch invisible to the rest of the run.

The switch is bracketed with @code{call-with-fiber-safe-wind}, not with a plain
@code{dynamic-wind}: a fiber that yields would otherwise put the old engine back
in the middle of the measured run, and everything after the first suspension
would be counted as not covered."
  (let* ((previous (catch #t vm-engine (lambda (key . args) 'regular)))
         (restore (if previous previous 'regular))
         (use-engine! (lambda (wanted)
                        (catch #t
                               (lambda ()
                                 (unless (eq? (vm-engine) wanted)
                                   (set-vm-engine! wanted)))
                               (lambda (key . args) #f)))))
    (call-with-fiber-safe-wind
     (lambda () (use-engine! engine))
     thunk
     (lambda () (use-engine! restore)))))

(define* (coverage-data->report data
                                #:key
                                (include #f)
                                (exclude #f)
                                (base (getcwd)))
  "Turn Guile's coverage @var{data} into a report.

Every instrumented source file is kept, with the count of each of its lines -
including the lines that never ran, which is the whole point of a coverage
report.  @var{include} and @var{exclude} filter the files by absolute path."
  (let ((prefixes (coverage-include-paths #:include include #:base base))
        (banned (coverage-exclude-paths #:exclude exclude #:base base)))
    (make-coverage-report
     (filter-map (lambda (name)
                   (let* ((path (normalize-coverage-path name #:base base))
                          (lines (line-execution-counts data name)))
                     (and lines
                          (pair? lines)
                          (coverage-path-included? path
                                                   #:include prefixes
                                                   #:exclude banned)
                          (make-coverage-file
                           path
                           (map (lambda (entry)
                                  (cons (1+ (car entry)) (cdr entry)))
                                lines)))))
                 (instrumented-source-files data)))))

(define* (collect-coverage thunk
                           #:key
                           (include #f)
                           (exclude #f)
                           (base (getcwd)))
  "Run @var{thunk} and return its values preceded by its coverage report.

The report is a @code{coverage-report} record; the values of @var{thunk} follow
it unchanged.  The run happens on the debug engine, and the engine in use
before the call is restored afterwards.

@example
(call-with-values
 (lambda () (collect-coverage (lambda () (run-the-tests))))
 (lambda (report result) ...))
@end example"
  (call-with-values
   (lambda ()
     (with-debug-vm (lambda () (with-code-coverage thunk))))
   (lambda (data . results)
     (apply values
            (coverage-data->report data
                                   #:include include
                                   #:exclude exclude
                                   #:base base)
            results))))

;; ====== LCOV ======

(define (write-lcov report port)
  "Write @var{report} to @var{port} in the LCOV trace file format."
  (format port "TN:~%")
  (for-each (lambda (file)
              (format port "SF:~a~%" (coverage-file-path file))
              (for-each (lambda (entry)
                          (format port "DA:~a,~a~%" (car entry) (cdr entry)))
                        (coverage-file-lines file))
              (format port "LH:~a~%" (coverage-file-covered file))
              (format port "LF:~a~%" (coverage-file-instrumented file))
              (format port "end_of_record~%"))
            (coverage-report-files report)))

(define (lcov-string report)
  "Return @var{report} as an LCOV trace file held in a string."
  (call-with-output-string (lambda (port) (write-lcov report port))))

(define* (ensure-directories! path)
  "Create @var{path} and every missing parent, doing nothing when it is there."
  (let loop ((parent (dirname path)))
    (unless (or (string=? parent path)
                (string=? parent "/")
                (string=? parent ".")
                (string-null? parent)
                (file-exists? parent))
      (loop (dirname parent))
      (catch #t (lambda () (mkdir parent)) (lambda (key . args) #t))))
  (unless (file-exists? path)
    (catch #t (lambda () (mkdir path)) (lambda (key . args) #t)))
  path)

(define (write-lcov-file report path)
  "Write @var{report} to @var{path} as LCOV, creating the directory as needed.

Returns @var{path}, so a caller can report where the file went."
  (ensure-directories! (dirname path))
  (call-with-output-file path (lambda (port) (write-lcov report port)))
  path)

(define (read-lcov port)
  "Return the LCOV trace data read from @var{port} as a report.

Only @code{SF} and @code{DA} records carry information here; the other lines
are accepted and ignored, so a file written by @code{genhtml} or by another
tool can be read as well."
  (let loop ((line (read-line port))
             (path #f)
             (counts (make-hash-table))
             (files '()))
    (define (finish)
      (make-coverage-file path
                          (hash-map->list cons counts)))
    (cond
      ((eof-object? line)
       (make-coverage-report (if path (cons (finish) files) files)))
      ((string-prefix? "SF:" line)
       (loop (read-line port)
             (substring line 3 (string-length line))
             (make-hash-table)
             (if path (cons (finish) files) files)))
      ((and path (string-prefix? "DA:" line))
       (let* ((parts (string-split (substring line 3 (string-length line))
                                   #\,))
              (number (and (pair? parts) (string->number (car parts))))
              (hits (and (pair? parts)
                         (pair? (cdr parts))
                         (string->number (cadr parts)))))
         (when (and number hits)
           (hash-set! counts
                      number
                      (max hits (hash-ref counts number 0))))
         (loop (read-line port) path counts files)))
      (else (loop (read-line port) path counts files)))))

(define* (read-lcov-file path)
  "Return the LCOV trace file at @var{path} as a report.

A missing file is an empty report, which is what a run that produced no
coverage looks like."
  (if (file-exists? path)
      (catch #t
             (lambda () (call-with-input-file path read-lcov))
             (lambda (key . args) (empty-coverage-report)))
      (empty-coverage-report)))

(define (merge-lcov-files paths)
  "Return the reports of every file of @var{paths} merged into one.

A path that does not exist is skipped, so the caller can pass the files a run
was expected to write without checking each one."
  (apply merge-coverage-reports
         (map read-lcov-file paths)))

;; ====== HTML ======

;; One self-contained page: the summary, the table of files, and the annotated
;; source of every file in it.  Nothing is fetched and nothing is split across
;; pages, so the report works from a file:// URL and can be mailed, archived or
;; opened straight from a build directory.

(define %coverage-stylesheet
  ":root{--bg:#fbfbfd;--fg:#181a1f;--muted:#5c6673;--line:#e5e8ee;--card:#fff;--track:#e9edf2;--ok:#1f8a4c;--ok-soft:#eaf7ef;--bad:#c0392b;--bad-soft:#fdeeec;--warn:#b7791f;--accent:#0b5cad;--ring:var(--ok)}
@media (prefers-color-scheme:dark){:root{--bg:#0f1216;--fg:#e7eaee;--muted:#98a2b3;--line:#232a33;--card:#151a20;--track:#222934;--ok:#46c184;--ok-soft:#10261a;--bad:#f0736a;--bad-soft:#2b1412;--warn:#dca63f;--accent:#79b8ff}}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.55 system-ui,-apple-system,sans-serif}
a{color:var(--accent);text-decoration:none}a:hover{text-decoration:underline}
code,pre{font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace}
header.top{position:sticky;top:0;z-index:10;background:var(--bg);border-bottom:1px solid var(--line);padding:1rem clamp(1rem,4vw,2.5rem) .9rem}
.headline{display:flex;align-items:center;gap:1.5rem;justify-content:space-between;flex-wrap:wrap}
h1{font-size:1.35rem;margin:0 0 .2rem}
.sub{color:var(--muted);margin:0}
.ring{--pct:0;position:relative;width:6.2rem;height:6.2rem;border-radius:50%;background:conic-gradient(var(--ring) calc(var(--pct)*1%),var(--track) 0);display:grid;place-items:center;flex:none}
.ring.mid{--ring:var(--warn)}.ring.low{--ring:var(--bad)}
.ring-inner{display:grid;place-items:center;width:4.7rem;height:4.7rem;border-radius:50%;background:var(--bg);font-weight:700;font-variant-numeric:tabular-nums}
.stats{display:flex;gap:1.6rem;list-style:none;margin:.9rem 0 0;padding:0;flex-wrap:wrap}
.stats li{display:flex;flex-direction:column}
.stats b{font-size:1.05rem;font-variant-numeric:tabular-nums}
.stats span{color:var(--muted);font-size:.74rem;text-transform:uppercase;letter-spacing:.06em}
.controls{display:flex;gap:1rem;align-items:center;margin-top:.9rem;flex-wrap:wrap}
.controls input[type=search]{flex:1;min-width:14rem;padding:.45rem .7rem;border:1px solid var(--line);border-radius:.5rem;background:var(--card);color:var(--fg);font:inherit}
.controls label{color:var(--muted);font-size:.9rem;display:flex;align-items:center;gap:.4rem;white-space:nowrap}
main{padding:1.4rem clamp(1rem,4vw,2.5rem) 3rem;max-width:120rem;margin:0 auto}
table{border-collapse:collapse;width:100%}
.files{margin-bottom:2rem}
.files th{font-size:.72rem;text-transform:uppercase;letter-spacing:.07em;color:var(--muted);text-align:left;padding:.4rem .6rem;border-bottom:1px solid var(--line)}
.files td{padding:.45rem .6rem;border-bottom:1px solid var(--line)}
.files tr:hover td{background:var(--card)}
.num,.count{text-align:right;font-variant-numeric:tabular-nums;white-space:nowrap}
.good{color:var(--ok)}.mid{color:var(--warn)}.low{color:var(--bad)}
.bar{background:var(--track);border-radius:999px;height:.6rem;min-width:6rem;overflow:hidden}
.bar span{display:block;height:100%;background:var(--ok)}
.bar.mid span{background:var(--warn)}.bar.low span{background:var(--bad)}
section.file{margin:0 0 2rem;border:1px solid var(--line);border-radius:.8rem;background:var(--card);overflow:hidden;scroll-margin-top:12rem}
.file-head{display:flex;gap:1rem;align-items:baseline;justify-content:space-between;flex-wrap:wrap;padding:.9rem 1rem .4rem}
.file-head h2{font-size:.98rem;margin:0;font-weight:600;overflow-wrap:anywhere}
.file-meta{display:flex;gap:.7rem;align-items:center;color:var(--muted);font-size:.85rem;flex-wrap:wrap}
.chip{background:var(--ok-soft);color:var(--ok);border-radius:999px;padding:.1rem .55rem;font-variant-numeric:tabular-nums}
.uncovered{color:var(--bad)}
section.file>.bar{margin:0 1rem .8rem}
.source{font-size:.85rem}
.source th{font-size:.72rem;text-transform:uppercase;letter-spacing:.07em;color:var(--muted);text-align:left;padding:.35rem .6rem;border-bottom:1px solid var(--line);border-top:1px solid var(--line)}
.source td{padding:0 .6rem;vertical-align:top}
.source td.num,.source th.num{width:4.5rem}
.source td.count,.source th.count{width:4.5rem}
.source td.code{width:auto}
.source td.num{color:var(--muted);padding-top:.15rem;user-select:none}
.source td.count{color:var(--ok);padding-top:.15rem}
.source td.code pre{margin:0;padding:.15rem 0;font-size:.82rem;white-space:pre-wrap;word-break:break-word}
tr.hit td.code{background:var(--ok-soft)}
tr.miss td.code{background:var(--bad-soft)}
tr.miss td.count{color:var(--bad);font-weight:600}
tr.plain td.code{background:transparent}
body.hide-covered tr.hit{display:none}
footer{color:var(--muted);font-size:.8rem;padding:0 clamp(1rem,4vw,2.5rem) 2rem;max-width:120rem;margin:0 auto}
@media (max-width:40rem){.ring{width:4.8rem;height:4.8rem}.ring-inner{width:3.6rem;height:3.6rem}}")

(define %coverage-script
  "(function(){
  var filter = document.getElementById('filter');
  var hide = document.getElementById('hide-covered');
  var rows = Array.prototype.slice.call(document.querySelectorAll('#files tbody tr'));
  var sections = Array.prototype.slice.call(document.querySelectorAll('section.file'));
  function apply() {
    var q = (filter.value || '').toLowerCase();
    function shown(path) { return q === '' || path.toLowerCase().indexOf(q) !== -1; }
    rows.forEach(function (row) { row.hidden = !shown(row.getAttribute('data-path')); });
    sections.forEach(function (section) { section.hidden = !shown(section.getAttribute('data-path')); });
  }
  filter.addEventListener('input', apply);
  document.addEventListener('keydown', function (event) {
    if (event.key === 'f' && !event.ctrlKey && !event.metaKey && event.target === document.body) {
      filter.focus();
      event.preventDefault();
    }
    if (event.key === 'Escape' && document.activeElement === filter) {
      filter.value = '';
      apply();
      filter.blur();
    }
  });
  hide.addEventListener('change', function () {
    document.body.classList.toggle('hide-covered', hide.checked);
  });
})();")

(define (html-escape text)
  "Return @var{text} with the characters that are markup in HTML escaped."
  (let loop ((chars (string->list (format #f "~a" text)))
             (out '()))
    (if (null? chars)
        (apply string-append (reverse out))
        (loop (cdr chars)
              (cons (case (car chars)
                      ((#\&) "&amp;")
                      ((#\<) "&lt;")
                      ((#\>) "&gt;")
                      ((#\") "&quot;")
                      (else (string (car chars))))
                    out)))))

(define (for-each-indexed proc items)
  "Call @var{proc} with each element of @var{items} and its 0-based index."
  (let loop ((rest items) (index 0))
    (unless (null? rest)
      (proc (car rest) index)
      (loop (cdr rest) (1+ index)))))

(define (coverage-css-class count)
  "Return the CSS class of a line with @var{count}, or @code{#f} for no code."
  (cond
    ((not count) "plain")
    ((> count 0) "hit")
    (else "miss")))

(define (coverage-level value)
  "Return the word that colours @var{value} percent as good, middling or bad."
  (cond ((>= value 80) "good")
        ((>= value 50) "mid")
        (else "low")))

(define (coverage-level-class value)
  "Return the CSS class for @var{value} percent, prefixed with a space."
  (string-append " " (coverage-level value)))

(define (percentage-bar value)
  "Return a small HTML bar showing @var{value} percent, coloured by the result."
  (format #f "<div class=\"bar~a\"><span style=\"width:~,1f%\"></span></div>"
          (coverage-level-class value)
          value))

(define (html-document title body)
  "Return a complete, self-contained HTML page with @var{title} and @var{body}."
  (string-append
   "<!DOCTYPE html>\n"
   "<html lang=\"en\">\n<head>\n"
   "<meta charset=\"utf-8\">\n"
   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
   (format #f "<title>~a</title>\n" (html-escape title))
   (format #f "<style>~a</style>\n" %coverage-stylesheet)
   "</head>\n<body>\n"
   body
   "\n<script>" %coverage-script "</script>\n</body>\n</html>\n"))

(define (read-source-lines path)
  "Return the lines of the source file at @var{path}, or @code{#f} when unreadable."
  (catch #t
         (lambda ()
           (call-with-input-file path
             (lambda (port)
               (let loop ((line (read-line port)) (lines '()))
                 (if (eof-object? line)
                     (reverse lines)
                     (loop (read-line port) (cons line lines)))))))
         (lambda (key . args) #f)))

(define (source-rows file)
  "Return the HTML rows that show @var{file}'s source, with its counts.

The counts are walked in step with the source lines instead of being looked up
one line at a time, so a report of a long file stays a linear pass."
  (let ((lines (coverage-file-lines file))
        (source (read-source-lines (coverage-file-path file))))
    (call-with-output-string
     (lambda (port)
       (if source
           (let loop ((rest source)
                      (number 1)
                      (counts lines))
             (when (pair? rest)
               (let* ((entry (and (pair? counts)
                                  (= number (caar counts))
                                  (car counts)))
                      (hits (and entry (cdr entry)))
                      (remaining (if entry (cdr counts) counts)))
                 (format port
                         "<tr class=\"~a\"><td class=\"num\">~a</td><td class=\"count\">~a</td><td class=\"code\"><pre>~a</pre></td></tr>~%"
                         (coverage-css-class hits)
                         number
                         (if hits (format #f "~a" hits) "")
                         (html-escape (car rest)))
                 (loop (cdr rest) (1+ number) remaining))))
           (format port
                   "<tr class=\"plain\"><td colspan=\"3\">the source file could not be read at report time; ~a lines were instrumented</td></tr>~%"
                   (length lines)))))))

(define (uncovered-line-numbers file)
  "Return the numbers of the lines of @var{file} that never ran."
  (filter-map (lambda (entry) (and (zero? (cdr entry)) (car entry)))
              (coverage-file-lines file)))

(define* (uncovered-summary file #:key (limit 12))
  "Return a short human sentence naming the lines of @var{file} that never ran."
  (let ((numbers (uncovered-line-numbers file)))
    (cond
      ((null? numbers) "every line covered")
      ((> (length numbers) limit)
       (format #f "uncovered: ~a … (~a lines)"
               (string-join (map number->string
                                 (take numbers limit))
                            ", ")
               (length numbers)))
      (else (format #f "uncovered: ~a"
                    (string-join (map number->string numbers) ", "))))))

(define (html-summary report title)
  "Return the sticky header of the report: totals, a ring and the controls."
  (let ((percentage (coverage-report-percentage report))
        (files (coverage-report-files report)))
    (string-append
     "<header class=\"top\">\n"
     "<div class=\"headline\">\n<div>\n"
     (format #f "<h1>~a</h1>\n" (html-escape title))
     (format #f "<p class=\"sub\">~,1f% of ~a lines covered in ~a file~p</p>\n"
             percentage
             (coverage-report-instrumented report)
             (length files)
             (length files))
     "</div>\n"
     (format #f "<div class=\"ring~a\" style=\"--pct:~,1f\"><span class=\"ring-inner\">~,1f%</span></div>\n"
             (coverage-level-class percentage)
             percentage
             percentage)
     "</div>\n"
     "<ul class=\"stats\">\n"
     (format #f "<li><b>~a</b><span>lines</span></li>\n"
             (coverage-report-instrumented report))
     (format #f "<li><b>~a</b><span>covered</span></li>\n"
             (coverage-report-covered report))
     (format #f "<li><b>~a</b><span>missed</span></li>\n"
             (- (coverage-report-instrumented report)
                (coverage-report-covered report)))
     (format #f "<li><b>~a</b><span>files</span></li>\n" (length files))
     "</ul>\n"
     "<div class=\"controls\">\n"
     "<input id=\"filter\" type=\"search\" placeholder=\"Filter files… (press f)\" autocomplete=\"off\">\n"
     "<label><input id=\"hide-covered\" type=\"checkbox\"> hide covered lines</label>\n"
     "</div>\n"
     "</header>\n")))

(define (files-table files)
  "Return the table that lists every file of @var{files} and links to it."
  (string-append
   "<table class=\"files\" id=\"files\">\n"
   "<thead><tr><th>File</th><th class=\"num\">Lines</th><th class=\"num\">Covered</th><th class=\"num\">Coverage</th><th></th></tr></thead>\n"
   "<tbody>\n"
   (call-with-output-string
    (lambda (port)
      (for-each-indexed
       (lambda (file index)
         (let ((percentage (coverage-file-percentage file)))
           (format port
                   "<tr data-path=\"~a\"><td><a href=\"#file-~a\"><code>~a</code></a></td><td class=\"num\">~a</td><td class=\"num\">~a</td><td class=\"num~a\">~,1f%</td><td>~a</td></tr>~%"
                   (html-escape (coverage-file-path file))
                   index
                   (html-escape (coverage-file-path file))
                   (coverage-file-instrumented file)
                   (coverage-file-covered file)
                   (coverage-level-class percentage)
                   percentage
                   (percentage-bar percentage))))
       files)))
   "</tbody>\n</table>\n"))

(define (file-section file index)
  "Return one section of the page: the head of @var{file} and its source."
  (let ((percentage (coverage-file-percentage file)))
    (string-append
     (format #f "<section class=\"file\" id=\"file-~a\" data-path=\"~a\">\n"
             index
             (html-escape (coverage-file-path file)))
     "<div class=\"file-head\">\n"
     (format #f "<h2><code>~a</code></h2>\n"
             (html-escape (coverage-file-path file)))
     (format #f "<div class=\"file-meta\"><span class=\"chip\">~a of ~a lines</span><b class=\"~a\">~,1f%</b><span class=\"uncovered\">~a</span></div>\n"
             (coverage-file-covered file)
             (coverage-file-instrumented file)
             (coverage-level percentage)
             percentage
             (html-escape (uncovered-summary file)))
     "</div>\n"
     (percentage-bar percentage)
     "<table class=\"source\">\n"
     "<thead><tr><th class=\"num\">Line</th><th class=\"count\">Hits</th><th>Source</th></tr></thead>\n"
     "<tbody>\n"
     (source-rows file)
     "</tbody>\n</table>\n"
     "</section>\n")))

(define (clear-generated-pages directory)
  "Delete the page files a previous report left in @var{directory}, if any.

The report is one page now; the per-file pages an older report wrote are
removed so a build directory does not keep both.  Only files this writer
generates are touched."
  (when (file-exists? directory)
    (catch #t
           (lambda ()
             (for-each (lambda (entry)
                         (when (or (string-prefix? "file-" entry)
                                   (string=? entry "index.html"))
                           (catch #t
                                  (lambda ()
                                    (delete-file (in-vicinity directory entry)))
                                  (lambda (key . args) #f))))
                       (scandir directory)))
           (lambda (key . args) #f))))

(define* (write-html-report report #:key (directory #f) (title "Veritas coverage"))
  "Write @var{report} as one self-contained HTML page into @var{directory}.

The page holds the summary, the table of files and the annotated source of
every file, so the whole project's coverage is one document: nothing is fetched,
nothing is split across pages, and it opens from a file:// URL.  Source lines
that never ran are marked, and the page can be filtered by path.

@var{directory} defaults to @code{reports/html}, or to whatever
@code{VERITAS_COVERAGE_HTML_DIR} says.  Returns the path of the page."
  (let* ((directory (or directory (coverage-html-directory)))
         (files (coverage-report-files report))
         (index (in-vicinity directory "index.html")))
    (ensure-directories! directory)
    (clear-generated-pages directory)
    (call-with-output-file index
      (lambda (port)
        (display
         (html-document
          title
          (string-append
           (html-summary report title)
           "<main>\n"
           (files-table files)
           (call-with-output-string
            (lambda (sections)
              (for-each-indexed
               (lambda (file file-index)
                 (display (file-section file file-index) sections))
               files)))
           "</main>\n"
           "<footer>Generated by Veritas. LCOV, JaCoCo, Cobertura and JSON reports "
           "are written next to the HTML report.</footer>\n"))
         port)))
    index))

;; ====== JSON ======

(define (round-percentage value)
  "Return @var{value} rounded to two decimals."
  (/ (round (* 100.0 value)) 100.0))

(define* (write-coverage-json report #:key (path #f))
  "Write a JSON summary of @var{report} to @var{path}.

The document holds the totals, and for each file its counts and the numbers of
the lines that never ran, which is what a CI job needs to annotate a pull
request without parsing HTML.  @var{path} defaults to
@code{coverage-summary.json} next to the LCOV file."
  (let* ((path (or path (coverage-json-path)))
         (files (coverage-report-files report))
         (document
          `((tool . "veritas")
            (totals . ((lines . ,(coverage-report-instrumented report))
                       (covered . ,(coverage-report-covered report))
                       (percentage
                        .
                        ,(round-percentage
                          (coverage-report-percentage report)))))
            (files
             .
             ,(list->vector
               (map (lambda (file)
                      `((path . ,(coverage-file-path file))
                        (lines . ,(coverage-file-instrumented file))
                        (covered . ,(coverage-file-covered file))
                        (percentage
                         .
                         ,(round-percentage
                           (coverage-file-percentage file)))
                        (uncovered
                         .
                         ,(list->vector
                           (filter-map (lambda (entry)
                                         (and (zero? (cdr entry))
                                              (car entry)))
                                       (coverage-file-lines file))))))
                    files))))))
    (ensure-directories! (dirname path))
    (call-with-output-file path
      (lambda (port) (display (scm->json-string document) port)))
    path))

;; ====== Java-world XML ======

;; JaCoCo and Cobertura do not know Scheme, but the dashboards and code-quality
;; services that draw coverage usually speak one of them.  Both formats are line
;; oriented, which is what a report of ours is: a line ran, or it did not.  There
;; are no branches to describe, so the branch counters are reported as zero, and
;; the instruction counts are the line counts - the finest grain this report has.

(define (coverage-fraction covered total)
  "Return @var{covered} out of @var{total} as the rate both XML formats use.

The formats spell their rates as fractions between zero and one, not as
percentages, and round nothing, so the caller decides how to show them."
  (if (zero? total)
      0.0
      (/ covered total)))

(define (path-below path root)
  "Return the part of @var{path} below @var{root}, or @code{#f} when it is not."
  (let ((prefix (if (string=? root "/")
                    "/"
                    (string-append root "/"))))
    (and (string-prefix? prefix path)
         (substring path (string-length prefix)))))

(define (source-relative-name path sources)
  "Return @var{path} relative to the first of @var{sources} that contains it."
  (let loop ((roots sources))
    (if (null? roots)
        #f
        (or (path-below path (car roots))
            (loop (cdr roots))))))

(define (path-directory name)
  "Return the directory part of @var{name}, without a trailing separator."
  (let ((slash (string-rindex name #\/)))
    (if slash (substring name 0 slash) "")))

(define (path-base-name name)
  "Return the last element of @var{name}."
  (let ((slash (string-rindex name #\/)))
    (if slash (substring name (1+ slash)) name)))

(define (without-extension name)
  "Return @var{name} with the extension after its last dot removed."
  (let ((dot (string-rindex name #\.)))
    (if dot (substring name 0 dot) name)))

(define (coverage-file-relative-name file sources)
  "Return the name @var{file} is reported under in the XML formats.

A file below one of @var{sources} (the source roots of the run) is reported
relative to it, which is what both formats expect when they also name the
roots; anything else keeps its absolute path."
  (or (source-relative-name (coverage-file-path file) sources)
      (coverage-file-path file)))

(define (files-by-directory files sources)
  "Return @var{files} grouped by the directory they are reported under.

Each group is @code{(directory . entries)}, where an entry is
@code{(file . relative-name)}; groups are sorted by directory, and so are the
entries, so two runs of the same tests write the same document."
  (let ((table (make-hash-table)))
    (for-each (lambda (file)
                (let* ((relative (coverage-file-relative-name file sources))
                       (directory (path-directory relative)))
                  (hash-set! table directory
                             (cons (cons file relative)
                                   (hash-ref table directory '())))))
              files)
    (sort (map (lambda (group)
                 (cons (car group)
                       (sort (cdr group)
                             (lambda (a b)
                               (string<? (cdr a) (cdr b))))))
               (hash-map->list cons table))
          (lambda (a b) (string<? (car a) (car b))))))

(define (now-milliseconds)
  "Return the current time as the milliseconds since the epoch XML wants."
  (let ((time (gettimeofday)))
    (+ (* 1000 (car time))
       (quotient (cdr time) 1000))))

(define (file-stats file)
  "Return the @code{(covered . instrumented)} lines of @var{file}."
  (cons (coverage-file-covered file)
        (coverage-file-instrumented file)))

(define (group-stats group)
  "Return the @code{(covered . instrumented)} lines of one directory @var{group}."
  (let loop ((entries (cdr group)) (covered 0) (instrumented 0))
    (if (null? entries)
        (cons covered instrumented)
        (let ((stats (file-stats (caar entries))))
          (loop (cdr entries)
                (+ covered (car stats))
                (+ instrumented (cdr stats)))))))

(define (report-stats report)
  "Return the @code{(covered . instrumented)} lines of @var{report}."
  (cons (coverage-report-covered report)
        (coverage-report-instrumented report)))

(define (rate-of stats)
  "Return @var{stats}, a @code{(covered . instrumented)} pair, as a rate."
  (coverage-fraction (car stats) (cdr stats)))

;;; ====== Cobertura ======

(define* (write-cobertura-xml report #:key (path #f) (sources #f))
  "Write @var{report} as a Cobertura XML document to @var{path}.

Cobertura is the format Jenkins, GitLab, Azure DevOps and SonarQube accept most
readily.  Every line carries its execution count, each file is a class, and a
directory is a package.  @var{sources} defaults to the source roots of the run,
and the filenames are written relative to them.

@var{path} defaults to @code{cobertura.xml} next to the LCOV file; the
@code{VERITAS_COBERTURA_PATH} environment variable overrides it.  Returns the
path written."
  (let* ((path (or path (coverage-cobertura-path)))
         (roots (or sources (coverage-include-paths)))
         (groups (files-by-directory (coverage-report-files report) roots))
         (timestamp (now-milliseconds))
         (totals (report-stats report)))
    (ensure-directories! (dirname path))
    (call-with-output-file path
      (lambda (port)
        (display "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" port)
        (display "<!DOCTYPE coverage SYSTEM \"http://cobertura.sourceforge.net/xml/coverage-04.dtd\">\n"
                 port)
        (format port
                "<coverage line-rate=\"~,4f\" branch-rate=\"0.0\" lines-covered=\"~a\" lines-valid=\"~a\" branches-covered=\"0\" branches-valid=\"0\" complexity=\"0\" version=\"veritas\" timestamp=\"~a\">\n"
                (rate-of totals)
                (car totals)
                (cdr totals)
                timestamp)
        (display "  <sources>\n" port)
        (for-each (lambda (root)
                    (format port "    <source>~a</source>\n"
                            (html-escape root)))
                  roots)
        (display "  </sources>\n  <packages>\n" port)
        (for-each
         (lambda (group)
           (let* ((directory (car group))
                  (entries (cdr group))
                  (package (string-join (string-split directory #\/) "."))
                  (stats (group-stats group)))
             (format port
                     "    <package name=\"~a\" line-rate=\"~,4f\" branch-rate=\"0.0\" complexity=\"0\">\n"
                     (html-escape package)
                     (rate-of stats))
             (display "      <classes>\n" port)
             (for-each
              (lambda (entry)
                (let* ((file (car entry))
                       (relative (cdr entry))
                       (base (without-extension (path-base-name relative)))
                       (class (if (string-null? package)
                                  base
                                  (string-append package "." base))))
                  (format port
                          "        <class name=\"~a\" filename=\"~a\" line-rate=\"~,4f\" branch-rate=\"0.0\" complexity=\"0\">\n"
                          (html-escape class)
                          (html-escape relative)
                          (rate-of (file-stats file)))
                  (display "          <methods/>\n          <lines>\n" port)
                  (for-each (lambda (line)
                              (format port
                                      "            <line number=\"~a\" hits=\"~a\"/>\n"
                                      (car line)
                                      (cdr line)))
                            (coverage-file-lines file))
                  (display "          </lines>\n        </class>\n" port)))
              entries)
             (display "      </classes>\n    </package>\n" port)))
         groups)
        (display "  </packages>\n</coverage>\n" port)))
    path))

;;; ====== JaCoCo ======

(define (write-counter port indent type missed covered)
  "Write one JaCoCo @code{counter} element of @var{type} to @var{port}."
  (format port
          "~a<counter type=\"~a\" missed=\"~a\" covered=\"~a\"/>\n"
          indent type missed covered))

(define (write-jacoco-file-counters port file indent)
  "Write the counters JaCoCo expects for @var{file}, prefixed by @var{indent}."
  (let* ((covered (coverage-file-covered file))
         (instrumented (coverage-file-instrumented file))
         (missed (- instrumented covered)))
    ;; An instruction is a line here, so the instruction counter and the line
    ;; counter say the same thing; a tool that only reads one of them still
    ;; shows the right percentage.
    (write-counter port indent "INSTRUCTION" missed covered)
    (write-counter port indent "LINE" missed covered)
    (write-counter port indent "COMPLEXITY" 0 0)
    (write-counter port indent "METHOD" 0 0)
    (if (zero? covered)
        (write-counter port indent "CLASS" 1 0)
        (write-counter port indent "CLASS" 0 1))))

(define (write-jacoco-total-counters port stats classes indent)
  "Write the counters shared by a package or a whole report to @var{port}.

@var{stats} is the @code{(covered . instrumented)} pair of the unit and
@var{classes} is the @code{(missed . covered)} pair of its classes."
  (let ((missed (- (cdr stats) (car stats))))
    (write-counter port indent "INSTRUCTION" missed (car stats))
    (write-counter port indent "LINE" missed (car stats))
    (write-counter port indent "COMPLEXITY" 0 0)
    (write-counter port indent "METHOD" 0 0)
    (write-counter port indent "CLASS" (car classes) (cdr classes))))

(define (group-class-stats group)
  "Return the @code{(missed . covered)} class counts of one directory @var{group}."
  (let loop ((entries (cdr group))
             (missed 0)
             (covered 0))
    (if (null? entries)
        (cons missed covered)
        (let ((file (caar entries)))
          (if (zero? (coverage-file-covered file))
              (loop (cdr entries) (1+ missed) covered)
              (loop (cdr entries) missed (1+ covered)))))))

(define (report-class-stats groups)
  "Return the @code{(missed . covered)} class counts of a whole report."
  (let loop ((rest groups) (missed 0) (covered 0))
    (if (null? rest)
        (cons missed covered)
        (let ((stats (group-class-stats (car rest))))
          (loop (cdr rest)
                (+ missed (car stats))
                (+ covered (cdr stats)))))))

(define* (write-jacoco-xml report #:key (path #f) (sources #f))
  "Write @var{report} as a JaCoCo XML document to @var{path}.

JaCoCo's format is what the coverage gutters of IDEs, SonarQube and Jenkins'
JaCoCo plugin read.  Each file becomes a class and a source file, every line
carries its missed and covered instruction counts, and the counters at class,
package and report level let a tool compute the totals without reading a line.

@var{path} defaults to @code{jacoco.xml} next to the LCOV file; the
@code{VERITAS_JACOCO_PATH} environment variable overrides it.  Returns the path
written."
  (let* ((path (or path (coverage-jacoco-path)))
         (roots (or sources (coverage-include-paths)))
         (groups (files-by-directory (coverage-report-files report) roots))
         (timestamp (now-milliseconds))
         (totals (report-stats report))
         (classes (report-class-stats groups)))
    (ensure-directories! (dirname path))
    (call-with-output-file path
      (lambda (port)
        (display "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
                 port)
        (display "<!DOCTYPE report PUBLIC \"-//JACOCO//DTD Report 1.1//EN\" \"report.dtd\">\n"
                 port)
        (display "<report name=\"veritas\">\n" port)
        (format port
                "  <sessioninfo id=\"veritas-~a\" start=\"~a\" dump=\"~a\"/>\n"
                (getpid) timestamp timestamp)
        (for-each
         (lambda (group)
           (let* ((directory (car group))
                  (entries (cdr group))
                  (stats (group-stats group))
                  (class-stats (group-class-stats group)))
             (format port "  <package name=\"~a\">\n" (html-escape directory))
             (for-each
              (lambda (entry)
                (let* ((file (car entry))
                       (relative (cdr entry))
                       (base (path-base-name relative))
                       (internal (if (string-null? directory)
                                     (without-extension base)
                                     (string-append directory "/"
                                                    (without-extension base)))))
                  (format port
                          "    <class name=\"~a\" sourcefilename=\"~a\">\n"
                          (html-escape internal)
                          (html-escape base))
                  (write-jacoco-file-counters port file "      ")
                  (display "    </class>\n" port)))
              entries)
             (for-each
              (lambda (entry)
                (let* ((file (car entry))
                       (relative (cdr entry)))
                  (format port "    <sourcefile name=\"~a\">\n"
                          (html-escape (path-base-name relative)))
                  (for-each
                   (lambda (line)
                     (let ((hits (cdr line)))
                       (format port
                               "      <line nr=\"~a\" mi=\"~a\" ci=\"~a\" mb=\"0\" cb=\"0\"/>\n"
                               (car line)
                               (if (zero? hits) 1 0)
                               (if (zero? hits) 0 1))))
                   (coverage-file-lines file))
                  (write-jacoco-file-counters port file "      ")
                  (display "    </sourcefile>\n" port)))
              entries)
             (write-jacoco-total-counters port stats class-stats "    ")
             (display "  </package>\n" port)))
         groups)
        (write-jacoco-total-counters port totals classes "  ")
        (display "</report>\n" port)))
    path))

;; ====== Publishing ======

;; One accumulator per destination directory, so the files of a whole run add
;; up instead of the last test file overwriting the report of the one before it.
;; Workers write their own LCOV fragment instead and the runner merges them, so
;; the accumulator never has to be shared between processes.

(define %coverage-accumulators (make-hash-table))

(define* (coverage-directory #:key (dir #f))
  "Return the directory the LCOV report is written to.

@var{dir} wins; otherwise @code{VERITAS_COVERAGE_DIR} decides, and
@code{\"reports/lcov\"} is the default.  The directory holds the LCOV trace, the
JSON summary and the Cobertura and JaCoCo documents."
  (or dir (getenv-or 'VERITAS_COVERAGE_DIR #:default "reports/lcov")))

(define* (coverage-html-directory #:key (html-dir #f))
  "Return the directory the HTML report is written to, or @code{#f} to skip it.

@var{html-dir} wins; otherwise @code{VERITAS_COVERAGE_HTML_DIR} decides.  An
empty value, @code{\"false\"} or @code{\"0\"} turns the HTML report off."
  (let ((value (if html-dir
                   html-dir
                   (getenv-or 'VERITAS_COVERAGE_HTML_DIR
                              #:default "reports/html"))))
    (if (and (string? value)
             (member value '("" "false" "0" "no")))
        #f
        value)))

(define* (coverage-lcov-path #:key (dir #f))
  "Return the path of the merged LCOV file of a coverage run."
  (in-vicinity (coverage-directory #:dir dir) "veritas.info"))

(define* (coverage-json-path #:key (dir #f))
  "Return the path of the JSON summary of a coverage run."
  (in-vicinity (coverage-directory #:dir dir) "coverage-summary.json"))

(define* (coverage-html-path #:key (html-dir #f))
  "Return the path of the index page of the HTML report, or @code{#f} when off."
  (let ((directory (coverage-html-directory #:html-dir html-dir)))
    (and directory (in-vicinity directory "index.html"))))

(define* (coverage-cobertura-path #:key (dir #f))
  "Return the path of the Cobertura XML report of a run.

The @code{VERITAS_COBERTURA_PATH} environment variable wins when it is set,
which is how a CI job puts the document where its dashboard looks for it."
  (or (getenv-or 'VERITAS_COBERTURA_PATH #:default #f)
      (in-vicinity (coverage-directory #:dir dir) "cobertura.xml")))

(define* (coverage-jacoco-path #:key (dir #f))
  "Return the path of the JaCoCo XML report of a run.

The @code{VERITAS_JACOCO_PATH} environment variable wins when it is set."
  (or (getenv-or 'VERITAS_JACOCO_PATH #:default #f)
      (in-vicinity (coverage-directory #:dir dir) "jacoco.xml")))

(define* (regenerate-coverage-reports #:key
                                      (lcov-file #f)
                                      (html-dir #f)
                                      (json-file #f)
                                      (cobertura-file #f)
                                      (jacoco-file #f)
                                      (sources #f))
  "Rebuild the HTML, JSON, Cobertura and JaCoCo reports from the last LCOV run.

@var{lcov-file} defaults to the report of the last run,
@code{reports/lcov/veritas.info}, and @var{sources} names the roots the two XML
documents report the files relative to (the include paths of the environment by
default).  Returns the report read back, or @code{#f} when no LCOV file has been
written yet, in which case nothing is written."
  (let ((path (or lcov-file (coverage-lcov-path))))
    (if (file-exists? path)
        (let* ((report (read-lcov-file path))
               (html (coverage-html-directory #:html-dir html-dir))
               (roots (coverage-include-paths #:include sources)))
          (when html
            (write-html-report report #:directory html))
          (write-coverage-json report #:path (or json-file
                                                 (coverage-json-path)))
          (write-cobertura-xml report #:path cobertura-file
                               #:sources roots)
          (write-jacoco-xml report #:path jacoco-file
                            #:sources roots)
          report)
        #f)))

(define* (publish-coverage! report
                            #:key
                            (dir #f)
                            (html-dir #f)
                            (json-file #f)
                            (cobertura-file #f)
                            (jacoco-file #f)
                            (sources #f)
                            (log #f))
  "Merge @var{report} into this run's coverage and write the reports.

The report of the run so far is kept per destination directory and merged with
@var{report}, so a process that runs several test files ends with all of them in
one report.  The merged report is written as LCOV, as HTML (unless
@code{html-dir} or the environment turns it off), as JSON, and as the two XML
formats the Java world reads: Cobertura and JaCoCo.  @var{sources} names the
roots the XML documents report the files relative to.  @var{log}, when given, is
called with a one-line summary and with the paths written.

Nothing is written when no file has been measured yet: an empty report is the
absence of coverage, and a report of zero lines is not something to publish.

Returns the merged report."
  (let* ((directory (coverage-directory #:dir dir))
         (key directory)
         (previous (hash-ref %coverage-accumulators key #f))
         (merged (if previous
                     (merge-coverage-reports previous report)
                     report))
         (lcov (in-vicinity directory "veritas.info"))
         (html (coverage-html-directory #:html-dir html-dir))
         (json (or json-file (in-vicinity directory "coverage-summary.json")))
         (cobertura (or cobertura-file
                        (in-vicinity directory "cobertura.xml")))
         (jacoco (or jacoco-file
                     (in-vicinity directory "jacoco.xml")))
         (roots (coverage-include-paths #:include sources)))
    (hash-set! %coverage-accumulators key merged)
    (unless (null? (coverage-report-files merged))
      (write-lcov-file merged lcov)
      (when html
        (write-html-report merged #:directory html))
      (write-coverage-json merged #:path json)
      (write-cobertura-xml merged #:path cobertura #:sources roots)
      (write-jacoco-xml merged #:path jacoco #:sources roots)
      (when log
        (log (coverage-report-summary-string merged))
        (log (format #f "Coverage reports written to ~a"
                     (string-join
                      (remove not
                              (list lcov
                                    (and html
                                         (in-vicinity html "index.html"))
                                    json
                                    cobertura
                                    jacoco))
                      ", ")))))
    merged))
