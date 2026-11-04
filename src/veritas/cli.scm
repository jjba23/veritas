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

;;; Commentary:
;;
;; Command line interface for the Veritas test runner.
;;
;; The CLI is intentionally a thin layer over the runner: every flag is
;; translated into the matching @code{VERITAS_*} environment variable that the
;; runner and the rest of Veritas already understand.  This keeps the
;; environment variables as the single, reproducible source of truth while
;; offering ergonomic syntactic sugar for interactive use and CI scripts.
;;
;; The runner module is resolved lazily (only after the environment has been
;; updated), so that parameters which are initialised from the environment when
;; their module loads - such as @code{VERITAS_RUN_BENCHMARKS} and
;; @code{VERITAS_ONLY_TEST} - observe the values supplied on the command line.
;;
;;; Code:

(define-module (veritas cli)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (srfi srfi-1)
  #:export (run-cli main
                    show-help
                    veritas-version
                    veritas-option-grammar
                    veritas-help-text
                    veritas-man-page-name))

(define veritas-version
  "1.1.0")

(define veritas-man-page-name
  "veritas(1)")

;; ====== Option grammar ======
;;
;; Names and defaults mirror the environment variables listed in the README.
;; Boolean switches come in positive/negative pairs because Guile's getopt-long
;; has no "--flag=no".

(define veritas-option-grammar
  '((scan-dir (single-char #\d)
              (value #t))
    (test-top-dir (single-char #\T)
                  (value #t))
    (test-type (single-char #\t)
               (value #t))
    (entrypoint (single-char #\e)
                (value #t))
    (test-module (single-char #\m)
                 (value #t))
    (reporter (single-char #\r)
              (value #t))
    (only-test (value #t))
    (only-suite (value #t))
    (only-tag (value #t))
    (only-failed (value #f))
    (no-only-failed (value #f))
    (seed (value #t))
    (partition (value #t))
    (jobs (single-char #\j)
          (value #t))
    (changed (value #f))
    (no-changed (value #f))
    (since (value #t))
    (src-dir (value #t))
    (list (value #f))
    (trace (value #f))
    (no-trace (value #f))
    (show-trace (value #f))
    (retries (value #t))
    (coverage (value #f))
    (no-coverage (value #f))
    (coverage-dir (value #t))
    (coverage-html-dir (value #t))
    (coverage-include (value #t))
    (coverage-exclude (value #t))
    (shuffle (single-char #\s)
             (value #f))
    (no-shuffle (value #f))
    (benchmarks (single-char #\b)
                (value #f))
    (no-benchmarks (value #f))
    (help (single-char #\h)
          (value #f))
    (version (single-char #\V)
             (value #f))))

;; ====== Help text ======

(define veritas-help-text
  (string-append "veritas "
   veritas-version
   " - test runner for the Veritas framework
"
   "\n"
   "usage: veritas [OPTIONS]\n"
   "\n"
   "Discover and run the test suites, tests and benchmarks of a project.
"
   "Every option is mapped to the corresponding VERITAS_* environment
"
   "variable, so the command line is only syntactic sugar over the exact
"
   "same configuration used programmatically and in CI.
"
   "\n"
   "options:\n"
   "  -d, --scan-dir DIR       root directory to scan for tests (VERITAS_SCAN_DIR)
"
   "  -T, --test-top-dir DIR   directory holding the test hierarchy (VERITAS_TEST_TOP_DIR)
"
   "  -t, --test-type TYPE     test category / sub-directory to load (VERITAS_TEST_TYPE)
"
   "  -e, --entrypoint NAME    no-argument function to call per file (VERITAS_ENTRYPOINT)
"
   "  -m, --test-module NAME   top level module prefix of the tests (VERITAS_TEST_MODULE)
"
   "  -r, --reporter NAME      reporter to use, e.g. emoji-reporter,
"
   "                           monotone-reporter, json-reporter,
"
   "                           junit-reporter or github-reporter (VERITAS_REPORTER)
"
   "      --only-test SUBSTR   only run tests whose name contains SUBSTR (VERITAS_ONLY_TEST)
"
   "      --only-suite SUBSTR  only run suites whose name contains SUBSTR (VERITAS_ONLY_SUITE)
"
   "      --only-tag TAG       only run tests/suites carrying TAG (VERITAS_ONLY_TAG)
"
   "      --only-failed        rerun only what failed in the previous run
"
   "                           (VERITAS_ONLY_FAILED)
"
   "      --no-only-failed     ignore the recorded failures (the default)
"
   "      --seed N             seed every shuffle of this run (VERITAS_SEED)
"
   "      --partition I/T      run slice I of the T slices of the test files
"
   "                           (VERITAS_PARTITION)
"
   "  -j, --jobs N             run the test files in N worker processes, one
"
   "                           file per process (VERITAS_JOBS)\n"
   "      --changed            run only the test files the working tree change\n"
   "                           reaches (VERITAS_CHANGED)\n"
   "      --no-changed         run every test file (the default)\n"
   "      --since REF          run only the test files the change since REF\n"
   "                           reaches (VERITAS_SINCE)\n"
   "      --src-dir DIR        source root the change graph is read from\n"
   "                           (VERITAS_SRC_DIR, default src)\n"
   "      --retries N          rerun a failing test up to N extra times\n"
   "                           (VERITAS_RETRIES)\n"
   "      --trace              record every step of the run, to\n"
   "                           reports/traces/last.scm (VERITAS_TRACE)\n"
   "      --no-trace           do not record a trace (the default)\n"
   "      --show-trace         print the timeline of the last run and exit\n"
   "      --list               print the test files this run would run, one\n"
   "                           per line, and run none of them (VERITAS_LIST)\n"
   "      --coverage           measure code coverage and write the reports
"
   "                           (VERITAS_COVERAGE)
"
   "      --no-coverage        do not measure coverage (the default)
"
   "      --coverage-dir DIR   where the LCOV trace and the JSON summary go
"
   "                           (VERITAS_COVERAGE_DIR, default reports/lcov)
"
   "      --coverage-html-dir DIR
"
   "                           where the HTML report goes, or \"false\" to skip
"
   "                           it (VERITAS_COVERAGE_HTML_DIR, default reports/html)
"
   "      --coverage-include DIRS
"
   "                           colon-separated source roots the report covers
"
   "                           (VERITAS_COVERAGE_INCLUDE, default the current
"
   "                           directory)
"
   "      --coverage-exclude DIRS
"
   "                           colon-separated source roots left out
"
   "                           (VERITAS_COVERAGE_EXCLUDE)
"
   "  -s, --shuffle            shuffle the order of discovered files (default)
"
   "      --no-shuffle         keep the discovered file order (VERITAS_SHUFFLE_FILES=false)
"
   "  -b, --benchmarks         run benchmarks (default) (VERITAS_RUN_BENCHMARKS=true)
"
   "      --no-benchmarks      skip benchmarks (VERITAS_RUN_BENCHMARKS=false)
"
   "  -h, --help               show this help and exit
"
   "  -V, --version            show version information and exit
"
   "\n"
   "environment variables:\n"
   "  VERITAS_ENTRYPOINT        entry-point function name, defaults to \"spec\"
"
   "  VERITAS_SCAN_DIR          directory to scan, defaults to \"./\"
"
   "  VERITAS_TEST_TYPE         test type directory, defaults to \"unit\"
"
   "  VERITAS_TEST_TOP_DIR      test hierarchy root, defaults to \"test\"
"
   "  VERITAS_TEST_MODULE       module prefix, defaults to \"veritas\"
"
   "  VERITAS_SHUFFLE_FILES     \"true\" or \"false\", defaults to \"true\"
"
   "  VERITAS_RUN_BENCHMARKS    \"true\" or \"false\", defaults to \"true\"
"
   "  VERITAS_ONLY_TEST         optional test-name substring filter
"
   "  VERITAS_ONLY_SUITE        optional suite-name substring filter
"
   "  VERITAS_ONLY_TAG          optional tag filter
"
   "  VERITAS_ONLY_FAILED       \"true\" to rerun only the tests that failed last time
"
   "  VERITAS_REPORTER          reporter symbol, defaults to emoji-reporter
"
   "  VERITAS_SEED              seed for every shuffle, printed by a shuffled run
"
   "  VERITAS_PARTITION         \"INDEX/TOTAL\" slice of the discovered test files
"
   "  VERITAS_JOBS              whole number of worker processes for the files,
"
   "                            defaults to 1 and then they run in this process\n"
   "  VERITAS_CHANGED           \"true\" to run only the tests the change reaches\n"
   "  VERITAS_SINCE             revision the change is read against, for example\n"
   "                            origin/trunk\n"
   "  VERITAS_SRC_DIR           source root of the change graph, defaults to src\n"
   "  VERITAS_RETRIES           extra attempts for a failing test, defaults to 0\n"
   "  VERITAS_TRACE             \"true\" to record a trace of the run\n"
   "  VERITAS_LIST              \"true\" to print the selected files and run none\n"
   "  VERITAS_COVERAGE          \"true\" to measure code coverage
"
   "  VERITAS_COVERAGE_DIR      where the LCOV trace is written, defaults to
"
   "                            reports/lcov
"
   "  VERITAS_COVERAGE_HTML_DIR where the HTML report is written, defaults to
"
   "                            reports/html, and \"false\" turns it off
"
   "  VERITAS_COVERAGE_INCLUDE  colon-separated source roots to report on,
"
   "                            defaults to the current directory
"
   "  VERITAS_COVERAGE_EXCLUDE  colon-separated source roots to leave out
"
   "  VERITAS_COBERTURA_PATH    where the Cobertura XML is written, defaults to
"
   "                            cobertura.xml in the coverage directory
"
   "  VERITAS_JACOCO_PATH       where the JaCoCo XML is written, defaults to
"
   "                            jacoco.xml in the coverage directory
"
   "  VERITAS_LAST_FAILED_PATH  where the failing names are recorded, defaults
"
   "                            to reports/last-failed.scm
"
   "  VERITAS_JUNIT_PATH        where junit-reporter writes, defaults to
"
   "                            reports/junit/veritas-junit.xml
"
   "\n"
   "examples:\n"
   "  veritas                          run unit tests with the project defaults
"
   "  veritas --test-type integration  run the integration test suite
"
   "  veritas --no-shuffle --reporter monotone-reporter
"
   "  veritas --only-suite auth        run only the suites mentioning \"auth\"
"
   "  veritas --only-tag slow          run only tests/suites tagged \"slow\"
"
   "  veritas --no-benchmarks          fast feedback loop, skip benchmarks
"
   "  veritas --only-failed            rerun what failed in the previous run
"
   "  veritas --seed 42                repeat a shuffled order exactly
"
   "  veritas --partition 1/3          run one third of the test files
"
   "  veritas --jobs 4                 run the files in four worker processes\n"
   "  veritas --changed                run only the tests this change can reach\n"
   "  veritas --since origin/trunk     run only the tests a branch touches\n"
   "  veritas --retries 2              ride out an intermittent failure\n"
   "  veritas --trace                  record the run, then\n"
   "  veritas --show-trace             print the recorded timeline\n"
   "  veritas --changed --list         show what --changed would run\n"
   "  veritas --coverage               measure coverage and write LCOV and HTML
"
   "  veritas --coverage --coverage-exclude test
"
   "                                   measure the sources, not the tests
"
   "  veritas --reporter junit-reporter  write a JUnit XML report
"
   "\n"
   "interactive use:\n"
   "  The same runner is available from a Guile REPL.  Load the CLI module and
"
   "  call (run-cli '(\"--help\")) or (show-help) to inspect the options, and
"
   "  ((@ (veritas runner) run-tests)) to run with the current environment.
"))

(define (show-help)
  "Print the full Veritas CLI help page to standard output."
  (display veritas-help-text))

;; ====== Environment translation ======

(define (setenv-if-present! opts key env)
  "Set environment variable @var{env} when option @var{key} was supplied."
  (let ((value (option-ref opts key #f)))
    (when value
      (setenv env value))))

(define (setenv-boolean! opts positive negative env default)
  "Translate a positive/negative switch pair into @var{env}, a \"true\"/\"false\" string.

@var{negative} takes precedence when both switches are supplied.  When neither
switch is present @var{default} is used."
  (let ((pos (option-ref opts positive #f))
        (neg (option-ref opts negative #f)))
    (cond
      (neg (setenv env "false"))
      (pos (setenv env "true"))
      (default (setenv env default)))))

(define (apply-options-to-environment! opts)
  "Translate parsed command line @var{opts} into VERITAS_* environment variables."
  (setenv-if-present! opts
                      'scan-dir "VERITAS_SCAN_DIR")
  (setenv-if-present! opts
                      'test-top-dir "VERITAS_TEST_TOP_DIR")
  (setenv-if-present! opts
                      'test-type "VERITAS_TEST_TYPE")
  (setenv-if-present! opts
                      'entrypoint "VERITAS_ENTRYPOINT")
  (setenv-if-present! opts
                      'test-module "VERITAS_TEST_MODULE")
  (setenv-if-present! opts
                      'reporter "VERITAS_REPORTER")
  (setenv-if-present! opts
                      'only-test "VERITAS_ONLY_TEST")
  (setenv-if-present! opts
                      'only-suite "VERITAS_ONLY_SUITE")
  (setenv-if-present! opts
                      'only-tag "VERITAS_ONLY_TAG")
  (setenv-if-present! opts
                      'seed "VERITAS_SEED")
  (setenv-if-present! opts
                      'partition "VERITAS_PARTITION")
  (setenv-if-present! opts
                      'jobs "VERITAS_JOBS")
  (setenv-if-present! opts
                      'since "VERITAS_SINCE")
  (setenv-if-present! opts
                      'src-dir "VERITAS_SRC_DIR")
  (setenv-if-present! opts
                      'retries "VERITAS_RETRIES")
  (setenv-if-present! opts
                      'coverage-dir "VERITAS_COVERAGE_DIR")
  (setenv-if-present! opts
                      'coverage-html-dir "VERITAS_COVERAGE_HTML_DIR")
  (setenv-if-present! opts
                      'coverage-include "VERITAS_COVERAGE_INCLUDE")
  (setenv-if-present! opts
                      'coverage-exclude "VERITAS_COVERAGE_EXCLUDE")
  (setenv-boolean! opts
                   'coverage
                   'no-coverage "VERITAS_COVERAGE" #f)
  (setenv-boolean! opts
                   'only-failed
                   'no-only-failed "VERITAS_ONLY_FAILED" #f)
  (setenv-boolean! opts
                   'shuffle
                   'no-shuffle "VERITAS_SHUFFLE_FILES" #f)
  (setenv-boolean! opts
                   'benchmarks
                   'no-benchmarks "VERITAS_RUN_BENCHMARKS" #f)
  (setenv-boolean! opts
                   'changed
                   'no-changed "VERITAS_CHANGED" #f)
  (setenv-boolean! opts
                   'trace
                   'no-trace "VERITAS_TRACE" #f)
  (when (option-ref opts 'list #f)
    (setenv "VERITAS_LIST" "true")))

;; ====== Driver ======

(define (ensure-test-load-path!)
  "Add the test hierarchy to Guile's load path.

Test files live under @code{SCAN_DIR/TEST_TOP_DIR} (by default @code{./test})
and declare modules such as @code{(veritas unit my-test)}.  The direct
interpreter invocation documented in the README passes @code{-L ./test}
manually; the CLI derives it from the environment so that @code{veritas} just
works out of the box."
  (let* ((scan-dir (or (getenv "VERITAS_SCAN_DIR") "./"))
         (test-top-dir (or (getenv "VERITAS_TEST_TOP_DIR") "test"))
         (base (if (string-suffix? "/" scan-dir) scan-dir
                   (string-append scan-dir "/")))
         (test-path (string-append base test-top-dir)))
    (add-to-load-path test-path)))

(define (normalize-args args)
  "Drop a leading @code{\"--\"} separator, if any.

The launcher script forwards user arguments after @code{--} so that Guile does
not try to interpret flags such as @code{-h} itself; this removes that marker."
  (if (and (pair? args)
           (string? (car args))
           (string=? "--"
                     (car args)))
      (cdr args) args))

(define (run-cli args)
  "Parse @var{args} and run the Veritas runner.

@var{args} is a list of command-line strings without the program name.  Returns
an exit status: @code{0} on success, @code{1} on failure.

@example
(run-cli '(\"--test-type\" \"unit\" \"--no-benchmarks\"))
@end example"
  (let* ((clean-args (normalize-args args))
         ;; getopt-long treats the first element as the program name, so we
         ;; supply our own.
         (opts (getopt-long (cons "veritas" clean-args) veritas-option-grammar
                            #:stop-at-first-non-option #f)))
    (cond
      ((option-ref opts
                   'help #f)
       (show-help) 0)
      ((option-ref opts
                   'version #f)
       (format #t "veritas ~a\n" veritas-version) 0)
      ((option-ref opts
                   'show-trace #f)
       (let ((trace (resolve-module '(veritas trace))))
         ((module-ref trace 'show-last-trace)))
       0)
      (else (apply-options-to-environment! opts)
            (ensure-test-load-path!)
            ;; Resolve the runner lazily, after the environment has been updated, so
            ;; that parameters initialised from the environment observe the CLI
            ;; values.
            (let ((runner (resolve-module '(veritas runner))))
              ((module-ref runner
                           'run-tests)))
            0))))

(define (main args)
  "Entry point suitable for a launcher or a Guile @code{-c} invocation.

@var{args} is the list of user arguments (typically @code{(cdr (command-line))}).
Exits the process with the status returned by @code{run-cli}."
  (exit (run-cli args)))
