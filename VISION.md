# Vision

Veritas is a test framework for GNU Guile 3.0 in which a test is a form, a failure is a sentence,
and the report is the hierarchy you wrote. It is also a runner with its own opinions: results are
deterministic, declarations are lazy, the run can be narrowed to what a diff can reach, and every
report is machine-readable. This document is about the direction of those opinions — what the
framework already anticipates, what a few years of work would make true, and which parts of the
design are worth refusing to change.

## Where veritas is today

Twenty modules under `src/veritas`, MIT licensed, and three dependencies in total: Guile 3.0,
guile-fibers and guile-json. The framework runs its own suite with itself, and CI validates the
JUnit document a run writes against a schema carried in the repository.

The vocabulary is already wide:

- **Declarations.** `test` and `suite`, both taking `#:skip?`, `#:tags`, `#:timeout`, `#:before`,
  `#:after`, `#:shuffle?`, `#:concurrent?`, `#:before-all` and `#:after-all`.
- **Matchers.** Equality, identity, truth, emptiness, containment over strings, lists, vectors and
  tables, approximate comparison, regular expressions, structural patterns via `assert-match` over
  `(ice-9 match)`, occurrence and absence of raised errors, ordering and length. `lazy` defers a
  payload into the assertion's own scope and inside its `#:timeout`, with no `eval` anywhere.
- **Styles.** Behaviour-driven aliases (`scenario`, `given`, `when-step`, `then`, the Latin
  `fabula`/`scaena`/`dato`/`quando`/`tunc` and the capitalised English spellings), table-driven
  `test-each` and `suite-each` in the spirit of pytest's parametrize, spies and stubs, temp bindings,
  snapshots with `VERITAS_UPDATE_SNAPSHOTS`, a generator library with shrinking, `property` with
  `#:runs`, `#:seed` and `#:shrink?`, and `with-container` for Podman or Docker.
- **Runner.** Discovery by directory, `--only-test`, `--only-suite` and `--only-tag` selection,
  seeded shuffling, parallel workers with `--jobs`, deterministic `--partition I/T` sharding, retries
  with flaky accounting kept apart from passes, step-by-step traces, `--only-failed` replay, and a
  final totals line that is read off the same result tree the report is printed from.
- **Change awareness.** `(veritas impact)` follows `#:use-module` from sources to importers to the
  test files that import them, so `--changed` and `--since REF` run what a diff can reach, with a
  `--list` mode that prints the selection and runs nothing.
- **Coverage.** The measured part of a run happens on Guile's debugging VM, so the counts are real;
  reports are merged across workers and written as LCOV, a JSON summary, Cobertura, JaCoCo and a
  single self-contained HTML page.
- **Reporters.** Emoji, monotone, JSON, JUnit and a GitHub reporter that writes workflow annotations
  and a step summary.

The decisions that constrain everything after them:

1. **The lazy model.** `test` and `suite` record a name, some keywords and thunks. Nothing is
   evaluated at declaration, which is what makes it possible to ask a file what it contains without
   running anything — and what makes a "describe" mode cheap.
2. **Determinism as a guarantee, not a hope.** Files sorted, results collected in declaration order
   even under concurrency, one seeded shuffle for the run and the seed printed, coverage sorted by
   path, floats fixed at three decimals. The two unavoidable seams — the clock and the current
   location — are parameters (`veritas-clock`, `veritas-current-location`), not globals.
3. **The JSON stream is the machine interface.** It is what `veritas.el` consumes to build a report
   richer than the terminal's, and it is why a report is data rather than text.
4. **The command line is sugar over environment variables.** Every flag maps to a `VERITAS_*`
   variable, so a suite behaves identically in a shell, in a script and in CI.
5. **A failing file never ends the run.** Failures are collected and reported together, then the
   process exits non-zero once.

What is honestly absent: there is no TAP reporter, no watch mode, no mutation testing, no branch
coverage, no diff coverage, no `assume` for property tests, no combinator to build a generator from
other generators, line numbers are opt-in because Guile exposes no line for a syntax object, and
`--only-*` selection is substring matching rather than a query language.

## The vision

1. **A veritas failure is a first-class artefact that any editor, CI and review tool can consume
   without scraping text.** One stable JSON schema, a TAP reporter for the tools that speak it, the
   same fields in JUnit, and a documented way for anything else to read a run.
2. **Change-aware runs are trustworthy enough to be the default in CI.** Selection that knows more
   than `#:use-module` — macros, `include`, `load`, data files, generated modules — and that is never
   silently empty, so running a subset stops being a risk and becomes the normal case.
3. **Property-based testing is native rather than bolted on.** Composable generators, preconditions,
   and shrinking that survives composition while staying replayable from one seed.
4. **Coverage answers the question a reviewer actually asks.** Branch or expression coverage from the
   same debug-VM pass, and a diff-coverage view that says whether *this patch's* lines are exercised.
5. **A subset run is always honest.** Whatever narrows a run — selection, partitions, change
   awareness, reruns — reports what it excluded, and an empty selection is an error rather than a
   green line.

## Features that are in scope and not built yet

- **A TAP reporter.** The JSON reporter already emits a verdict per assertion, so the data is there.
  The hard part is TAP's plan and subtest nesting when a suite's children report concurrently and a
  suite is both a test and a container.
- **Real line numbers everywhere.** `pre-test-result` has a `line` slot and JUnit writes it when a
  caller supplies one, because Guile 3.0 gives no line for a syntax object. The hard part is a
  reader-based or debug-info-based path that attributes a form to a source line without changing what
  `test` expands into.
- **Branch coverage.** `coverage-file-instrumented` counts the debug VM's instruction counters. The
  hard part is mapping counts to branches in a language where a form can expand into anything,
  without writing a full control-flow graph for Scheme.
- **Diff coverage.** `(veritas impact)` knows the diff and `(veritas coverage)` knows per-line
  counts, which is unusually close. The hard part is attributing a line on a branch to lines in the
  working tree across renames, moves and reformatting.
- **A deeper impact graph.** Today it follows `#:use-module` and test-name matching. The hard part is
  `include`, `load`, `define-syntax` that pulls in a module at expansion time, and modules whose names
  do not match their paths.
- **Generator composition.** Generators are independent procedures with their own shrinking. The
  hard part is a combinator layer — products, dependent generators, filtering — whose shrinking stays
  deterministic under a seed and does not reintroduce the values a filter rejected.
- **Watch mode.** There is no filesystem watcher. The hard part is not the polling loop, which fibres
  make cheap, but making it reuse `--changed` so a watch run and a CI run select the same files.
- **Snapshots of non-readable values.** A snapshot is Scheme data read back with `read`. The hard
  part is a canonical printer for ports, procedures and records, and a policy for what is stable
  enough to snapshot at all.

## New features that would be worth having

- **Mutation testing over the coverage instrumentation.** The two halves exist: instrument a file,
  flip a branch, and rerun only the tests the impact graph says can reach it. Very few frameworks can
  offer this without a second toolchain, and Veritas already has both toolchains.
- **`veritas describe --json`.** Dump the test tree — suites, tests, tags, benchmarks — without
  running anything. The lazy model makes this almost free, and it turns the suite into something a
  build step, an editor or a documentation generator can read.
- **A flake ledger.** Retries already record attempts. Persisting a per-test history across runs
  makes a flaky test visible before it fails CI, instead of after it wastes somebody's afternoon.
- **`--bisect` for a failing input.** `property` reports the seed and the shrunk value; a bisect mode
  that narrows a flaky failure's input over recorded seeds would turn the feature into a debugger.
- **A diagnostics file for other editors.** One `reports/diagnostics.scm` mapping failures to
  `file:line` and severity, which any editor integration can read without knowing the JSON schema.
- **A time budget.** Fail a run that exceeds N seconds, and print the ten slowest tests. CI budgets
  are real, and a suite that grows silently is a suite that gets skipped.
- **Tag expressions.** `--only-tag` takes one tag; `(unit and not slow)` is what a large suite
  eventually wants, and it is a small parser over the tags that already exist.

## Contributions that would move the needle

1. **The TAP reporter.** Small, self-contained, and it unlocks harnesses and editors that will never
   learn a bespoke JSON schema. The reporter seam is documented and a new one needs no core change.
2. **Diff coverage from the two existing modules.** Highest ratio of usefulness to new code in the
   project: the diff, the impact graph and the per-line counts are already there.
3. **The impact graph handling `include` and `load`.** It makes `--changed` safe enough to be a
   default, which is the whole argument for the feature.
4. **Generator combinators with honest shrinking.** Deep, well-scoped, and the property library is
   young enough that the design is still cheap to get right.
5. **`describe --json`.** A day's work that turns the lazy model into a build tool.
6. **Watch mode on top of `--changed`.** Quality-of-life with a clear correctness rule.
7. **A page on writing a reporter.** Every new reporter is a contribution; the documentation is what
   makes the second one cheap.

## Compared to related projects

| Project | What it is | Where veritas differs |
| --- | --- | --- |
| SRFI-64 | The Scheme testing reference: `test-begin`/`test-end`, a configurable test runner, thin but standard | SRFI-64 is the portable floor and has no suites, tags, concurrency, coverage or change awareness. Veritas is a full runner with its own report formats, and it is Guile-only on purpose. |
| Guile's `(test-suite lib)` | Guile's own bundled testing library, used by Guile itself | It is the incumbent for Guile projects and barely documented. Veritas offers the same declarations plus reports, parallelism and machine-readable output. |
| Rackunit | Racket's test framework: `check-equal?`, named test cases, a UI that folds results | Rackunit is tightly integrated with DrRacket. Veritas targets batch runs and CI, and treats the machine-readable stream as the primary interface. |
| `clojure.test` with `test.check` | Clojure's built-in test library plus property-based testing with shrinking | The closest philosophical neighbour, and the source of the naming (`spy`, `assert-called`, generators). Veritas adds tags, coverage formats, traces and change-aware selection; it has no REPL culture to build on. |
| pytest | Python's dominant test framework: fixtures, markers, parametrize, plugins, JUnit output | pytest's fixture graph and plugin ecosystem are far beyond Veritas. Veritas matches it on parametrized tables, markers-as-tags and JUnit output, and adds seeded determinism and change-aware runs as built-ins rather than plugins. |
| ExUnit | Elixir's test framework: `describe`/`test`, tags, `--only` and `--exclude`, setup callbacks | ExUnit is the model for the tag ergonomics and the `describe` nesting. Veritas has tags and suites but no exclude expressions yet, and it reports in five formats where ExUnit defers to JUnit. |
| Vitest and Jest | JavaScript test runners: watch mode, snapshots, coverage, rich assertions | Their watch mode and inline snapshots are what Veritas lacks. In exchange, Veritas declares without evaluating, guarantees declaration order under concurrency, and can list a run's selection. |
| Hypothesis | Python's property-based testing library: strategies, `assume`, a shrinking database | Hypothesis is a decade ahead on strategy composition and shrinking quality. Veritas has generators, shrinking, seeds and fake data, and the combination is young. |
| TAP and JUnit XML | Interchange formats rather than frameworks: TAP for streaming text protocols, JUnit XML for CI ingestion | Veritas writes JUnit and speaks JSON, not TAP. Adding TAP is the smallest way to join the wider ecosystem. |

What the others do that veritas could still add: pytest's fixture dependency graph and its plugin
seam, ExUnit's exclude expressions and setup callbacks, Hypothesis's strategy composition and
`assume`, Vitest's watch mode and inline snapshots, and TAP output so that any harness can consume a
run. Where veritas is ahead and should stay ahead: the lazy model means a file can be interrogated
without being run; determinism is a tested property rather than a preference, including
declaration-order results from concurrent fibers and a replayable seed; the reporter interface is a
structured event stream, which is why an editor can build a better report than the terminal; change
awareness is part of the runner rather than a wrapper; coverage comes out of Guile's own debug VM in
five formats at once; and the whole thing has three dependencies and no build system of its own.

## In the long run

The multi-year bets are about the record a run leaves rather than the assertion syntax. The first is
that **a run is data**: results, attempt counts, durations, coverage, traces and the selected set are
all files with stable shapes, and everything else — editors, CI, dashboards — is a reader. The second
is that **selection becomes the default**: once the impact graph, partitions and diff coverage are
trustworthy, running the whole suite is the fallback for the rare change that touches everything, and
the framework's value shifts from "runs your tests" to "knows which tests matter". The third is that
**property-based testing absorbs the table**: `test-each` and `property` are two answers to the same
question, and the honest long-run shape is one generator vocabulary used for examples *and* for
inputs. The fourth is that **Guile gets the test framework it deserves** — the reputation of a
language's ecosystem is set by its testing story, and being the obvious answer for a new Guile
project is worth more than any feature list.

## What this document is for

Use it to place a proposal. New assertions, reporters, generator combinators, coverage views and
selection mechanisms are in the project's character; a plugin system that needs a package manager, a
dependency on a build tool, or a report format that cannot be read back are not. Anything that makes
a run more deterministic, more explainable or more narrowly targeted is the work this project wants.

*This document describes intent, not commitments; the code, the release notes and the issue tracker
say what is actually true today.*
