;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; The whole framework behind one import: the runner, the assertions, mocking
;; and spying, snapshots, table and generated cases, and the BDD aliases.  Use
;; it when a test wants several of them; a test that only asserts can keep
;; importing (veritas veritas).

(define-module (veritas all)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (veritas assert)
  #:use-module (veritas mock)
  #:use-module (veritas snapshot)
  #:use-module (veritas table)
  #:use-module (veritas generator)
  #:use-module (veritas bdd)
  #:use-module (veritas trace)
  #:re-export (lazy lazy-value? generate-integer generate-char generate-list 
              generate-symbol generate-string make-assertion assert-equal 
              assert-eq assert-eqv assert-true assert-false assert-error 
              assert-no-error assert-approximate assert-not-approximate 
              assert-not-eq assert-not-eqv assert-not-equal assert-contains 
              assert-not-contains assert-matches assert-not-matches 
              assert-raises assert-empty assert-not-empty assert-length 
              assert-in-range assert-one-of is-approximate? proba aequalis 
              idem aequivalens non-aequalis non-idem non-aequivalens verus 
              falsus propinquus? propinquus non-propinquus errat non-errat 
              suscitat continet non-continet congruit non-congruit inanis 
              non-inanis longitudo intra unus-ex 
              evaluate-thunks-concurrently collect-assertion-successful 
              call-with-timeout fisher-yates-shuffle fresh-seed 
              veritas-retries test suite %test %suite 
              split-keyword-arguments defer-body-forms veritas-run benchmark 
              do-if-string-contained current-seed seeded-shuffle 
              failed-test-names failed-test-wanted? read-last-failed 
              write-last-failed! veritas-only-failed 
              veritas-last-failed-path write-last-failed-names! 
              merge-failed-names veritas-rerun-all-marker 
              veritas-current-file veritas-junit-data-path 
              veritas-coverage-data-path veritas-result-counts 
              veritas-result-counts-data-path write-result-counts 
              read-result-counts assert make-spy make-stub spy? spy-calls 
              spy-results spy-call-count spy-called-with? assert-called 
              assert-called-with assert-call-count with-temporary-binding 
              assert-snapshot snapshot-path snapshot-update? test-each 
              suite-each cases->table scenario feature story given when-step 
              then and-step but-step Given When Then fabula scaena dato 
              quando tunc etiam sed %scenario %step %given %when-step %then 
              %and-step %but-step veritas-tracing? veritas-trace-frames 
              trace-event! trace-reset! trace-file write-trace! read-trace 
              render-trace show-last-trace ))
