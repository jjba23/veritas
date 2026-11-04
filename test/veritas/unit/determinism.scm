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

(define-module (veritas unit determinism)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 textual-ports)
  #:use-module (veritas veritas)
  #:use-module (veritas model)
  #:use-module (veritas prelude)
  #:use-module (veritas reporter)
  #:export (spec))

;; Two runs of the same suite, with a fixed clock, have to produce the same
;; report.  That is what makes a snapshot of a run worth keeping, and what makes
;; a difference between two CI runs a real difference instead of the order a
;; hash table happened to be in.

(define vc
  (list (cons 'reporter 'monotone-reporter)))

(define (quietly thunk)
  (with-output-to-port (open-output-string) thunk))

(define (captured thunk)
  "Run @var{thunk} and return what it printed."
  (let ((port (open-output-string)))
    (with-output-to-port port thunk)
    (get-output-string port)))


;; ====== Fixtures ======

(define (flat-suite)
  (suite "flat"
         (test "one" (assert-true #t))
         (test "two" (assert-equal #:expect 4 #:got (+ 2 2)))
         (test "three" (assert-equal #:expect 6 #:got (* 2 3)))))

(define (nested-suite)
  (suite "outer"
         (test "before"
               (assert-equal #:expect 1 #:got 1))
         (suite "inner"
                (test "inside"
                      (assert-equal #:expect -1 #:got (- 1 2))))
         (test "after"
               (assert-equal #:expect 2 #:got 2))))

(define (indices-of result)
  (map test-result-index (suite-result-test-results result)))

(define (documents-of thunk)
  (map (lambda (i)
         (let ((result (quietly (lambda () (thunk)))))
           (junit-report-data (list result) #:seconds 1)))
       '(0 1 2)))

;; ====== Tests ======

(define determinism-suite
  (suite "determinism"
         #:shuffle? #f
         (test "a suite runs its tests in declaration order, run after run"
               (lambda (vc)
                 (let* ((first (indices-of ((flat-suite) vc)))
                        (second (indices-of ((flat-suite) vc))))
                   ((assert-equal #:expect 3 #:got (length first)) vc)
                   ((assert-equal #:expect second #:got first) vc))))

         (test "three runs of one suite produce the same JUnit document"
               (lambda (vc)
                 (let ((documents (documents-of (lambda () ((nested-suite) vc)))))
                   ((assert-equal #:expect (car documents)
                                  #:got (cadr documents))
                    vc)
                   ((assert-equal #:expect (car documents)
                                  #:got (caddr documents))
                    vc))))

         (test "timestamps come from the clock seam"
               (lambda (vc)
                 (parameterize ((veritas-clock
                                 (lambda () "2026-01-01T00:00:00Z")))
                   ((assert-equal #:expect "2026-01-01T00:00:00Z"
                                  #:got (now))
                    vc)
                   ;; A reporter's output with a pinned clock is the same bytes
                   ;; on every run, which is what a snapshot needs.
                   (let ((once (captured (lambda ()
                                          (log-fail (list (cons 'reporter
                                                                'json-reporter))
                                                    "same"))))
                         (twice (captured (lambda ()
                                           (log-fail (list (cons 'reporter
                                                                 'json-reporter))
                                                     "same")))))
                     ((assert-contains #:expect once
                                       #:got "2026-01-01T00:00:00Z")
                      vc)
                     ((assert-equal #:expect twice #:got once) vc)))))))

(define (spec)
  (veritas-run #:debug? #f
               #:shuffle? #f
               #:concurrent? #f
               determinism-suite))
