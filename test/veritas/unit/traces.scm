;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit traces)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (veritas veritas)
  #:use-module (veritas trace)
  #:export (spec))

;; A trace is the run, step by step: the test starts, each assertion passes or
;; fails, the test ends.  It is what makes a failure inspectable after the fact
;; instead of only a summary line.

(define vc '((reporter . monotone-reporter)))

(define (quietly thunk)
  (with-output-to-port (open-output-string) thunk))

(define trace-path "tmp-traces-suite.scm")

(define trace-suite
  (suite "traces"
         #:shuffle? #f
         #:concurrent? #f
         (test "a traced test records its start, its assertions and its end"
               (lambda (vc)
                 (parameterize ((veritas-tracing? #t)
                                (veritas-trace-frames '()))
                   (quietly (lambda ()
                              ((test "traced"
                                     (assert-true #t)
                                     (assert-true #f))
                               vc)))
                   (let* ((frames (reverse (veritas-trace-frames)))
                          (kinds (map (lambda (frame)
                                        (assoc-ref frame 'kind))
                                      frames))
                          (details (map (lambda (frame)
                                          (assoc-ref frame 'detail))
                                        frames)))
                     ((assert-equal #:expect '(test-start assertion
                                                           assertion test-end)
                                    #:got kinds)
                      vc)
                     ((assert-true (and (member "pass" details) #t)) vc)
                     ((assert-true (and (member "fail" details) #t)) vc)
                     ((assert-equal #:expect "1 attempt(s)"
                                    #:got (car (last-pair details)))
                      vc)))))

         (test "a trace survives a round trip through a file"
               (lambda (vc)
                 (parameterize ((veritas-tracing? #t)
                                (veritas-trace-frames '()))
                   (trace-event! 'test-start "written" "first")
                   (trace-event! 'test-end "written" "last")
                   (write-trace! #:path trace-path))
                 (let ((frames (read-trace #:path trace-path)))
                   ((assert-equal #:expect 2 #:got (length frames)) vc)
                   ((assert-equal #:expect 'test-start
                                  #:got (assoc-ref (car frames) 'kind))
                    vc)
                   ((assert-equal #:expect 'test-end
                                  #:got (assoc-ref (cadr frames) 'kind))
                    vc))))

         (test "the rendered timeline names the steps"
               (lambda (vc)
                 (let ((text (with-output-to-string
                               (lambda ()
                                 (render-trace #:path trace-path)))))
                   ((assert-contains #:expect text #:got "test-start") vc)
                   ((assert-contains #:expect text #:got "test-end") vc))))

         (test "tracing off records nothing"
               (lambda (vc)
                 (parameterize ((veritas-tracing? #f)
                                (veritas-trace-frames '()))
                   (trace-event! 'test-start "ignored" "")
                   ((assert-equal #:expect '() #:got (veritas-trace-frames)) vc))))

         (test "a missing trace file reads as no frames"
               (lambda (vc)
                 ((assert-equal #:expect '()
                                 #:got (read-trace #:path "tmp-no-such-trace.scm"))
                  vc)))))

(define (spec)
  (let ((status (veritas-run #:debug? #f
                             #:shuffle? #f
                             #:concurrent? #f
                             trace-suite)))
    (when (file-exists? trace-path)
      (delete-file trace-path))
    status))
