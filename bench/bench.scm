;; veritas — the hot paths, measured.
;;
;; Run with:
;;   GUILE_LOAD_PATH=src guile -L ./src bench/bench.scm
;;
;; Three numbers matter for a test framework: what declaring a test costs, what
;; running an assertion costs, and what turning the results into a report costs.
;; Each is measured here on its own, in microseconds per operation.

(define-module (bench bench)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (veritas model)
  #:use-module (veritas prelude)
  #:use-module (veritas reporter))

(define vc '((reporter . monotone-reporter)))

(define (microseconds)
  (let ((now (gettimeofday)))
    (+ (* (car now) 1000000) (cdr now))))

(define* (measure name count thunk)
  "Run @var{thunk} @var{count} times and report microseconds per iteration."
  (let* ((started (microseconds))
         (_ (let loop ((i 0))
              (unless (= i count)
                (thunk)
                (loop (+ i 1)))))
         (elapsed (- (microseconds) started)))
    (format #t "~a: ~,3f us/op (~a ops in ~,1f ms)~%"
            name (/ (exact->inexact elapsed) count) count
            (/ elapsed 1000.0))
    (force-output)))

;; ====== 1. Declaration ======
;; A test that is declared and never run should cost a closure and nothing else.

(define %test
  (module-ref (resolve-module '(veritas veritas)) '%test))

(measure "declare test" 2000
         (lambda ()
           (%test "a declared test" (lambda (vc) '()))))

;; ====== 2. Assertion execution ======
;; One assertion, built and run, with the reporter writing to a null port.

(define (run-one-assertion)
  (with-output-to-port (%null-port)
                       (lambda ()
                         ((assert-equal #:expect 1 #:got 1) vc))))

(define (%null-port)
  (open-output-string))

(measure "run assertion" 20000 run-one-assertion)

;; ====== 3. Report generation ======

(define (case-tree count)
  "Return a result tree holding @var{count} test cases, ten per suite."
  (let loop ((left count) (suites '()))
    (if (<= left 0)
        (reverse suites)
        (let* ((n (min 10 left))
               (tests (map (lambda (i)
                             (make-test-result
                              i ""
                              (make-pre-test-result
                               (format #f "test ~a" i)
                               (list (make-assertion-result 0 "" "check" #t))
                               #:file "test/example.scm"
                               #:duration 0.001)))
                           (iota n))))
          (loop (- left n)
                (cons (make-suite-result (format #f "suite ~a" (length suites))
                                         tests "")
                      suites))))))

(define cases (or (and=> (getenv "BENCH_CASES") string->number) 1000))

(define tree (case-tree cases))

(measure (format #f "junit report data (~a cases)" cases) 5
         (lambda () (junit-report-data tree #:seconds 1)))

(measure (format #f "result counts (~a cases)" cases) 20
         (lambda () (result-counts tree #:files 1)))
