;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Properties: one assertion, many generated inputs.  A property draws a value
;; from each of its generators, calls its thunk, and repeats; a thunk that fails
;; for one input makes the property fail, and the failing input is shrunk to the
;; smallest value that still fails before it is reported.  The seed is reported
;; too, so a failure can be replayed with VERITAS_SEED.

;;; Code:

(define-module (veritas property)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (veritas generator)
  #:use-module (veritas model)
  #:use-module (veritas prelude)
  #:use-module (veritas reporter)
  #:use-module (veritas shuffle)
  #:export (property))

(define (seed-value value)
  "Return @var{value} as an exact seed, or @code{#f} when it is not one."
  (cond
    ((not value)
     #f)
    ((number? value)
     (inexact->exact (truncate value)))
    ((string? value)
     (let ((n (string->number value)))
       (and n
            (inexact->exact (truncate n)))))
    (else #f)))

(define (property-seed seed)
  "Return the seed a property should draw from: @var{seed}, the environment, or a fresh one."
  (or (seed-value seed)
      (seed-value (getenv "VERITAS_SEED"))
      (fresh-seed)))

(define (seconds-since start)
  "Return how many seconds have passed since @var{start}, a @code{gettimeofday}."
  (let ((end (gettimeofday)))
    (+ (- (car end)
          (car start))
       (/ (- (cdr end)
             (cdr start)) 1000000.0))))

(define (assertion-cases value)
  "Return the assertion cases @var{value} holds, flattening lists of them."
  (cond
    ((null? value)
     '())
    ((list? value)
     (append-map assertion-cases value))
    ((procedure? value)
     (list value))
    (else '())))

(define (run-property-case case vc)
  "Run one assertion @var{case}, answering whether it passed.

What the case prints goes to the output port in effect, which the property
redirects into its captured output."
  (catch #t
         (lambda ()
           (let ((result (case vc
                           )))
             (if (and (list? result)
                      (assq 'assertion-successful result))
                 (and (cdr (assq 'assertion-successful result)) #t)
                 (begin
                   (format #t
                    "the property case returned ~s, which is not an assertion result~%"
                    result) #f))))
         (lambda (key . args)
           (format #t "the property raised ~a: ~s~%" key args) #f)))

(define (run-property-trial thunk vc values)
  "Run the property @var{thunk} once on @var{values}.

Returns a pair of whether it passed and what happened, the reporter output
included, so a failing run can be reported and a passing one stays quiet."
  (let ((port (open-output-string)))
    (let ((passed? (catch #t
                          (lambda ()
                            (with-output-to-port port
                                                 (lambda ()
                                                   (let ((cases (assertion-cases
                                                                 (apply thunk
                                                                  values))))
                                                     (if (null? cases)
                                                         (begin
                                                           (display
                                                            "the property built no assertions"
                                                            port) #f)
                                                         (every (lambda (case )
                                                                  (run-property-case
                                                                   case vc))
                                                                cases))))))
                          (lambda (key . args)
                            (format port "the property raised ~a: ~s~%" key
                                    args) #f))))
      (cons passed?
            (get-output-string port)))))

(define (replace-at values index value)
  "Return a copy of @var{values} with @var{value} at @var{index}."
  (append (take values index)
          (cons value
                (drop values
                      (+ index 1)))))

(define %shrink-step-limit
  ;; The walk to the smallest failing input has to end even if a shrinker keeps
  ;; offering values.  Real shrinkers halve or drop, so this is a guard, not a
  ;; budget a property normally reaches.
  2000)

(define (shrink-values generators values thunk vc)
  "Return @var{values} shrunk, one argument at a time, while the property fails.

Each candidate is tried in the order its generator offers, smallest first, and
the first one that still fails is kept; the walk resumes on that value."
  (let loop
    ((values values)
     (index 0)
     (steps 0))
    (if (or (>= index
                (length generators))
            (>= steps %shrink-step-limit)) values
        (let* ((generator (list-ref generators index))
               (current (list-ref values index))
               (candidates (catch #t
                                  (lambda ()
                                    (generator-shrink generator current))
                                  (lambda (key . args)
                                    '())))
               (smaller (remove (lambda (candidate)
                                  (equal? candidate current)) candidates)))
          (let try
            ((rest smaller))
            (cond
              ((null? rest)
               (loop values
                     (+ index 1) steps))
              ((car (run-property-trial thunk vc
                                        (replace-at values index
                                                    (car rest))))
               (try (cdr rest)))
              (else (loop (replace-at values index
                                      (car rest)) index
                          (+ steps 1)))))))))

(define* (property name
                   #:key generators
                   thunk
                   (runs 100)
                   seed
                   (shrink? #t))
  "Return a property named @var{name}, a task like a test.

@var{generators} is a non-empty list of generators, one per argument of
@var{thunk}.  Each of the @var{runs} runs draws one value from every generator
and calls the thunk with them; the thunk returns the assertions to check, as a
test body does.

@example
(property \\\"reverse of reverse is the original list\\\"
          #:generators (list (generate-list (generate-integer)))
          #:thunk (lambda (xs)
                    (assert-equal #:expect xs
                                  #:got (reverse (reverse xs)))))
@end example

A failing input is shrunk to the smallest value from the same generators that
still fails, and both values are reported.  @var{seed} makes the draw
reproducible; without one, @code{VERITAS_SEED} is used, and failing that a fresh
seed is drawn and reported with the failure."

  (unless (and (list? generators)
               (pair? generators)
               (every procedure? generators))
    (error
     "property needs #:generators, a non-empty list of generator procedures"
     generators))
  (unless (procedure? thunk)
    (error "property needs #:thunk, a procedure of the generated values" thunk))
  (unless (and (integer? runs)
               (positive? runs))
    (error "property needs a positive #:runs" runs))

  (let* ((chosen-seed (property-seed seed))
         (task (lambda (vc)
                 (log-test vc name)
                 (let* ((started (gettimeofday))
                        (rand (seed->random-state chosen-seed))
                        (failure (let loop
                                   ((i 0))
                                   (if (>= i runs) #f
                                       (let* ((values (map (lambda (generator)
                                                             (generator rand))
                                                           generators))
                                              (trial (run-property-trial thunk
                                                      vc values)))
                                         (if (car trial)
                                             (loop (+ i 1))
                                             (list values
                                                   (cdr trial)
                                                   (+ i 1))))))))
                   (if (not failure)
                       (begin
                         (log-pass vc
                                   (format #f "property held for ~a cases"
                                           runs))
                         (make-pre-test-result name
                                               (list (make-assertion-result 0
                                                      "" name #t))
                                               #:duration (seconds-since
                                                           started)))
                       (let* ((original (car failure))
                              (case-number (caddr failure))
                              (shrunk (if shrink?
                                          (shrink-values generators original
                                                         thunk vc) original))
                              (final (run-property-trial thunk vc shrunk))
                              (was-shrunk? (and shrink?
                                                (not (equal? shrunk original))))
                              (summary (string-append (format #f
                                                       "property ~s failed on input ~s~%"
                                                       name shrunk)
                                                      (if was-shrunk?
                                                          (format #f
                                                           "shrunk from ~s~%"
                                                           original) "")
                                                      (format #f
                                                       "failed after ~a of ~a cases, seed ~a~%"
                                                       case-number runs
                                                       chosen-seed)))
                              (output (string-append summary
                                                     (cdr final))))
                         (log-fail vc
                                   (format #f
                                    "property ~s failed on input ~s~@[ (shrunk from ~s)~]"
                                    name shrunk
                                    (and was-shrunk? original)))
                         (make-pre-test-result name
                                               (list (make-assertion-result 0
                                                      output
                                                      (format #f
                                                       "property ~s failed"
                                                       name) #f))
                                               #:duration (seconds-since
                                                           started))))))))
    ;; Tagged like a test, so a suite accepts it as a child and the report
    ;; counts it as one.
    (set-procedure-property! task
                             'veritas-task-type
                             'test)
    (collect-in! task) task))
