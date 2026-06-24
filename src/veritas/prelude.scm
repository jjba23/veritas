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

(define-module (veritas prelude)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (veritas model)
  #:use-module (veritas reporter)
  #:export (evaluate-thunks-concurrently evaluate-thunks
                                         flatten-list
                                         capture-result-and-output
                                         getenv-or
                                         display-all-results
                                         display-assertion-results
                                         collect-assertion-successful
                                         collect-assertion-successful*))

(define-syntax-rule (evaluate-thunks-concurrently thunks)
  "Evaluate a list of @var{thunks} concurrently using Guile Fibers.

Each zero-argument procedure in the list @var{thunks} is spawned inside its own
concurrent fiber thread. This macro blocks the execution of the calling fiber
until all spawned thunks have reported their results back through a shared channel.

Exceptions raised within individual fibers are caught safely to avoid hanging or
silent termination, returning a formatted error string instead.

Note: The output list order corresponds to the exact completion order of the
fibers, meaning it is non-deterministic.

@example
(evaluate-thunks-concurrently
 (list (lambda () (sleep 1) 'delayed)
       (lambda () 'instant)))
@result{} (instant delayed)
@end example"
  (let ((channel
          (make-channel)))
    (run-fibers (lambda ()
                  ;; Spawn producers
                  (for-each (lambda (func)
                              (spawn-fiber (lambda ()
                                             ;; Catch exceptions so the fiber doesn't die silently
                                             (catch #t
                                                    (lambda ()
                                                      (let ((result (func)))
                                                        (put-message channel
                                                                     result)))
                                                    (lambda (key . args)
                                                      ;; Send the error down the channel to prevent hanging
                                                      (put-message channel
                                                                   (format #f
                                                                    "~a: ~a"
                                                                    key args)))))))
                            thunks)

                  ;; Consume messages INSIDE the run-fibers block
                  ;;
                  ;; Note: #:drain? #t is not needed because the `map` loop implicitly
                  ;; blocks the main fiber until all thunks have reported back.
                  (map (lambda (_)
                         (get-message channel)) thunks)))))

(define-syntax-rule (evaluate-thunks thunks)
  "Evaluate a list of @var{thunks} sequentially.

Applies each zero-argument procedure in the list @var{thunks} one after another
in a synchronous manner.

Returns a list containing the evaluation results of each thunk preserving the original
order of the input list.

@example
(evaluate-thunks (list (lambda () (+ 1 1)) (lambda () (* 2 3))))
@result{} (2 6)
@end example"
  (map (lambda (t)
         (apply t
                '())) thunks))

(define (flatten-list xs)
  "Recursively flatten a nested list structure @var{xs}.

Traverses the given structure and flattens all nested sub-lists into a continuous,
one-dimensional list of elements. If @var{xs} is empty, returns an empty list. If
it is a non-list atom, returns it wrapped inside a single-element list.

@example
(flatten-list '((1 2) (3 (4 5)) 6))
@result{} (1 2 3 4 5 6)
@end example"
  (cond
    ((null? xs)
     '())
    ((pair? xs)
     (append (flatten-list (car xs))
             (flatten-list (cdr xs))))
    (else (list xs))))

(define (collect-assertion-successful* xs)
  "Recursive procedure to extract success values from test data @var{xs}.

Dispatches dynamically on the type of @var{xs} to look for assertion results:
@itemize
@item If it is an assertion result, extracts its success state boolean.
@item If it is a pre-test or test result, recursively extracts outcomes from internal fields.
@item Nested lists are mapped over recursively.
@item Unspecified values, benchmark results, and explicit @code{#t} values return empty lists.
@end itemize

Raises an error if an unknown object structure is encountered."
  (cond
    ((unspecified? xs)
     '())
    ((assertion-result? xs)
     (assertion-result-successful xs))
    ((pre-test-result? xs)
     (map collect-assertion-successful*
          (pre-test-result-results xs)))
    ((test-result? xs)
     (collect-assertion-successful* (test-result-results xs)))
    ((benchmark-result? xs)
     '())
    ((list? xs)
     (map collect-assertion-successful xs))
    ((equal? #t xs)
     '())
    (else (error (format #f
                  "could not read assertion success from unknown type: ~a" xs)))))

(define (collect-assertion-successful xs)
  "Collect and flatten all assertion success statuses found in @var{xs}.

Processes the arbitrary test result tree or list structure @var{xs}, extracts all underlying
assertion outcomes, and returns a flat list containing only booleans indicating success
(@code{#t}) or failure (@code{#f}).

@example
(collect-assertion-successful my-test-suite-record)
@result{} (#t #t #f #t)
@end example"
  (flatten-list (collect-assertion-successful* xs)))

(define (display-assertion-results assertion-results)
  "Display the captured text outputs for a list of @var{assertion-results}.

Iterates over @var{assertion-results}. For each item, validates that it is an assertion record
and dumps its tracked standard output directly to the current output port. Errors during processing
are safely intercepted and logged to stdout without crashing the reporter framework."
  (for-each (lambda (x)
              (catch #t
                     (lambda ()
                       (if (assertion-result? x)
                           (display (assertion-result-output x))
                           (error (format #f "not an assertion-result: ~a" x))))
                     (lambda (key . args)
                       (format #t "could not display assertion: ~a: ~a" key
                               args)))) assertion-results))

(define-syntax-rule (display-test-results test-results)
  "Display outputs and nested assertions for a list of @var{test-results}.

Iterates through @var{test-results}. If an item is a valid test result, it prints the
captured test output, and then processes its internal pre-test assertion data structures
by forwarding them to @code{display-assertion-results}."
  (for-each (lambda (x)
              (cond
                ((unspecified? x)
                 '())
                ((test-result? x)
                 (begin
                   (catch #t
                          (lambda ()
                            (display (test-result-output x)))
                          (lambda (key . args)
                            (format #t "~a: ~a" key args)))
                   (display-assertion-results (pre-test-result-results (test-result-results
                                                                        x))))
                 '())

                (else '()))) test-results))

(define-syntax-rule (display-all-results vc all-results)
  "Print and log every test outcome inside @var{all-results} using reporter client @var{vc}.

Iterates over the compilation of @var{all-results}. Evaluates types dynamically:
@itemize
@item @code{suite-result}: Displays suite output, followed by inner test results.
@item @code{pre-test-result}: Logs the test identity via @var{vc} and displays inner assertions.
@item @code{benchmark-result}: Dispatches to @code{log-benchmark}.
@end itemize

Throws an explicit error if an unrecognized result type is processed."
  (for-each (lambda (x)
              (cond
                ((suite-result? x)
                 (begin
                   (display (suite-result-output x))
                   (display-test-results (suite-result-test-results x))))
                ((pre-test-result? x)
                 (begin
                   (log-test vc
                             (pre-test-result-name x))
                   (display-assertion-results (pre-test-result-results x))))
                ((benchmark-result? x)
                 (log-benchmark vc x))
                (else (error (format #f
                              "could not display result of unknown type: ~a" x)))))
            all-results))

(define* (getenv-or var
                    #:key (default #f))
  "Lookup environment variable @var{var}, returning @var{default} if absent.

Converts the symbol @var{var} into a string to query the operating system's environment.
If the variable does not exist, or if an exception occurs during lookup, the specified
@var{default} keyword value is returned.

@example
(getenv-or 'GUILE_LOAD_PATH #:default \"/usr/share/guile\")
@result{} \"/usr/share/guile\"
@end example"
  (catch #t
         (lambda ()
           (let* ((v (getenv (symbol->string var))))
             (if (not v) default v)))
         (lambda (key . args)
           (display (format #f
                     "could not read environment variable ~a, return default: ~a
error: ~a: ~a"
                     var
                     default
                     key
                     args)) default)))

(define* (capture-result-and-output thunk)
  "Execute @var{thunk}, capturing both its return value and standard output.

Dynamically binds the standard output port to a string port while running @var{thunk}.
Returns a custom @code{result-and-output} object record packing the thunk's evaluation value
and any textual stdout written during runtime. Closes the internal string port safely upon completion.

@example
(capture-result-and-output (lambda () (display \"In execution...\") 100))
@result{} #<result-and-output result: 100 output: \"In execution...\">
@end example"
  (let* ((output-port (open-output-string))
         (result (with-output-to-port output-port thunk))
         (the-output (get-output-string output-port))
         (_ (close-port output-port)))
    (make-result-and-output result the-output)))
