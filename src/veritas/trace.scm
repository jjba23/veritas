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

(define-module (veritas trace)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:export (veritas-tracing?
            veritas-trace-frames
            trace-event!
            trace-reset!
            trace-file
            write-trace!
            read-trace
            render-trace
            show-last-trace))

;; A run is a sequence of steps: a test starts, an assertion passes or fails, the
;; test ends.  Recording those steps turns a failed run into something that can
;; be replayed in the reader: what ran, in what order, and what each step
;; reported.  A frame is a small alist, so the file stays readable with `read'
;; and diffable in a review.

(define veritas-tracing?
  (make-parameter #f))

(define veritas-trace-frames
  (make-parameter '()))

(define* (trace-event! kind name #:optional (detail ""))
  "Record one step of the run when tracing is on.

@var{kind} is a symbol such as @code{test-start}, @code{assertion} or
@code{test-end}, @var{name} is what the step is about, and @var{detail} is a
short string a reader can use to tell one frame from another."
  (when (veritas-tracing?)
    (veritas-trace-frames
     (cons (list (cons 'kind kind)
                 (cons 'name name)
                 (cons 'detail detail)
                 (cons 'at (get-internal-real-time)))
           (veritas-trace-frames)))))

(define (trace-reset!)
  "Forget the frames recorded so far."
  (veritas-trace-frames '()))

(define (trace-file)
  "Return where the last trace of this run is written."
  "reports/traces/last.scm")

(define (ensure-directory! path)
  (unless (file-exists? path)
    (catch #t
      (lambda ()
        (mkdir path))
      (lambda args #f))))

(define* (write-trace! #:key (path (trace-file)))
  "Write the frames recorded so far to @var{path} and return it.

The newest frame is last in the file, so the timeline reads top to bottom."
  (ensure-directory! "reports")
  (ensure-directory! "reports/traces")
  (call-with-output-file path
    (lambda (port)
      (pretty-print (reverse (veritas-trace-frames)) port)))
  path)

(define* (read-trace #:key (path (trace-file)))
  "Return the frames stored at @var{path}, oldest first."
  (catch #t
    (lambda ()
      (call-with-input-file path read))
    (lambda args '())))

(define* (render-trace #:key (path (trace-file)) (port (current-output-port)))
  "Print the trace at @var{path} as a numbered timeline."
  (let ((frames (read-trace #:path path)))
    (if (null? frames)
        (format port "no trace at ~a; run with --trace first~%" path)
        (begin
          (format port "trace: ~a (~a frames)~%~%" path (length frames))
          (let loop ((rest frames) (index 1))
            (unless (null? rest)
              (let* ((frame (car rest))
                     (kind (assoc-ref frame 'kind))
                     (name (assoc-ref frame 'name))
                     (detail (assoc-ref frame 'detail)))
                (format port "  ~a. ~a  ~a~@[  (~a)~]~%"
                        index kind name
                        (and detail (not (string-null? detail)) detail))
                (loop (cdr rest) (+ index 1)))))))))

(define (show-last-trace)
  "Print the trace of the last run, and return whether there was one."
  (if (file-exists? (trace-file))
      (begin
        (render-trace)
        #t)
      (begin
        (format #t "no trace at ~a; run with --trace first~%" (trace-file))
        #f)))
