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
;; Snapshot testing: compare a value with the one recorded the first time the
;; check ran.  A missing snapshot is written and the check passes, so a new test
;; does not fail merely for being new.  A mismatch fails and shows both sides,
;; and VERITAS_UPDATE_SNAPSHOTS=true rewrites every snapshot the run reaches,
;; which is how an intended change is accepted.
;;
;; A snapshot is Scheme data, read back with `read': lists, vectors, strings,
;; numbers, symbols and alists travel exactly.  Procedures, records and ports do
;; not, so snapshot the string they print instead.
;;
;;; Code:

(define-module (veritas snapshot)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (veritas prelude)
  #:use-module (veritas reporter)
  #:export (assert-snapshot
                    snapshot-path
                    snapshot-update?))

(define (snapshot-update?)
  "Return @code{#t} when snapshots are to be rewritten instead of compared.

Set @code{VERITAS_UPDATE_SNAPSHOTS} to @code{true} to accept the current value
of every snapshot a run reaches."
  (let ((flag (getenv-or 'VERITAS_UPDATE_SNAPSHOTS
                         #:default #f)))
    (or (equal? "true" flag)
        (equal? "1" flag))))

(define (snapshot-key name)
  "Return @var{name} as a file-name-safe key.

Everything outside @code{a-z} and @code{0-9} becomes a dash and runs of dashes
collapse, so a name can read as a sentence and still name exactly one file."
  (let loop ((chars (string->list (string-downcase name)))
             (out '())
             (last-dash? #t))
    (if (null? chars)
        (list->string (reverse (if (and (pair? out)
                                        (char=? (car out) #\-))
                                   (cdr out) out)))
        (let ((c (car chars)))
          (cond ((or (char-alphabetic? c) (char-numeric? c))
                 (loop (cdr chars) (cons c out) #f))
                (last-dash? (loop (cdr chars) out #t))
                (else (loop (cdr chars) (cons #\- out) #t)))))))

(define* (snapshot-path name #:key dir)
  "Return the file holding the snapshot called @var{name}.

@var{dir} overrides @code{VERITAS_SNAPSHOT_DIR}, which itself defaults to
@code{test/snapshots}."
  (string-append (or dir
                     (getenv-or 'VERITAS_SNAPSHOT_DIR
                                #:default "test/snapshots"))
                 "/"
                 (snapshot-key name)
                 ".snap"))

(define (ensure-directories! path)
  "Create every directory in front of @var{path}, ignoring existing ones."
  (let* ((dir (dirname path))
         (absolute? (string-prefix? "/" dir))
         (parts (remove string-null? (string-split dir #\/))))
    (let loop ((ps parts)
               (so-far (if absolute? "/" "")))
      (unless (null? ps)
        (let ((next (cond ((string=? so-far "") (car ps))
                          ((string=? so-far "/") (string-append "/" (car ps)))
                          (else (string-append so-far "/" (car ps))))))
          (catch #t (lambda () (mkdir next)) (lambda (key . args) #f))
          (loop (cdr ps) next))))))

(define (read-snapshot path)
  "Return @code{(value . #f)} for the snapshot at @var{path}.

Anything that cannot be read comes back as @code{(#f . message)} instead of
raising, so a damaged snapshot is reported as a failed assertion."
  (catch #t
         (lambda ()
           (call-with-input-file path
             (lambda (port)
               (let ((value (read port)))
                 (if (eof-object? value)
                     (cons #f "the snapshot file is empty")
                     (cons value #f))))))
         (lambda (key . args)
           (cons #f (format #f "~a: ~a" key args)))))

(define* (assert-snapshot #:key got name dir)
  "Assert that @var{got} matches the snapshot recorded under @var{name}.

@var{name} labels the assertion and names the file, so it is required; the file
lives under @code{VERITAS_SNAPSHOT_DIR} (default @code{test/snapshots}) or under
@var{dir}.  The first run writes the snapshot and passes.  Later runs compare
with @code{equal?}, and a mismatch fails with both values.  A snapshot that
cannot be read is a failure too, never a silent rewrite.

A @var{got} that is a procedure is called with no arguments when the assertion
runs, so state that only exists at test time can be snapshotted.

@example
(assert-snapshot #:name \"the rendered cart\"
                 #:got (lambda () (render (make-cart))))
@end example"
  (if (not name)
      (error "assert-snapshot needs a #:name: it labels the assertion and names the snapshot file"))
  (let ((path (snapshot-path name #:dir dir)))
    (lambda (vc)
      (log-assert vc name)
      (let* ((value (if (procedure? got) (got) got))
             (exists? (file-exists? path))
             (update? (snapshot-update?)))
        (cond
          ((or (not exists?) update?)
           (ensure-directories! path)
           (call-with-output-file path
             (lambda (port)
               (write value port)
               (newline port)))
           (log-pass vc (format #f "~a snapshot ~a"
                                (if exists? "updated" "wrote") path))
           `((assertion-successful unquote #t)
             (assertion-name unquote name)))
          (else
           (let ((stored (read-snapshot path)))
             (cond
               ((not (car stored))
                (log-fail vc (format #f "could not read snapshot ~a: ~a"
                                     path (cdr stored)))
                `((assertion-successful unquote #f)
                  (assertion-name unquote name)))
               ((equal? (car stored) value)
                (log-pass vc (format #f "matches snapshot ~a" path))
                `((assertion-successful unquote #t)
                  (assertion-name unquote name)))
               (else
                (log-fail vc (format #f
                                     "snapshot mismatch for ~a\nexpected: ~s\n     got: ~s\n(rerun with VERITAS_UPDATE_SNAPSHOTS=true to accept the new value)"
                                     path (car stored) value))
                `((assertion-successful unquote #f)
                  (assertion-name unquote name)))))))))))
