;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A short explanation of where two values differ, for an assertion that
;; failed.  The expect/got pair already says what the values were; this says
;; which element or character went wrong, which is what a reader misses when
;; the values are long.

;;; Code:

(define-module (veritas diff)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (render-diff))

(define (list-prefix-length a b)
  "Return how many leading elements of @var{a} and @var{b} are @code{equal?}."
  (let loop
    ((a a)
     (b b)
     (n 0))
    (cond
      ((null? a)
       n)
      ((null? b)
       n)
      ((equal? (car a)
               (car b))
       (loop (cdr a)
             (cdr b)
             (+ n 1)))
      (else n))))

(define (string-prefix-length a b)
  "Return how many leading characters of strings @var{a} and @var{b} are @code{char=?}."
  (let ((end (min (string-length a)
                  (string-length b))))
    (let loop
      ((i 0))
      (if (and (< i end)
               (char=? (string-ref a i)
                       (string-ref b i)))
          (loop (+ i 1)) i))))

(define (render-sequence-diff kind expect got)
  "Return the first difference between lists @var{expect} and @var{got}.

@var{kind} names the sequences in the message, so a vector can say
@code{vectors} while being compared as lists."
  (let* ((i (list-prefix-length expect got))
         (expected-length (length expect))
         (got-length (length got)))
    (cond
      ((and (< i expected-length)
            (< i got-length))
       (format #f
        "  ~a differ at index ~a (expected ~s, got ~s); lengths ~a and ~a"
        kind
        i
        (list-ref expect i)
        (list-ref got i)
        expected-length
        got-length))
      ((< expected-length got-length)
       (format #f
        "  ~a are equal for their first ~a element~a; expected ~a element~a, got ~a, the longer continues with ~s"
        kind
        i
        (if (= i 1) "" "s")
        expected-length
        (if (= expected-length 1) "" "s")
        got-length
        (drop got i)))
      ((< got-length expected-length)
       (format #f
        "  ~a are equal for their first ~a element~a; expected ~a element~a, got ~a, the longer continues with ~s"
        kind
        i
        (if (= i 1) "" "s")
        expected-length
        (if (= expected-length 1) "" "s")
        got-length
        (drop expect i)))
      (else #f))))

(define (render-string-diff expect got)
  "Return the first difference between strings @var{expect} and @var{got}."
  (let* ((i (string-prefix-length expect got))
         (expected-length (string-length expect))
         (got-length (string-length got)))
    (cond
      ((and (< i expected-length)
            (< i got-length))
       (format #f
        "  strings differ at index ~a (expected ~s, got ~s); lengths ~a and ~a"
        i
        (string-ref expect i)
        (string-ref got i)
        expected-length
        got-length))
      (else (format #f
             "  strings are equal for their first ~a character~a; lengths ~a and ~a, the longer continues with ~s"
             i
             (if (= i 1) "" "s")
             expected-length
             got-length
             (if (< expected-length got-length)
                 (substring got i)
                 (substring expect i)))))))

(define (render-diff expect got)
  "Return a short explanation of where @var{expect} and @var{got} differ.

Only strings, lists and vectors are described; @code{#f} means there is nothing
useful to add - the values are equal, or they are of a kind a positional diff
says nothing about.

@example
(render-diff '(1 2 3) '(1 2 9))
@result{} \"  lists differ at index 2 (expected 3, got 9); lengths 3 and 3\"
@end example"
  (cond
    ((equal? expect got)
     #f)
    ((and (string? expect)
          (string? got))
     (render-string-diff expect got))
    ((and (list? expect)
          (list? got))
     (render-sequence-diff "lists" expect got))
    ((and (vector? expect)
          (vector? got))
     (render-sequence-diff "vectors"
                           (vector->list expect)
                           (vector->list got)))
    (else #f)))
