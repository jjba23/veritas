;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Generator combinators for the property-based testing subsystem.  Every
;; generator is a closure taking a random state, so the calling convention is
;; always: (lambda (rand) ...).  Sizes are picked inside the generator, which
;; saves threading a size argument through the pipeline by hand.
;;
;; A generator may also carry a shrinker: a procedure of one value that returns
;; a list of smaller replacements, smallest first.  A property uses it to walk a
;; failing input down to the smallest value that still fails.  Generators made
;; by hand need no shrinker; they simply do not shrink.

;;; Code:

(define-module (veritas generator)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:export (generate-integer
            generate-natural
            generate-real
            generate-boolean
            generate-char
            generate-list
            generate-vector
            generate-tuple
            generate-symbol
            generate-string
            generate-one-of
            generate-constant
            generate-first-name
            generate-last-name
            generate-name
            generate-email
            generate-beer
            generate-car
            generator-shrink))

(define (with-shrink generator shrink)
  "Return @var{generator} with @var{shrink} attached as its shrinker."
  (set-procedure-property! generator 'veritas-shrink shrink)
  generator)

(define (generator-shrink generator value)
  "Return smaller replacements for @var{value} taken from @var{generator}.

The list is smallest first, and is empty when the generator cannot shrink a
value - a constant generator, or one a user wrote by hand."
  (let ((shrink (and (procedure? generator)
                     (procedure-property generator 'veritas-shrink))))
    (if shrink (shrink value) '())))

(define (integer-shrinks value)
  "Return integers between zero and @var{value}, smallest first."
  (if (zero? value)
      '()
      (delete-duplicates
       (list 0
             (quotient value 2)
             (if (positive? value) (- value 1) (+ value 1)))
       =)))

(define* (generate-integer #:key (max-bound 8192))
  "Return a generator of integers between 0 and @var{max-bound} (exclusive).

Call the generator with a random state:

@example
(define gen (generate-integer #:max-bound 100))
(gen (seed->random-state 42))
@end example"
  (with-shrink
   (lambda (rand)
     (let* ((random-size (+ 1
                            (random 100 rand)))
            (bound (min (max random-size 1) max-bound)))
       (random bound rand)))
   integer-shrinks))

(define* (generate-natural #:key (max-bound 8192))
  "Return a generator of integers between 0 and @var{max-bound} inclusive."
  (with-shrink
   (lambda (rand)
     (random (+ max-bound 1) rand))
   integer-shrinks))

(define* (generate-real #:key (min -1000) (max 1000))
  "Return a generator of real numbers in the range @var{min} to @var{max}."
  (with-shrink
   (lambda (rand)
     (+ min (* (- max min)
               (/ (random 1000000 rand) 1000000.0))))
   (lambda (value)
     (if (zero? value)
         '()
         (delete-duplicates (list 0 (/ value 2)) =)))))

(define (generate-boolean)
  "Return a generator of @code{#t} and @code{#f}."
  (with-shrink
   (lambda (rand)
     (= 1 (random 2 rand)))
   (lambda (value)
     (if value '(#f) '()))))

(define (generate-char)
  "Return a generator of printable ASCII characters, 32 to 126."
  (with-shrink
   (lambda (rand)
     (integer->char (+ 32
                       (random 95 rand))))
   (lambda (c)
     (let ((n (char->integer c)))
       (if (<= n 97)
           '()
           (delete-duplicates
            (list #\a (integer->char (+ 97 (quotient (- n 97) 2))))
            char=?))))))

(define (string-shrinks value)
  "Return shorter strings than @var{value}, smallest first."
  (if (string-null? value)
      '()
      (delete-duplicates
       (append '("")
               (let ((n (string-length value)))
                 (if (> n 1)
                     (list (substring value 0 (quotient n 2))
                           (substring value 0 (- n 1)))
                     '())))
       string=?)))

(define (list-shrinks element-shrink value)
  "Return shorter lists than @var{value}, smallest first.

Elements are shrunk with @var{element-shrink}, so a list shrinks in its spine
and in its contents."
  (if (null? value)
      '()
      (append
       (list '())
       (let ((n (length value)))
         (if (> n 1)
             (list (take value (quotient n 2))
                   (drop value (quotient n 2)))
             '()))
       (append-map (lambda (i)
                     (map (lambda (smaller)
                            (append (take value i)
                                    (cons smaller (drop value (+ i 1)))))
                          (element-shrink (list-ref value i))))
                   (iota (length value))))))

(define* (generate-string #:key (min-len 0)
                          (max-len #f))
  "Return a generator of printable-ASCII strings.

Strings are @var{min-len} to @var{max-len} characters long."
  (let ((list-gen (generate-list (generate-char)
                                 #:min-len min-len
                                 #:max-len max-len)))
    (with-shrink
     (lambda (rand)
       (list->string (list-gen rand)))
     string-shrinks)))

(define* (generate-symbol #:key (min-len 1)
                          (max-len #f))
  "Return a generator of lowercase symbols, @var{min-len} to @var{max-len} long.

Symbols stay unescaped because the characters are always @code{a-z}."
  (let* ((generate-symbol-char (lambda (rand)
                                 (integer->char (+ 97
                                                   (random 26 rand)))))
         (list-gen (generate-list generate-symbol-char
                                  #:min-len min-len
                                  #:max-len max-len)))
    (with-shrink
     (lambda (rand)
       (string->symbol (list->string (list-gen rand))))
     (lambda (symbol)
       (map string->symbol
            (string-shrinks (symbol->string symbol)))))))

(define* (generate-list elem-generator
                        #:key (min-len 0)
                        (max-len #f))
  "Return a generator of lists whose elements come from @var{elem-generator}.

Lists are @var{min-len} to @var{max-len} long."
  (with-shrink
   (lambda (rand)
     (let* ((random-size (+ 1
                            (random 100 rand)))
            (current-max (if max-len
                             (min random-size max-len) random-size))
            (safe-max (max (+ min-len 1) current-max))
            (delta (- safe-max min-len))
            (target-length (+ min-len
                              (if (> delta 0)
                                  (random delta rand) 0))))
       (let loop
         ((count 0)
          (acc '()))
         (if (= count target-length) acc
             (loop (+ count 1)
                   (cons (elem-generator rand) acc))))))
   (lambda (value)
     (list-shrinks (lambda (element)
                     (generator-shrink elem-generator element))
                   value))))

(define* (generate-vector elem-generator
                          #:key (min-len 0)
                          (max-len #f))
  "Return a generator of vectors whose elements come from @var{elem-generator}."
  (let ((list-gen (generate-list elem-generator
                                 #:min-len min-len
                                 #:max-len max-len)))
    (with-shrink
     (lambda (rand)
       (list->vector (list-gen rand)))
     (lambda (value)
       (map list->vector
            (list-shrinks (lambda (element)
                            (generator-shrink elem-generator element))
                          (vector->list value)))))))

(define (generate-tuple generators)
  "Return a generator of lists holding one value from each of @var{generators}."
  (with-shrink
   (lambda (rand)
     (map (lambda (generator)
            (generator rand))
          generators))
   (lambda (values)
     (append-map (lambda (i)
                   (map (lambda (smaller)
                          (append (take values i)
                                  (cons smaller (drop values (+ i 1)))))
                        (generator-shrink (list-ref generators i)
                                          (list-ref values i))))
                 (iota (length generators))))))

(define (generate-one-of choices)
  "Return a generator that picks one of @var{choices} at random.

A choice is usually a constant, so there is nothing to shrink."
  (unless (and (list? choices) (pair? choices))
    (error "generate-one-of needs a non-empty list of choices" choices))
  (lambda (rand)
    (list-ref choices (random (length choices) rand))))

(define (generate-constant value)
  "Return a generator that always answers @var{value}."
  (lambda (rand)
    value))

;; ====== Fake data ======
;;
;; Named values are a common property input: a report that says "Ada Lovelace"
;; reads better than one that says "a7f3q".  These pick from fixed lists, so a
;; seed still reproduces the run.

(define first-names
  '("Ada" "Grace" "Alan" "Linus" "Barbara" "Edsger" "Margaret" "Ken"
    "Frances" "Dennis" "Guido" "Anita" "John" "Radia" "Donald" "Jean"
    "Niklaus" "Karen" "Leslie" "Tim"))

(define last-names
  '("Lovelace" "Hopper" "Turing" "Torvalds" "Liskov" "Dijkstra" "Hamilton"
    "Thompson" "Allen" "Ritchie" "Rossum" "Borg" "McCarthy" "Perlman"
    "Knuth" "Sammet" "Wirth" "Lamport" "Berners-Lee" "Matsumoto"))

(define beers
  '("Pale Ale" "India Pale Ale" "Pilsner" "Stout" "Porter" "Wheat Beer"
    "Saison" "Bock" "Amber Ale" "Sour Ale" "Brown Ale" "Barleywine"))

(define cars
  '("Toyota Corolla" "Honda Civic" "Ford Mustang" "Tesla Model 3"
    "Volkswagen Golf" "BMW 3 Series" "Mazda MX-5" "Subaru Outback"
    "Volvo XC90" "Fiat 500" "Porsche 911" "Jeep Wrangler"))

(define (generate-first-name)
  "Return a generator of first names."
  (generate-one-of first-names))

(define (generate-last-name)
  "Return a generator of last names."
  (generate-one-of last-names))

(define (generate-name)
  "Return a generator of full names, a first name and a last name."
  (let ((first (generate-first-name))
        (last (generate-last-name)))
    (lambda (rand)
      (format #f "~a ~a" (first rand) (last rand)))))

(define (generate-email)
  "Return a generator of @code{first.last@@example.com} email addresses."
  (let ((first (generate-first-name))
        (last (generate-last-name)))
    (lambda (rand)
      (format #f "~a.~a@@example.com"
              (string-downcase (first rand))
              (string-downcase (last rand))))))

(define (generate-beer)
  "Return a generator of beer styles."
  (generate-one-of beers))

(define (generate-car)
  "Return a generator of car makes and models."
  (generate-one-of cars))
