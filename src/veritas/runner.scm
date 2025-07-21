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

(define-module (veritas runner)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (ice-9 ftw)
  #:export (run-unit-tests))

(define remove-stat
  (match-lambda
    ((name stat)
     name)
    ((name stat children ...)
     (list name
           (map remove-stat children)))))

(define (flatten-list xs)
  (cond
    ((null? xs)
     '())
    ((pair? xs)
     (append (flatten-list (car xs))
             (flatten-list (cdr xs))))
    (else (list xs))))

(define (run-test-file test-file test-type)
  (let* ((m (resolve-module (append '(veritas)
                                    (list test-type
                                          (string->symbol (string-replace-substring
                                                           test-file ".scm" ""))))))
         (spec (module-ref m
                           'spec)))
    (write m)
    (newline)
    (write spec)
    (apply spec
           '())))

(define* (run-unit-tests)
  (display "\nscanning directories for unit tests...")
  (display "
scanning present working directory for unit tests...")
  (newline)
  (let* ((dir "./")
         (files (remove-stat (file-system-tree dir)))
         (interesting-files (flatten-list (discover-unit-tests files))))
    (if (not (null? interesting-files))
        (begin
          (display (format #f "\nfound unit tests: ~a\n\n" interesting-files))
          (for-each (lambda (file)
                      (run-test-file file
                                     'unit)) interesting-files))
        (begin
          (display (format #f "\ncould not find any unit tests!"))))))

(define (discover-unit-tests d)
  (cond
    ((and (list? d)
          (or (equal? "./"
                      (car d))
              (equal? "test"
                      (car d))
              (equal? "veritas"
                      (car d))))
     (discover-unit-tests (cdr d)))
    ((and (list? d)
          (equal? "unit"
                  (car d)))
     (map (lambda (dir)
            (format #f "~a" dir))
          (flatten-list (cdr d))))
    ((list? d)
     (map discover-unit-tests d))
    (else '())))

