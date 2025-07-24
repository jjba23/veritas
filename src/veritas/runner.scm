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
  #:export (run-tests))

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
    (apply spec
           '())))

(define* (getenv-or var
                    #:key (default #f))
  (let* ((v (getenv var)))
    (if (not v) default v)))

(define* (run-tests)
  (let* ((dir (getenv-or "VERITAS_RUNNER_SCAN_DIR"
                         #:default "./"))
         (test-type (getenv-or "VERITAS_RUNNER_TEST_TYPE"
                               #:default "unit"))
         (_ (begin
              (display (format #f "\nscanning directories for ~a tests..."
                               test-type))
              (display (format #f
                        "
scanning present working directory for ~a tests..."
                        test-type))
              (newline)))
         (files (remove-stat (file-system-tree dir)))
         (interesting-files (flatten-list (discover-tests test-type files))))
    
    (if (not (null? interesting-files))
        (begin
          (display (format #f "\nfound ~a tests: ~a\n\n" test-type
                           interesting-files))
          (for-each (lambda (file)
                      (run-test-file file
                                     'unit)) interesting-files))
        (begin
          (display (format #f "\ncould not find any ~a tests!" test-type))))))

(define (discover-tests test-type d)
  (cond
    ((and (list? d)
          (or (equal? "./"
                      (car d))
              (equal? "test"
                      (car d))
              (equal? "veritas"
                      (car d))))
     (discover-tests test-type
                     (cdr d)))
    ((and (list? d)
          (equal? test-type
                  (car d)))
     (map (lambda (dir)
            (format #f "~a" dir))
          (flatten-list (cdr d))))
    ((list? d)
     (map (lambda (x)
            (discover-tests test-type x)) d))
    (else '())))

