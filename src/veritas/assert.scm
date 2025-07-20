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

(define-module (veritas assert)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (veritas reporter)
  #:export (assert-equal assert-eq
                         assert-eqv
                         assert-true
                         assert-false
                         assert-error
                         assert-no-error))

(define* (assert-equal* #:key expected sut compare name)
  (lambda (reporter)
    (when name
      (apply (assoc-ref reporter
                        'log-assert)
             (list name)))

    (if (compare expected sut)
        (begin
          (apply (assoc-ref reporter
                            'log-pass)
                 (list (format #f "expected: ~a, sut: ~a" expected sut)))
          `((assertion-successful unquote #t)
            (assertion-name unquote name)))
        (begin
          (apply (assoc-ref reporter
                            'log-fail)
                 (list (format #f "error, expected: ~a, sut: ~a" expected sut)))

          `((assertion-successful unquote #f)
            (assertion-name unquote name))))))

(define* (assert-error thunk
                       #:key name)
  (lambda (reporter)
    (when name
      (apply (assoc-ref reporter
                        'log-assert)
             (list name)))
    (catch #t
           (lambda ()
             (apply thunk
                    '())
             (apply (assoc-ref reporter
                               'log-fail)
                    (list "did not catch any error")) #f)
           (lambda (key . args)
             (apply (assoc-ref reporter
                               'log-pass)
                    (list (format #f "success catching error: ~a: ~a" key args)))
             #t))))

(define* (assert-no-error thunk
                          #:key name)
  (lambda (reporter)
    (when name
      (apply (assoc-ref reporter
                        'log-assert)
             (list name)))
    (catch #t
           (lambda ()
             (apply thunk
                    '())
             (apply (assoc-ref reporter
                               'log-pass)
                    (list "success running without errors")) #t)
           (lambda (key . args)
             (apply (assoc-ref reporter
                               'log-fail)
                    (list (format #f "caught error: ~a: ~a" key args))) #f))))

(define* (assert-equal #:key expected sut name)
  (assert-equal* #:expected expected
                 #:sut sut
                 #:compare equal?
                 #:name name))

(define* (assert-eqv #:key expected sut name)
  (assert-equal* #:expected expected
                 #:sut sut
                 #:compare eqv?
                 #:name name))

(define* (assert-eq #:key expected sut name)
  (assert-equal* #:expected expected
                 #:sut sut
                 #:compare eq?
                 #:name name))

(define* (assert-true x
                      #:key name)
  (assert-equal* #:sut x
                 #:expected #t
                 #:compare eq?
                 #:name name))

(define* (assert-false x
                       #:key name)
  (assert-equal* #:sut x
                 #:expected #f
                 #:compare eq?
                 #:name name))
