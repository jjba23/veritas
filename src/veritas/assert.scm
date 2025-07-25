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

(define* (assert-equal* #:key expect got compare name)
  (lambda (vc)
    (let* ((gott (if (and (list? got)
                          (not (null? got))
                          (or (equal? 'compute
                                      (car got))
                              (equal? 'comp
                                      (car got))
                              (equal? 'computare
                                      (car got))))
                     (begin
                       (eval (cadr got)
                             (interaction-environment))) got)))
      (when name
        (log-assert vc name))
      (if (compare expect gott)
          (begin
            (log-pass vc
                      (format #f "expect: ~a, got: ~a" expect gott))
            `((assertion-successful unquote #t)
              (assertion-name unquote name)))
          (begin
            (log-fail vc
                      (format #f "error, expect: ~a, got: ~a" expect gott))
            `((assertion-successful unquote #f)
              (assertion-name unquote name)))))))

(define* (assert-error thunk
                       #:key name)
  (lambda (vc)
    (when name
      (apply (assoc-ref (assoc-ref vc
                                   'reporter)
                        'log-assert)
             (list name)))
    (catch #t
           (lambda ()
             (apply thunk
                    '())
             (log-fail vc "did not catch any error")
             `((assertion-successful unquote #f)
               (assertion-name unquote name)))
           (lambda (key . args)
             (log-pass vc
                       (format #f "success catching error: ~a: ~a" key args))
             `((assertion-successful unquote #t)
               (assertion-name unquote name))))))

(define* (assert-no-error thunk
                          #:key name)
  (lambda (vc)
    (when name
      (apply (assoc-ref (assoc-ref vc
                                   'reporter)
                        'log-assert)
             (list name)))
    (catch #t
           (lambda ()
             (apply thunk
                    '())
             (log-pass vc "success running without errors")
             `((assertion-successful unquote #t)
               (assertion-name unquote name)))
           (lambda (key . args)
             (log-fail vc
                       (format #f "caught error: ~a: ~a" key args))
             `((assertion-successful unquote #f)
               (assertion-name unquote name))))))

(define* (assert-equal #:key expect got name)
  (assert-equal* #:expect expect
                 #:got got
                 #:compare equal?
                 #:name name))

(define* (assert-eqv #:key expect got name)
  (assert-equal* #:expect expect
                 #:got got
                 #:compare eqv?
                 #:name name))

(define* (assert-eq #:key expect got name)
  (assert-equal* #:expect expect
                 #:got got
                 #:compare eq?
                 #:name name))

(define* (assert-true x
                      #:key name)
  (assert-equal* #:got x
                 #:expect #t
                 #:compare eq?
                 #:name name))

(define* (assert-false x
                       #:key name)
  (assert-equal* #:got x
                 #:expect #f
                 #:compare eq?
                 #:name name))

