;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit mock)
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:use-module (veritas mock)
  ;; `answer' is exported so that a run-time lookup can reach it by name.
  #:export (spec answer))

(define vc (list (cons 'reporter 'monotone-reporter)))

;; Run an assertion here and now, and report whether it succeeded.  A nested
;; assertion returns a result alist rather than a boolean, so this is the way to
;; check one from inside another.
(define (ok? assertion)
  (cdr (assq 'assertion-successful (assertion vc))))

;; A module-level procedure to stand in for a dependency worth replacing.
(define (answer)
  42)

;; Captured while this file is loaded, which is when this module is the current
;; one.  At run time the current module is whoever called the runner.
(define this-module (current-module))

(define mock-suite
  (suite "test doubles"
         #:concurrent? #f
         #:shuffle? #f
         (test "a spy records the calls and the results"
               (lambda (vc)
                 (let ((add (make-spy + #:name "add")))
                   ((assert-equal #:expect 3 #:got (add 1 2)) vc)
                   ((assert-equal #:expect 7 #:got (add 3 4)) vc)
                   ((assert-true (spy? add)) vc)
                   ((assert-equal #:expect '((1 2) (3 4))
                                  #:got (spy-calls add))
                    vc)
                   ((assert-equal #:expect '(3 7) #:got (spy-results add)) vc)
                   ((assert-equal #:expect 2 #:got (spy-call-count add)) vc)
                   ((assert-true (spy-called-with? add '(1 2))) vc)
                   ((assert-false (spy-called-with? add '(2 1))) vc))))
         (test "a stub ignores its arguments and returns a value"
               (lambda (vc)
                 (let ((clock (make-stub #:returns 0 #:name "clock")))
                   ((assert-equal #:expect 0 #:got (clock)) vc)
                   ((assert-equal #:expect 0 #:got (clock "some" "arguments")) vc)
                   ((assert-equal #:expect '(() ("some" "arguments"))
                                  #:got (spy-calls clock))
                    vc)
                   ((assert-equal #:expect 2 #:got (spy-call-count clock))
                    vc))))
         (test "assertions read the spy when they run"
               (lambda (vc)
                 (let ((send (make-spy list #:name "send")))
                   ;; Nothing has been called yet, so the checks start out false.
                   ((assert-false (ok? (assert-called send))) vc)
                   (send "ada" "thanks")
                   ((assert-true (ok? (assert-called send))) vc)
                   ((assert-true (ok? (assert-called-with send '("ada" "thanks"))))
                    vc)
                   ((assert-false (ok? (assert-called-with send '("ada")))) vc)
                   ((assert-true (ok? (assert-call-count send #:expect 1))) vc)
                   ((assert-false (ok? (assert-call-count send #:expect 2))) vc))))
         (test "a spy is an ordinary procedure"
               (lambda (vc)
                 (let ((double (make-spy (lambda (x) (* x 2)))))
                   ((assert-equal #:expect '(2 4 6)
                                  #:got (map double '(1 2 3)))
                    vc)
                   ((assert-equal #:expect 3 #:got (spy-call-count double)) vc))))
         (test "a binding is replaced and restored"
               (lambda (vc)
                 (let ((original (module-ref this-module 'answer)))
                   ((assert-equal #:expect 42
                                  #:got ((module-ref this-module 'answer)))
                    vc)
                   (with-temporary-binding this-module
                                           'answer
                                           (lambda () 0)
                                           (lambda ()
                                             ((assert-equal #:expect 0
                                                            #:got ((module-ref
                                                                    this-module
                                                                    'answer)))
                                              vc)))
                   ((assert-equal #:expect 42
                                  #:got ((module-ref this-module 'answer)))
                    vc)
                   ((assert-true (eq? original
                                      (module-ref this-module 'answer)))
                    vc))))
         (test "a call that reads the binding at run time sees the replacement"
               (lambda (vc)
                 ;; A compiled call site keeps calling the original procedure -
                 ;; Guile resolves it while compiling - so this looks the binding
                 ;; up while running instead.
                 ((assert-equal #:expect 0
                                #:got (with-temporary-binding this-module
                                                              'answer
                                                              (lambda () 0)
                                                              (lambda ()
                                                                (eval '((@ (veritas
                                                                            unit
                                                                            mock)
                                                                           answer))
                                                                      this-module))))
                  vc)))
         (test "a binding is restored even when the body raises"
               (lambda (vc)
                 (catch #t
                        (lambda ()
                          (with-temporary-binding this-module
                                                  'answer
                                                  (lambda () 0)
                                                  (lambda () (error "boom"))))
                        (lambda (key . args) #f))
                 ((assert-equal #:expect 42
                                #:got ((module-ref this-module 'answer)))
                  vc)))))

(define (spec)
  (veritas-run #:debug? #f
               #:shuffle? #f
               mock-suite))
