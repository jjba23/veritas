;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit property)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (veritas property)
  #:use-module (veritas model)
  #:use-module (veritas reporter)
  #:export (spec))

;; Properties are ordinary tasks, so a test can run one directly against a
;; quiet client and inspect the result instead of letting a failing property
;; fail the suite that is checking it.
(define vc
  (list (cons 'reporter
              'monotone-reporter)))

(define (contains? text needle)
  (and (string-contains text needle) #t))

(define (run-task task)
  (let* ((port (open-output-string))
         (result (with-output-to-port port
                                      (lambda ()
                                        (task vc)))))
    (cons result
          (get-output-string port))))

(define reverse-property
  (property "reverse of reverse is the original list"
            #:generators (list (generate-list (generate-integer #:max-bound 50)))
            #:thunk (lambda (xs)
                      (assert-equal #:expect xs
                                    #:got (reverse (reverse xs))))
            #:seed 42
            #:runs 25))

(define under-fifty-property
  (property "generated naturals stay under fifty"
            #:generators (list (generate-natural #:max-bound 99))
            #:thunk (lambda (n)
                      (assert-true (< n 50)
                                   #:name (format #f "~a is under 50" n)))
            #:seed 7
            #:runs 100))

(define property-suite
  (suite "properties"
         (test "a property is a test task"
               (assert-equal #:expect 'test
                             #:got (procedure-property reverse-property
                                                       'veritas-task-type)))
         (test "a property that holds reports a passing assertion"
               (let* ((outcome (run-task reverse-property))
                      (result (car outcome))
                      (assertions (pre-test-result-results result)))
                 (assert-equal #:expect
                               "reverse of reverse is the original list"
                               #:got (pre-test-result-name result))
                 (assert-equal #:expect 1
                               #:got (length assertions))
                 (assert-true (assertion-result-successful (car assertions)))
                 (assert-true (contains? (cdr outcome) "held for 25 cases"))))
         (test "a property that fails reports the shrunk input"
               (let* ((outcome (run-task under-fifty-property))
                      (result (car outcome))
                      (assertion (car (pre-test-result-results result)))
                      (detail (assertion-result-output assertion)))
                 (assert-false (assertion-result-successful assertion))
                 (assert-true (contains? detail "failed on input"))
                 (assert-true (contains? detail "shrunk from"))
                 (assert-true (contains? detail "50"))
                 (assert-true (contains? detail "is under 50"))))))

(define (spec)
  (veritas-run #:debug? #f property-suite))
