;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit batteries)
  #:use-module (ice-9 format)
  #:use-module (veritas all)
  #:export (spec))

;; One import should be enough: the runner, the assertions, the doubles and the
;; snapshot helpers all reachable from (veritas all).

(define batteries-suite
  (suite "batteries included"
         #:shuffle? #f
         (test "one import reaches the assertion library"
               (assert-equal #:expect 2 #:got (lazy (+ 1 1)))
               (assert-equal #:expect '(1 2)
                             #:got (list 1 2)))

         (test "one import reaches the runner"
               (assert-true (procedure? veritas-run))
               (assert-true (procedure? %test)))

         (test "one import reaches the doubles"
               (assert-true (procedure? make-stub))
               (assert-true (procedure? make-spy)))

         (test "one import reaches the table helpers"
               (assert-true (procedure? cases->table))
               (assert-true (procedure? test-each))
               (assert-true (procedure? suite-each)))

         (test "one import reaches the generators"
               (assert-true (procedure? generate-integer))
               (assert-true (procedure? generate-string)))

         (test "one import reaches the BDD steps"
               (assert-true (procedure? %scenario))
               (assert-true (procedure? %step)))

         (test "one import reaches the snapshot helpers"
               (assert-true (procedure? assert-snapshot))
               (assert-true (procedure? snapshot-path)))

         (test "one import reaches the trace recorder"
               (assert-true (parameter? veritas-tracing?))
               (assert-true (procedure? read-trace))
               (assert-true (procedure? render-trace)))

         (test "one import reaches the retry count"
               (assert-true (parameter? veritas-retries)))))

(define (spec)
  (veritas-run #:debug? #f
               #:shuffle? #f
               batteries-suite))
