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
               (assert-true (procedure? cases->table)))))

(define (spec)
  (veritas-run #:debug? #f
               #:shuffle? #f
               batteries-suite))
