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

(define-module (veritas unit container)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (veritas veritas)
  #:use-module (veritas container)
  #:use-module (veritas reporter)
  #:export (spec))

;; Instead of requiring a real Docker/Podman daemon, these tests point the
;; runtime at a small fake executable that records its invocations and replies
;; with canned answers.  This exercises command construction, parsing and the
;; scoped cleanup logic deterministically.

(define fake-runtime "/tmp/veritas-fake-container-runtime.sh")
(define fake-log "/tmp/veritas-fake-container-runtime.log")

(define (install-fake-runtime!)
  (call-with-output-file fake-runtime
    (lambda (port)
      (for-each (lambda (line)
                  (display line port)
                  (newline port))
                (list "#!/bin/sh"
                      "printf '%s\\n' \"$*\" >> \"$FAKE_DOCKER_LOG\""
                      "case \"$1\" in"
                      "  run) echo fake-container ;;"
                      "  inspect) echo true ;;"
                      "  exec) echo exec-ok ;;"
                      "  logs) echo line1; echo ready ;;"
                      "  rm|stop) echo removed ;;"
                      "  build) echo built ;;"
                      "  *) : ;;"
                      "esac"
                      "exit 0"))))
  (system* "chmod" "+x" fake-runtime))

(install-fake-runtime!)
(setenv "VERITAS_CONTAINER_RUNTIME" fake-runtime)
(setenv "FAKE_DOCKER_LOG" fake-log)

(define lifecycle-suite
  (suite "container lifecycle against a fake runtime"
         (test "runtime detection honours the environment"
               (assert-equal #:expect fake-runtime #:got (container-runtime))
               (assert-true (container-runtime-available?)))
         ;; Both values are folded into a single direct assertion: assertions
         ;; must be direct arguments of `test', so they cannot be spread inside
         ;; a `call-with-values' continuation.
         (test "run-command reports status and output"
               (assert-equal #:expect '(3 "hi\n")
                             #:got (call-with-values
                                       (lambda ()
                                         (run-command (list "sh" "-c"
                                                            "echo hi; exit 3")))
                                     (lambda (status output)
                                       (list status output)))))
         (test "run-command! raises on failure"
               (assert-raises (lambda () (run-command! (list "sh" "-c" "exit 7")))))
         (test "container-build shells out to the runtime"
               (assert-equal #:expect "built"
                             #:got (string-trim-both
                                    (container-build #:tag "demo" #:context "."))))
         (test "container-run returns an id"
               (assert-equal #:expect "fake-container"
                             #:got (container-run #:image "alpine")))
         (test "container-running? detects a running container"
               (assert-true (container-running? "fake-container")))
         (test "container-exec returns output"
               (assert-equal #:expect "exec-ok"
                             #:got (container-exec "fake-container" '("echo" "hi"))))
         (test "container-logs returns logs"
               (assert-matches #:pattern "ready"
                               #:got (container-logs "fake-container")))
         (test "wait-for-log succeeds on a matching line"
               (assert-true (container-wait-for-log "fake-container" "ready"
                                                    #:timeout 2)))
         (test "wait-for-command succeeds"
               (assert-true (container-wait-for-command "fake-container"
                                                        '("true")
                                                        #:timeout 2)))
         (test "assert-container-running reports success"
               (assert-container-running #:container "fake-container"
                                         #:name "fake is running"))
         (test "with-container cleans up after the thunk"
               (when (file-exists? fake-log)
                 (delete-file fake-log))
               (assert-equal #:expect "fake-container"
                             #:got (with-container (lambda (c) c)
                                    #:image "alpine"))
               (assert-matches #:pattern "rm -f fake-container"
                               #:got (call-with-input-file fake-log
                                       get-string-all)))))

;; Command construction, status parsing and readiness helpers, all driven
;; against the fake runtime installed above.
(define command-suite
  (suite "container command construction and readiness"
         (test "container-run forwards name, env, ports and volumes"
               (when (file-exists? fake-log)
                 (delete-file fake-log))
               (container-run #:image "alpine"
                              #:name "svc"
                              #:env '("A=1")
                              #:ports '("8080:80")
                              #:volumes '("/tmp:/data"))
               (assert-contains #:expect (call-with-input-file fake-log
                                           get-string-all)
                                #:got "--name svc")
               (assert-contains #:expect (call-with-input-file fake-log
                                           get-string-all)
                                #:got "-e A=1")
               (assert-contains #:expect (call-with-input-file fake-log
                                           get-string-all)
                                #:got "-p 8080:80")
               (assert-contains #:expect (call-with-input-file fake-log
                                           get-string-all)
                                #:got "-v /tmp:/data"))
         (test "container-exec-status returns status and output"
               (call-with-values
                   (lambda ()
                     (container-exec-status "fake-container" '("true")))
                 (lambda (status output)
                   (assert-equal #:expect 0 #:got status)
                   (assert-matches #:pattern "exec-ok" #:got output))))
         (test "wait-until returns as soon as the predicate holds"
               (assert-true (wait-until (lambda ()
                                          #t)
                                        #:timeout 1)))
         (test "wait-until raises once the timeout is exceeded"
               (assert-raises (lambda ()
                                (wait-until (lambda ()
                                              #f)
                                            #:timeout 0.1
                                            #:interval 0.05))))))

(define (spec)
  (veritas-run #:debug? #f lifecycle-suite command-suite))
