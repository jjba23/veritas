;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit container)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (fibers)
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
(define fake-state-dir (format #f "/tmp/veritas-fake-container-state-~a"
                               (getpid)))

(define (install-fake-runtime!)
  (call-with-output-file fake-runtime
    (lambda (port)
      (for-each (lambda (line)
                  (display line port)
                  (newline port))
                (list "#!/bin/sh"
                      "printf '%s\\n' \"$*\" >> \"$FAKE_DOCKER_LOG\""
                      "mkdir -p \"$FAKE_CONTAINER_STATE\""
                      ;; `run' answers with its --name (or the fixed id), and
                      ;; every other command names the container last.  A
                      ;; removed container leaves a marker behind, so `inspect'
                      ;; can answer for a container that was never started in
                      ;; this process - the tests of this file run concurrently
                      ;; and in shuffled order.  The markers live in a directory
                      ;; of this process, so one run cannot decide what the next
                      ;; one sees.
                      "id=fake-container"
                      "case \"$1\" in"
                      "  run)"
                      "    prev=''"
                      "    for arg in \"$@\"; do"
                      "      if [ \"$prev\" = \"--name\" ]; then id=\"$arg\"; fi"
                      "      prev=\"$arg\""
                      "    done"
                      "    rm -f \"$FAKE_CONTAINER_STATE/$id.removed\""
                      "    echo \"$id\" ;;"
                      "  inspect)"
                      "    for arg in \"$@\"; do id=\"$arg\"; done"
                      "    if [ -f \"$FAKE_CONTAINER_STATE/$id.removed\" ]; then echo false; else echo true; fi ;;"
                      "  exec) echo exec-ok ;;"
                      "  logs) echo line1; echo ready ;;"
                      "  rm|stop)"
                      "    for arg in \"$@\"; do id=\"$arg\"; done"
                      "    touch \"$FAKE_CONTAINER_STATE/$id.removed\""
                      "    echo removed ;;"
                      "  build) echo built ;;"
                      "  *) : ;;"
                      "esac"
                      "exit 0"))))
  ;; `chmod' rather than `(system* "chmod" ...)': this runs while the module is
  ;; being loaded, and spawning a process from inside a module load deadlocks
  ;; on some Guile builds (3.0.9 among them).
  (chmod fake-runtime #o755))

(install-fake-runtime!)
(setenv "VERITAS_CONTAINER_RUNTIME" fake-runtime)
(setenv "FAKE_DOCKER_LOG" fake-log)
(setenv "FAKE_CONTAINER_STATE" fake-state-dir)

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
               ;; The fake runtime answers from a marker, so the container the
               ;; test asks about is one this test started.
               (container-run #:image "alpine" #:name "running-probe")
               (assert-true (container-running? "running-probe")))
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
               (container-run #:image "alpine" #:name "assert-running-probe")
               (assert-container-running #:container "assert-running-probe"
                                         #:name "the probe is running"))
         (test "with-container cleans up after the thunk"
               (when (file-exists? fake-log)
                 (delete-file fake-log))
               (assert-equal #:expect "fake-container"
                             #:got (with-container (lambda (c) c)
                                    #:image "alpine"))
               (assert-matches #:pattern "rm -f fake-container"
                               #:got (call-with-input-file fake-log
                                       get-string-all)))
         ;; A test may sleep or reach for a port, which suspends its fiber.
         ;; Fibers unwinds and re-enters a plain `dynamic-wind' around a
         ;; suspension, so a cleanup written that way runs in the middle of the
         ;; thunk and the container is gone before the test has finished with
         ;; it.  The fake runtime answers `inspect' from a per-name marker, so
         ;; asking whether this container still runs is enough to tell the two
         ;; behaviours apart - and the name keeps the answer clear of the
         ;; containers the sibling tests start.
         (test "a yielding thunk keeps its container until it is done"
               (when (file-exists? fake-log)
                 (delete-file fake-log))
               (assert-true (with-container (lambda (c)
                                              (sleep 0.05)
                                              (container-running? c))
                                            #:image "alpine"
                                            #:name "yield-container")
                            #:name "the container is still running after the thunk yielded"))))

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
