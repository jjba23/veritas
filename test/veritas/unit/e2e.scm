;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit e2e)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 textual-ports)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module ((ice-9 threads) #:select (call-with-new-thread))
  #:use-module (veritas veritas)
  #:use-module (veritas container)
  #:use-module (veritas e2e)
  #:export (spec))

;; The services these tests start are real processes: a shell that prints one
;; line and then waits, and a Guile web server that answers the HTTP calls.  The
;; stub lives in a file of its own because its command line would otherwise be a
;; quoted string inside a quoted string.

(define stub-script (format #f "/tmp/veritas-e2e-stub-~a.scm" (getpid)))

(define (install-stub!)
  (call-with-output-file stub-script
    (lambda (port)
      (for-each (lambda (line)
                  (display line port)
                  (newline port))
                (list "(use-modules (web server) (web request) (web response)"
                      "             (web uri) (ice-9 match))"
                      "(define port (string->number (cadr (command-line))))"
                      "(define (handler request body)"
                      "  (match (uri-path (request-uri request))"
                      "    (\"/health\" (values (build-response #:code 200) \"ok\"))"
                      "    (\"/json\" (values (build-response"
                      "                       #:code 200"
                      "                       #:headers '((content-type . (application/json))))"
                      "                     \"{\\\"ok\\\":true}\"))"
                      "    (_ (values (build-response #:code 404) \"not found\"))))"
                      "(run-server handler 'http (list #:port port))")))))

(install-stub!)

(define (scratch-file name)
  "Return a path under /tmp, one per process, for @var{name}."
  (format #f "/tmp/veritas-e2e-~a-~a" name (getpid)))

(define (file-text path)
  (if (file-exists? path)
      (call-with-input-file path get-string-all)
      ""))

(define (process-alive? pid)
  "Return @code{#t} when a process with @var{pid} exists."
  (catch #t (lambda ()
              (kill pid 0) #t)
         (lambda (key . args) #f)))

(define* (shell-service #:key name script log-path wait)
  "Return a service that runs @var{script} in a shell, with @var{name}."
  (make-service #:name name
                #:command (list "sh" "-c" script)
                #:log-path log-path
                #:wait wait))

(define (start-registry-observer)
  "Return a thread that reports the registry once, as two channels.

The thread is started before any service is, so what it reads is the registry
itself rather than what its creator's dynamic extent happened to hold."
  (let ((ask (make-channel))
        (answer (make-channel)))
    (call-with-new-thread (lambda ()
                            (get-message ask)
                            (put-message answer
                                         (map service-instance-name
                                              (running-services)))))
    (cons ask answer)))

;; The spec of a service, and the ports handed out for one, are values: nothing
;; starts here.

(define spec-suite
  (suite "e2e service specifications"
         (test "a spec keeps what it was given"
               (let ((spec (make-service #:name "api"
                                         #:command '("app" "--serve")
                                         #:dir "/srv/app"
                                         #:env '("PORT=8080")
                                         #:wait (lambda (instance)
                                                 instance))))
                 (assert-equal #:expect "api" #:got (service-name spec))
                 (assert-equal #:expect '("app" "--serve")
                               #:got (service-command spec))
                 (assert-equal #:expect "/srv/app" #:got (service-dir spec))
                 (assert-equal #:expect '("PORT=8080") #:got (service-env spec))
                 (assert-true (procedure? (service-wait spec)))))
         (test "a spec that names nothing else defaults to the reports tree"
               (let ((spec (make-service #:name "api" #:command '("true"))))
                 (assert-equal #:expect "reports/services/api.log"
                               #:got (service-log-path spec))
                 (assert-equal #:expect '() #:got (service-env spec))
                 (assert-false (service-dir spec))
                 (assert-false (service-wait spec))))
         (test "free-port hands out a port that can be bound"
               (let ((port (free-port)))
                 (assert-true (integer? port))
                 (assert-true (< 1024 port 65536))))))

;; A real process, its log, and the wait for it to be ready.

(define process-suite
  (suite "e2e service lifecycle"
         (test "a service writes its output to the log it was given"
               (let* ((log (scratch-file "log"))
                      (instance (start-service (shell-service
                                                #:name "noop"
                                                #:script "echo ready; sleep 300"
                                                #:log-path log))))
                 (wait-for-log instance "ready" #:timeout 10)
                 (assert-matches #:pattern "ready" #:got (service-log instance))
                 (assert-matches #:pattern "ready"
                                 #:got (service-log-tail instance #:lines 2))
                 (assert-true (service-running? instance))
                 (assert-equal #:expect "noop"
                               #:got (service-instance-name instance))
                 (stop-service instance)
                 (assert-false (service-running? instance))))
         (test "a service that cannot be started leaves the reason in its log"
               (let* ((log (scratch-file "broken-log"))
                      (instance (start-service
                                 (make-service #:name "broken"
                                               #:command
                                               (list "/nonexistent/veritas-probe")
                                               #:log-path log))))
                 ;; The child reports the failed exec and leaves; a service that
                 ;; is not there is a failure with a reason, not a pass.  The
                 ;; reason names the program, whatever language it is written in.
                 (wait-until (lambda ()
                               (not (service-running? instance)))
                             #:timeout 10)
                 (assert-matches #:pattern "/nonexistent/veritas-probe"
                                 #:got (file-text log))
                 (assert-false (service-running? instance))))
         ;; A server that forks keeps its work in a child of its own.  Stopping
         ;; only the process this framework forked would leave that child
         ;; running, so the stop signals the whole session the service leads.
         (test "stopping a service takes the programs it started with it"
               (let* ((pidfile (scratch-file "grandchild.pid"))
                      ;; `$0' of a `sh -c' script is the argument after it, so
                      ;; the script records the pid of the program it forks
                      ;; without a path being pasted into the command.
                      (instance (start-service
                                 (make-service
                                  #:name "forking"
                                  #:command
                                  (list "sh" "-c"
                                        "sleep 300 & echo $! > \"$0\"; wait"
                                        pidfile)
                                  #:log-path (scratch-file "forking-log")))))
                 (wait-until (lambda ()
                               (file-exists? pidfile))
                             #:timeout 10)
                 (let ((grandchild (string->number
                                    (string-trim-both
                                     (file-text pidfile)))))
                   (assert-true (integer? grandchild))
                   (assert-true (process-alive? grandchild))
                   (stop-service instance)
                   (wait-until (lambda ()
                                 (not (process-alive? grandchild)))
                               #:timeout 10)
                   (assert-false (process-alive? grandchild)))))
         (test "a service runs in the directory and environment it was given"
               (let* ((log (scratch-file "placed-log"))
                      (instance (start-service
                                 (make-service
                                  #:name "placed"
                                  #:command (list "sh" "-c"
                                                  "echo \"$GREETING at $(pwd)\"")
                                  #:dir "/tmp"
                                  #:env '("GREETING=hi")
                                  #:log-path log))))
                 (wait-for-log instance "hi at /tmp" #:timeout 10)
                 (assert-matches #:pattern "hi at /tmp"
                                 #:got (service-log instance))
                 (stop-service instance)))
         (test "a spec that could not run is refused before anything starts"
               (assert-raises (lambda ()
                                (start-service (make-service #:command '("true")))))
               (assert-raises (lambda ()
                                (start-service (make-service #:name "no-command"))))
               (assert-raises (lambda ()
                                (start-service (make-service
                                                #:name "bad-env"
                                                #:command '("true")
                                                #:env '("NOT-AN-ASSIGNMENT"))))))
         ;; A shell that ignores the ask and keeps working is the case the kill
         ;; is for: the ask has to be given up on, not waited on.
         (test "a service that ignores the ask to stop is killed"
               (let ((instance (start-service (shell-service
                                               #:name "stubborn"
                                               #:script
                                               "trap '' TERM; while :; do sleep 0.1; done"
                                               #:log-path
                                               (scratch-file "stubborn-log")))))
                 (sleep 0.2)
                 (assert-true (stop-service instance #:timeout 0.3
                                            #:interval 0.05))
                 (assert-false (service-running? instance))))
         (test "wait-for-log raises when the line never comes"
               (let ((instance (start-service (shell-service
                                               #:name "quiet"
                                               #:script "sleep 300"
                                               #:log-path
                                               (scratch-file "quiet-log")))))
                 (assert-raises (lambda ()
                                  (wait-for-log instance "ready"
                                                #:timeout 0.3
                                                #:interval 0.05)))
                 (stop-service instance)))
         (test "wait-for-port waits for a listening socket"
               (let* ((port (free-port))
                      (listener (socket AF_INET SOCK_STREAM 0)))
                 (bind listener (make-socket-address AF_INET INADDR_LOOPBACK port))
                 (listen listener 1)
                 (assert-true (wait-for-port "127.0.0.1" port #:timeout 2))
                 (close listener)))
         (test "wait-for-port raises when nothing listens"
               (assert-raises (lambda ()
                                (wait-for-port "127.0.0.1" (free-port)
                                               #:timeout 0.3
                                               #:interval 0.05))))
         (test "with-services stops what it started, even after a yield"
               (let ((seen #f))
                 (with-services (lambda (instances)
                                  (set! seen (car instances))
                                  (sleep 0.05)
                                  (assert-true (service-running? seen)))
                                #:services
                                (list (shell-service
                                       #:name "scoped"
                                       #:script "echo ready; sleep 300"
                                       #:log-path (scratch-file "scoped-log"))))
                 (assert-false (service-running? seen))
                 ;; Only what this test started: a sibling test of the same
                 ;; suite may be running services of its own at this moment.
                 (assert-false (running-service "scoped"))))
         ;; The registry is what a test that never held an instance reaches the
         ;; suite's services through, so it has to be one value for the process
         ;; and not one per fiber: a thread that was already running when the
         ;; services started reads it here.
         (test "the registry is the same one from another thread"
               (let* ((observer (start-registry-observer))
                      (ask (car observer))
                      (answer (cdr observer)))
                 (with-services (lambda (instances)
                                  (put-message ask 'now)
                                  (let ((seen (get-message answer)))
                                    (assert-equal
                                     #:expect #t
                                     #:got (and (member "observed" seen) #t)
                                     #:name "the other thread sees the service")))
                                #:services
                                (list (shell-service
                                       #:name "observed"
                                       #:script "echo ready; sleep 300"
                                       #:log-path
                                       (scratch-file "observed-log"))))))))

;; The same services, exercised the way a test suite uses them: started before
;; the first test, still running for every test, stopped after the last.

(define stub-port (free-port))

(define (stub-url path)
  (format #f "http://127.0.0.1:~a~a" stub-port path))

(define http-suite
  (e2e-suite "e2e over http against a real service"
    (list (make-service #:name "stub"
                        #:command (list "guile" "-s" stub-script
                                        (number->string stub-port))
                        #:log-path (scratch-file "stub-log")
                        #:wait (lambda (instance)
                                 (wait-for-http (stub-url "/health")
                                                #:timeout 60))))
    #:timeout 120
    (test "the suite started the service before its tests"
          (assert-service-running #:service "stub"
                                  #:name "the stub is up"))
    (test "a healthy path answers 200"
          (assert-equal #:expect 200
                        #:got (http-status (http-get (stub-url "/health")))))
    (test "the body is the one the service sent"
          (assert-equal #:expect "ok"
                        #:got (http-body (http-get (stub-url "/health")))))
    (test "a missing path answers 404 instead of raising"
          (assert-equal #:expect 404
                        #:got (http-status (http-get (stub-url "/missing")))))
    (test "a json body is parsed"
          (assert-equal #:expect #t
                        #:got (assoc-ref (http-json
                                          (http-get (stub-url "/json")))
                                         "ok")))
    (test "a header is read back, whatever its case"
          (let ((response (http-get (stub-url "/json"))))
            (assert-matches #:pattern "application/json"
                            #:got (http-header response "content-type"))
            (assert-matches #:pattern "json"
                            #:got (http-header response 'content-type))))
    (test "wait-for-http returns the response once it arrives"
          (assert-equal #:expect 200
                        #:got (http-status
                               (wait-for-http (stub-url "/health")
                                              #:timeout 10))))))

(define (spec)
  (veritas-run #:debug? #f spec-suite process-suite http-suite))
