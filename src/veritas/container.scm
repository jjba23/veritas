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

;;; Commentary:
;;
;; Docker/Podman helpers for integration and black box testing, following the
;; well known container-test pattern: start a container, wait until it is
;; ready, exercise it (exec, logs, host ports), then clean it up.
;;
;; The runtime is auto-detected (Podman first, then Docker) and can be forced
;; with the VERITAS_CONTAINER_RUNTIME environment variable, mirroring the way
;; testcontainers libraries target a Docker-compatible endpoint.  Everything
;; shells out to the CLI, so no daemon bindings are required.
;;
;; Typical use inside a suite:
;;
;;   (with-container
;;     (lambda (c)
;;       (assert-true (container-running? c))
;;       (assert-matches #:pattern "ready"
;;                       #:got (container-exec c '("app" "--version"))))
;;     #:image "alpine"
;;     #:command '("sh" "-c" "echo ready && sleep 600")
;;     #:wait (lambda (c)
;;              (container-wait-for-log c "ready" #:timeout 10)))
;;
;;; Code:

(define-module (veritas container)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (veritas reporter)
  #:use-module (veritas prelude)
  #:export (run-command run-command!
                        container-runtime
                        container-runtime-available?
                        container-run
                        container-exec
                        container-exec-status
                        container-stop
                        container-remove
                        container-logs
                        container-running?
                        container-build
                        container-wait-for-log
                        container-wait-for-command
                        container-wait-for-port
                        wait-until
                        with-container
                        assert-container-running))

;; ====== Process helpers ======

(define (executable-in-path? name)
  "Return @code{#t} when executable @var{name} is found in @env{PATH}."
  (let ((path (getenv "PATH")))
    (and path
         (any (lambda (dir)
                (let ((candidate (string-append (if (string-suffix? "/" dir)
                                                    dir
                                                    (string-append dir "/"))
                                                name)))
                  (and (file-exists? candidate)
                       (access? candidate X_OK))))
              (string-split path #\:)))))

(define (find-executable names)
  "Return the first executable in @var{names} found in @env{PATH}, or @code{#f}."
  (find executable-in-path? names))

(define* (run-command command
                      #:key (input #f))
  "Run @var{command}, a list of strings, without a shell.

Returns two values: the exit status (an integer, @code{-1} when the process
could not be started) and the captured standard output as a string.

@example
(call-with-values (lambda () (run-command (list \"echo\" \"hi\")))
  (lambda (status output) (list status output)))
@result{} (0 \"hi\\n\")
@end example"
  (catch #t
         (lambda ()
           (let* ((port (apply open-pipe* OPEN_READ command))
                  (output (or (get-string-all port) ""))
                  (status (close-pipe port)))
             (values (or (status:exit-val status) -1) output)))
         (lambda (key . args)
           (values -1
                   (format #f "could not run ~a: ~a ~a"
                           (car command) key args)))))

(define* (run-command! command
                       #:key (input #f))
  "Like @code{run-command}, but raise an error on a non-zero exit status.

Returns the captured standard output."
  (call-with-values (lambda ()
                      (run-command command
                                   #:input input))
                    (lambda (status output)
                      (if (zero? status) output
                          (error (format #f
                                  "command failed with status ~a: ~s~%~a"
                                  status command output))))))

;; ====== Runtime detection ======

(define (container-runtime)
  "Return the container runtime executable to use.

Honours @code{VERITAS_CONTAINER_RUNTIME}, otherwise prefers @command{podman}
and falls back to @command{docker}."
  (or (getenv "VERITAS_CONTAINER_RUNTIME")
      (find-executable '("podman" "docker"))
      (error
       "no container runtime found: install podman or docker, or set VERITAS_CONTAINER_RUNTIME")))

(define (container-runtime-available?)
  "Return @code{#t} when a container runtime is available."
  (catch #t
         (lambda ()
           (and (container-runtime) #t))
         (lambda (key . args)
           #f)))

(define (runtime-command . args)
  "Build a command list for the detected runtime with @var{args}."
  (cons (container-runtime) args))

;; Note: `runtime-command' returns a full argv list, e.g.
;; (runtime-command "exec" id "ls") => ("docker" "exec" id "ls").

;; ====== Container lifecycle ======

(define* (container-run #:key image
                        name
                        (env '())
                        (ports '())
                        (volumes '())
                        (command '())
                        (detach? #t)
                        (extra-args '()))
  "Start @var{image} and return the new container id (or name when given).

@var{env} entries are passed as @code{-e} variables, @var{ports} as
@code{-p} bindings and @var{volumes} as @code{-v} mounts."
  (let ((args (append (list "run")
                      (if detach?
                          (list "-d")
                          '())
                      (if name
                          (list "--name" name)
                          '())
                      (append-map (lambda (e)
                                    (list "-e" e)) env)
                      (append-map (lambda (p)
                                    (list "-p" p)) ports)
                      (append-map (lambda (v)
                                    (list "-v" v)) volumes)
                      extra-args
                      (list image)
                      command)))
    (string-trim-both (run-command! (apply runtime-command args)))))

(define (container-exec container command)
  "Run @var{command} (a list of strings) inside running @var{container}.

Returns the captured combined output and raises on failure."
  (string-trim-both (run-command! (apply runtime-command "exec" container
                                         command))))

(define (container-exec-status container command)
  "Run @var{command} inside @var{container}, returning two values: status and output."
  (run-command (apply runtime-command "exec" container command)))

(define (container-stop container)
  "Stop @var{container}, returning its exit status."
  (call-with-values (lambda ()
                      (run-command (runtime-command "stop" container)))
                    (lambda (status output)
                      status)))

(define (container-remove container)
  "Force-stop and remove @var{container}, returning its exit status."
  (call-with-values (lambda ()
                      (run-command (runtime-command "rm" "-f" container)))
                    (lambda (status output)
                      status)))

(define (container-logs container)
  "Return the logs of @var{container} as a string."
  (string-trim-both (run-command! (runtime-command "logs" container))))

(define (container-running? container)
  "Return @code{#t} when @var{container} exists and is running."
  (call-with-values (lambda ()
                      (run-command (runtime-command "inspect" "-f"
                                                    "{{.State.Running}}"
                                                    container)))
                    (lambda (status output)
                      (and (zero? status)
                           (string=? (string-trim-both output) "true")))))

(define* (container-build #:key tag dockerfile
                          (context ".")
                          (extra-args '()))
  "Build an image with @var{tag} from @var{dockerfile} in @var{context}."
  (run-command! (apply runtime-command
                       (append (list "build" "-t" tag)
                               (if dockerfile
                                   (list "-f" dockerfile)
                                   '()) extra-args
                               (list context)))))

;; ====== Readiness ======

(define* (wait-until predicate
                     #:key (timeout 30)
                     (interval 0.2)
                     (message "timed out waiting for condition"))
  "Poll @var{predicate} every @var{interval} seconds until it is true.

Returns @code{#t}, or raises an error after @var{timeout} seconds.  Exceptions
raised by @var{predicate} are treated as a false result."
  (let loop
    ((elapsed 0))
    (cond
      ((catch #t predicate
              (lambda (key . args)
                #f))
       #t)
      ((>= elapsed timeout)
       (error (format #f "~a (after ~a seconds)" message timeout)))
      (else (usleep (inexact->exact (round (* interval 1000000.0))))
            (loop (+ elapsed interval))))))

(define* (container-wait-for-log container pattern
                                 #:key (timeout 30)
                                 (interval 0.2))
  "Wait until @var{container} logs match @var{pattern} (regexp or string)."
  (let ((rx (if (regexp? pattern) pattern
                (make-regexp pattern))))
    (wait-until (lambda ()
                  (regexp-exec rx
                               (container-logs container)))
                #:timeout timeout
                #:interval interval
                #:message (format #f "container ~a never logged a match"
                                  container))))

(define* (container-wait-for-command container command
                                     #:key (timeout 30)
                                     (interval 0.2))
  "Wait until @var{command} succeeds inside @var{container}."
  (wait-until (lambda ()
                (call-with-values (lambda ()
                                    (container-exec-status container command))
                                  (lambda (status output)
                                    (zero? status))))
              #:timeout timeout
              #:interval interval
              #:message (format #f "command ~s never succeeded in ~a" command
                                container)))

(define* (container-wait-for-port host port
                                  #:key (timeout 30)
                                  (interval 0.2))
  "Wait until a TCP connection to @var{host}:@var{port} succeeds."
  (wait-until (lambda ()
                (catch #t
                       (lambda ()
                         (let ((s (socket AF_INET SOCK_STREAM 0)))
                           (catch #t
                                  (lambda ()
                                    (connect s
                                             (make-socket-address AF_INET
                                                                  (inet-pton
                                                                   AF_INET
                                                                   host) port))
                                    (close s) #t)
                                  (lambda (key . args)
                                    (close s) #f))))
                       (lambda (key . args)
                         #f)))
              #:timeout timeout
              #:interval interval
              #:message (format #f "port ~a:~a never opened" host port)))

;; ====== Scoped helper ======

(define* (with-container thunk
                         #:key image
                         name
                         (env '())
                         (ports '())
                         (volumes '())
                         (command '())
                         (detach? #t)
                         (extra-args '())
                         (wait #f)
                         (keep? #f))
  "Start a container, call @var{thunk} with its id, and always clean it up.

@var{wait}, when supplied, is a procedure called with the container id after
start (for example to wait for a log line or a port).  @var{keep?} leaves the
container running for debugging."
  (let ((container (container-run #:image image
                                  #:name name
                                  #:env env
                                  #:ports ports
                                  #:volumes volumes
                                  #:command command
                                  #:detach? detach?
                                  #:extra-args extra-args)))
    (call-with-fiber-safe-wind
     (lambda () #t)
     (lambda ()
       (when wait
         (wait container))
       (thunk container))
     (lambda ()
       (unless keep?
         (container-remove container))))))

;; ====== Assertion ======

(define* (assert-container-running #:key container name)
  "Return an assertion that @var{container} is running.

Unlike @code{assert-true}, the check is performed when the assertion runs, not
when it is constructed."
  (lambda (vc)
    (when name
      (log-assert vc name))
    (if (container-running? container)
        (begin
          (log-pass vc
                    (format #f "container ~a is running" container))
          `((assertion-successful unquote #t)
            (assertion-name unquote name)))
        (begin
          (log-fail vc
                    (format #f "container ~a is not running" container))
          `((assertion-successful unquote #f)
            (assertion-name unquote name))))))
