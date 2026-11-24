;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; End-to-end testing: what a system does when it runs, not what a procedure
;; returns.  A test at this level starts the programs it talks to, waits until
;; they are ready, exercises them over an interface they really offer - HTTP,
;; usually - and asserts on the answer with the matchers this framework already
;; has.
;;
;; The unit of the module is a @code{service}: a command, the directory it runs
;; in, the environment it gets, the file its output goes to, and a procedure
;; that says when it is ready.  Starting one gives it a session of its own and a
;; log of its own, and execs it, so the process is the program itself and not a
;; shell left in front of it.  Stopping one signals the whole session, so a
;; server that forked workers takes them with it, and reaps it, so a run leaves
;; no zombies behind.
;;
;; Two shapes cover most suites:
;;
;;   (e2e-suite "the checkout api"
;;     (list api db)
;;     (test "a cart survives a round trip"
;;           (assert-equal #:expect 200
;;                         #:got (http-status (http-get (url "/carts/1"))))))
;;
;; @code{e2e-suite} starts @var{api} and @var{db} before the first test, waits
;; for both to be ready, and stops them after the last one - including when a
;; test failed.  A test that needs a process of its own, or wants to restart
;; one to watch it recover, brackets it with @code{with-services} instead.
;;
;; Readiness is a @code{#:wait} procedure rather than a fixed sleep:
;; @code{wait-for-port} waits for a socket, @code{wait-for-http} for a status,
;; and @code{wait-for-log} for a line a server prints once it is serving.  A
;; suite that sleeps instead waits for the slowest machine it will ever run on.
;;
;; Containers are not part of this module: @code{(veritas container)} starts
;; one, and its id, host ports and logs are wired into a suite the same way any
;; other dependency is.
;;
;;; Code:

(define-module (veritas e2e)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module ((web client)
                #:select (http-request)
                #:prefix web:)
  #:use-module ((web response)
                #:select (response-code response-headers))
  #:use-module ((veritas container)
                #:select (wait-until container-wait-for-port))
  #:use-module (veritas reporter)
  #:use-module (veritas prelude)
  #:use-module (veritas veritas)
  #:export (make-service service?
                         service-name
                         service-command
                         service-dir
                         service-env
                         service-log-path
                         service-wait
                         service-instance?
                         service-instance-service
                         service-instance-name
                         service-instance-pid
                         service-instance-log-path
                         start-service
                         stop-service
                         service-running?
                         service-log
                         service-log-tail
                         free-port
                         wait-for-log
                         wait-for-port
                         wait-for-http
                         http-response?
                         http-status
                         http-headers
                         http-body
                         http-header
                         http-json
                         http-request
                         http-get
                         http-post
                         assert-service-running
                         running-service
                         running-services
                         with-services
                         e2e-suite))

;; ====== What a service is ======

(define-record-type service
  (%make-service name command dir env log-path wait) service?
  (name service-name)
  (command service-command)
  (dir service-dir)
  (env service-env)
  (log-path service-log-path)
  (wait service-wait))

(define* (make-service #:key name command (dir #f) (env '()) (log-path #f)
                       (wait #f))
  "Return the description of a service named @var{name} that runs @var{command}.

@var{command} is a program and its arguments, a non-empty list of strings, and
it is executed as it is: nothing in the list is re-read as shell syntax, so a
path with a space in it needs no quoting.  @var{dir} is the directory to run it
in, @code{#f} for the directory this run was started from.  @var{env} is a list
of @code{\"NAME=value\"} assignments added to the environment of the service.

@var{log-path} is where the output of the service is collected, by default
@code{reports/services/@var{name}.log}.  A run that fails under CI leaves that file
behind, which is normally the whole of what there is to read.  @var{wait} is a
procedure called with the instance once it is started, and is how a service
declares itself ready - @code{wait-for-port}, @code{wait-for-http} or
@code{wait-for-log}, or one of them per dependency it needs.

@example
(make-service #:name \"api\"
              #:command '(\"app\" \"serve\")
              #:env '(\"PORT=8080\")
              #:wait (lambda (instance)
                      (wait-for-http \"http://127.0.0.1:8080/health\")))
@end example"
  (%make-service name command dir env
                 (or log-path (format #f "reports/services/~a.log" name))
                 wait))

(define-record-type service-instance
  (%make-service-instance service pid) service-instance?
  (service service-instance-service)
  (pid service-instance-pid))

(define (service-instance-name instance)
  "Return the name of the service @var{instance} runs."
  (service-name (service-instance-service instance)))

(define (service-instance-log-path instance)
  "Return the file @var{instance} writes its output to."
  (service-log-path (service-instance-service instance)))

;; ====== Environment ======

(define (validate-assignment assignment)
  "Raise unless @var{assignment} is a \"NAME=value\" string, and return it.

Only the first @code{=} separates the name from the value, so a value may hold
one of its own - a base64 secret, a query string.  The entry reaches the service
as an argument of @command{env}, so the assignment is a value here and never a
word of a shell command; it is checked before anything is started, so a
malformed environment is a mistake to read rather than a service that comes up
without it."
  (unless (and (string? assignment)
               (let ((equals (string-index assignment #\=)))
                 (and equals (> equals 0))))
    (error "a service environment entry needs a NAME=value assignment,
and this one is not one:"
           assignment))
  assignment)

;; ====== Starting and stopping ======

(define (validate-service spec)
  "Raise unless @var{spec} can be started.

The checks happen before anything is started: a service that is missing a name,
a command or a well-formed environment entry is a mistake to report, not a
process to start and watch fail."
  (unless (and (service-name spec)
               (string? (service-name spec))
               (not (string-null? (service-name spec))))
    (error "a service needs a name, and this one has none:" spec))
  (let ((command (service-command spec)))
    (unless (and (list? command)
                 (pair? command)
                 (every string? command))
      (error "a service needs a command, a non-empty list of strings:"
             spec)))
  (for-each validate-assignment (service-env spec)))

;; The service is started through a shell for the things a program cannot do for
;; itself: redirect its output, change directory, and take a session of its own
;; so that stopping it reaches whatever it started.  The last `exec' replaces the
;; shell with the program, so no shell is left in the process tree, and the log
;; path, the directory, the environment and the command all arrive as arguments:
;; nothing is ever pasted into the text the shell reads.
(define service-launcher
  (string-append "exec >> \"$1\" 2>&1; shift;"
                 " cd \"$1\" || { echo \"no such directory: $1\" >&2; exit 127; };"
                 " shift; exec env \"$@\""))

(define (spawn-service spec)
  "Start the program of @var{spec} and return its pid.

@command{setsid} makes the service the leader of a new session, which is what
lets @code{stop-service} signal its whole group; @command{sh} makes both output
streams the log; the last @code{exec} makes the program the process, so the pid
this returns is the pid of the service.

@code{open-pipe*} starts a program here rather than @code{primitive-fork}: a run
has fibers' worker threads in it, and Guile warns about - and promises nothing
for - a fork taken beside them, while a process started through a pipe is the
path it supports."
  (let ((port (apply open-pipe* OPEN_READ
                     "setsid" "sh" "-c" service-launcher
                     "sh"
                     (service-log-path spec)
                     (or (service-dir spec) ".")
                     (append (service-env spec) (service-command spec)))))
    ;; The pid is the one thing a pipe keeps for the process behind it.  The
    ;; pipe itself is not read - the shell has already sent both streams to the
    ;; log - and it is closed with `close-port' rather than `close-pipe', which
    ;; would wait for a service that is meant to outlive this call.
    (let ((pid (hashq-ref port/pid-table port)))
      (close-port port)
      pid)))

(define (start-service spec)
  "Start @var{spec} and return the running instance.

The service is not ready when this returns - that is what its @var{wait} is
for, and @code{e2e-suite} and @code{with-services} apply it.

@example
(define instance (start-service (make-service #:name \"api\" #:command '(\"app\"))))
(service-running? instance)
@result{} #t
(stop-service instance)
@end example"
  (validate-service spec)
  (ensure-parent-directories (service-log-path spec))
  (let ((pid (spawn-service spec)))
    (if pid
        (%make-service-instance spec pid)
        (error "the service could not be started:"
               (service-name spec)))))

(define (service-status instance)
  "Return how the process of @var{instance} stands: @code{'running} or @code{'gone}.

A child that has exited is reaped here, which is what keeps a run from leaving a
zombie for every service it stopped.  Guile answers @code{(0 . status)} for a
child that still lives and @code{(pid . status)} once it has been reaped, and
raises once there is no child left to ask about."
  (catch #t
         (lambda ()
           (let ((answer (waitpid (service-instance-pid instance) WNOHANG)))
             (if (and (pair? answer)
                      (= (car answer) (service-instance-pid instance)))
                 'gone 'running)))
         (lambda (key . args) 'gone)))

(define (service-running? instance)
  "Return @code{#t} when the process of @var{instance} still runs.

A service that exited on its own is reported as not running, and reaped."
  (eq? 'running (service-status instance)))

(define (signal-service instance signal)
  "Send @var{signal} to the whole session @var{instance} runs in.

The negative pid is the process group, and the group is the session the service
started: its own forks go with it, which is the difference between stopping a
server and leaving its workers running."
  (catch #t
         (lambda ()
           (kill (- (service-instance-pid instance)) signal)
           #t)
         (lambda (key . args) #f)))

(define* (stop-service instance #:key (timeout 10) (interval 0.1))
  "Stop @var{instance} and return @code{#t} when it is gone.

The service is asked to stop first, so it can close what it holds, and killed
only when it is still there after @var{timeout} seconds.  Stopping a service
that already stopped is not an error."
  (let wait-for-exit ((elapsed 0))
    (cond
      ((not (service-running? instance)) #t)
      ((>= elapsed timeout)
       (signal-service instance SIGKILL)
       (let kill-again ((left 5))
         (cond ((not (service-running? instance)) #t)
               ((<= left 0) #f)
               (else (usleep (inexact->exact (round (* interval 1000000))))
                     (kill-again (- left 1))))))
      (else
       ;; Only the first pass asks; the rest wait for the process to be gone,
       ;; which a server that flushes on the way out takes a moment to be.
       (when (zero? elapsed)
         (signal-service instance SIGTERM))
       (usleep (inexact->exact (round (* interval 1000000))))
       (wait-for-exit (+ elapsed interval))))))

;; ====== What a service says ======

(define (service-log instance)
  "Return everything @var{instance} has written to its log so far."
  (let ((path (service-instance-log-path instance)))
    (if (file-exists? path)
        (call-with-input-file path get-string-all)
        "")))

(define* (service-log-tail instance #:key (lines 20))
  "Return the last @var{lines} lines @var{instance} wrote to its log.

What a failure report wants: the end of the log is where a server says why it
never came up, and the beginning is where it says what it is."
  (let* ((all (string-split (service-log instance) #\newline))
         (wanted (min lines (length all))))
    (string-join (take-right all wanted) "\n")))

;; ====== The services a suite is running ======
;;
;; A test body is called with nothing but the reporter, so a test inside an
;; `e2e-suite' has no other way to reach the instance the suite started for it.
;; The registry is that way, and it holds the services of every suite running at
;; this moment - newest first, so a suite that starts a second instance of a
;; name it already has finds the one it just started.
;;
;; It is one mutable cell rather than a parameter: a parameter is bound per
;; dynamic extent, and the tests of a suite run in fibers of their own, so what
;; `start-services' wrote in the suite's fiber would not be what the test fiber
;; reads back.

(define service-registry (cons 'veritas-service-registry '()))

(define (registered-services)
  "Return the instances the registry holds, newest first."
  (cdr service-registry))

(define (set-registered-services! instances)
  "Make @var{instances} what the registry holds."
  (set-cdr! service-registry instances))

(define (register-services instances)
  "Add @var{instances} to the services this run has running."
  (set-registered-services! (append (reverse instances)
                                    (registered-services))))

(define (unregister-services instances)
  "Forget @var{instances}, by identity, and return what is left."
  (set-registered-services! (remove (lambda (instance)
                                      (memq instance instances))
                                    (registered-services))))

(define (running-services)
  "Return every service instance this run has started and not yet stopped."
  (registered-services))

(define (running-service name)
  "Return the newest instance of the service named @var{name}, or @code{#f}.

@var{name} is what the spec called it.  The instance is the newest one this run
started and has not stopped, so a suite that restarts a service reaches the one
it just started."
  (find (lambda (instance)
          (equal? name (service-instance-name instance)))
        (registered-services)))

;; ====== Ready ======

;; `wait-for-port' probes a socket and knows nothing about what is behind it, so
;; the one in (veritas container) is the one to reuse; only its name mentions
;; containers, and a service is what this module waits for.
(define wait-for-port
  container-wait-for-port)

(define* (wait-for-log instance pattern #:key (timeout 30) (interval 0.2))
  "Wait until @var{instance} logs a line matching @var{pattern}.

@var{pattern} is a regexp or a string.  The line is the readiness a program
declares itself: a server that prints @code{\"listening on 8080\"} when it is
taking connections has said more about being ready than its port has."
  (let ((rx (if (regexp? pattern) pattern (make-regexp pattern))))
    (wait-until (lambda ()
                  (regexp-exec rx (service-log instance)))
                #:timeout timeout
                #:interval interval
                #:message (format #f "~a never logged a match for ~a"
                                  (service-instance-name instance) pattern))))

(define (free-port)
  "Return a port nothing is listening on, for a service to be told to use.

The port is bound, read and released, so what it returns is a port that was free
a moment ago.  A service started with it can run beside another service, and
beside another run of the same suite, without two of them agreeing on a number
and one of them losing.

@example
(define port (free-port))
(make-service #:name \"api\"
              #:command (list \"app\" \"--port\" (number->string port))
              #:wait (lambda (instance) (wait-for-port \"127.0.0.1\" port)))
@end example"
  (let ((socket (socket AF_INET SOCK_STREAM 0)))
    (catch #t
           (lambda ()
             (bind socket (make-socket-address AF_INET INADDR_LOOPBACK 0))
             (let ((port (sockaddr:port (getsockname socket))))
               (close socket)
               port))
           (lambda (key . args)
             (close socket)
             (apply throw key args)))))

;; ====== Talking to what is running ======

(define-record-type http-response
  (%make-http-response status headers body) http-response?
  (status http-status)
  (headers http-headers)
  (body http-body))

(define (body->text body)
  "Return the body of a response, whatever shape the client gave it in.

A response with a charset is decoded to a string, one without arrives as a
bytevector, and one read as a port is read out."
  (cond
    ((string? body) body)
    ((bytevector? body) (utf8->string body))
    ((port? body) (get-string-all body))
    ((not body) "")
    (else (format #f "~a" body))))

(define* (http-request url #:key (method 'GET) (body #f) (headers '()))
  "Send an HTTP @var{method} request to @var{url} and return the response.

@var{headers} is what @code{(web client)} takes, an alist of
@code{(name . value)}.  The call raises when no server answers, and returns the
response as it is when one does: a @code{404} is an answer, and a test that
expects one asserts on @code{http-status}.

@example
(http-status (http-request \"http://127.0.0.1:8080/carts\"
                           #:method 'POST
                           #:body \"{\\\"item\\\":\\\"tea\\\"}\"
                           #:headers '((content-type . (application/json)))))
@result{} 201
@end example"
  (call-with-values
      (lambda ()
        (web:http-request url #:method method #:body body #:headers headers))
    (lambda (response response-body)
      (%make-http-response (response-code response)
                           (response-headers response)
                           (body->text response-body)))))

(define* (http-get url #:key (headers '()))
  "Fetch @var{url} and return the response."
  (http-request url #:method 'GET #:headers headers))

(define* (http-post url #:key (body #f) (headers '()))
  "Post @var{body} to @var{url} and return the response."
  (http-request url #:method 'POST #:body body #:headers headers))

(define (http-header response name)
  "Return the first value of header @var{name} in @var{response}, as a string.

@var{name} may be a symbol or a string, and the match ignores case, because the
HTTP header a server sends is not obliged to be spelled the way the test spells
it.  A header with several values answers with the first; @code{http-headers}
holds all of them, in the shape the client parsed them into, for a test that
needs the rest."
  (let ((wanted (if (symbol? name) (symbol->string name) name)))
    (any (lambda (header)
           (let ((key (car header)))
             (and (string-ci=? wanted (if (symbol? key)
                                          (symbol->string key)
                                          key))
                  (let ((value (cdr header)))
                    (let ((first (if (pair? value) (car value) value)))
                      (cond ((string? first) first)
                            ((symbol? first) (symbol->string first))
                            (else (format #f "~a" first))))))))
         (http-headers response))))

(define (http-json response)
  "Return the body of @var{response} parsed as JSON.

An object becomes an alist with string keys, so a field is read with
@code{assoc-ref}; the order a JSON object lists its fields in is not kept, and
an assertion compares what it holds, not how it was written."
  (json-string->scm (http-body response)))

(define* (wait-for-http url #:key (status 200) (method 'GET) (body #f)
                        (headers '()) (timeout 30) (interval 0.2))
  "Wait until @var{url} answers @var{status}, and return that response.

A connection refused is not a failure here: it is what a server that is still
starting does, and the call retries until @var{timeout} seconds have passed.

@example
(wait-for-http \"http://127.0.0.1:8080/health\" #:timeout 60)
@end example"
  (let loop ((elapsed 0))
    (let ((response (catch #t
                      (lambda ()
                        (http-request url #:method method #:body body
                                      #:headers headers))
                      (lambda (key . args) #f))))
      (cond
        ((and response
              (= status (http-status response)))
         response)
        ((>= elapsed timeout)
         (error (format #f "~a never answered ~a within ~a seconds"
                        url status timeout)))
        (else (usleep (inexact->exact (round (* interval 1000000))))
              (loop (+ elapsed interval)))))))

;; ====== Assertions ======

(define* (assert-service-running #:key instance service name)
  "Return an assertion that a service is running.

@var{instance} is a started service, and @var{service} names one that a suite
started - what @code{running-service} finds - for a test that never held the
instance itself.  The check runs when the assertion runs, so a service that died
during the test is a failure, not a pass recorded at the start of it.

@example
(assert-service-running #:service \"api\" #:name \"the api survived the write\")
@end example"
  (lambda (vc)
    (let* ((target (or instance
                       (and service (running-service service))))
           (label (or (and target (service-instance-name target))
                      service
                      "the service"))
           (up? (and target (service-running? target))))
      (when name
        (log-assert vc name))
      (if up?
          (begin
            (log-pass vc (format #f "~a is running" label))
            `((assertion-successful . #t) (assertion-name . ,name)))
          (begin
            (log-fail vc (format #f "~a is not running" label))
            `((assertion-successful . #f) (assertion-name . ,name)))))))

;; ====== Starting and stopping a group ======

(define (start-each services)
  "Start every service in @var{services}, stopping the ones started so far on failure."
  (let loop ((rest services) (started '()))
    (if (null? rest)
        (reverse started)
        (catch #t
               (lambda ()
                 (loop (cdr rest)
                       (cons (start-service (car rest)) started)))
               (lambda (key . args)
                 (for-each stop-service started)
                 (apply throw key args))))))

(define* (start-services services #:key (vc #f))
  "Start @var{services}, wait for each to be ready, and return the instances.

Every service is started before any wait runs, so a slow one does not hold up
the start of the others.  A service that cannot be started, or one whose
@code{#:wait} gives up, stops every service started before it and raises: a
half-started environment is never handed to a test."
  (let ((started (start-each services)))
    (catch #t
           (lambda ()
             (for-each (lambda (instance)
                         (let ((wait (service-wait
                                      (service-instance-service instance))))
                           (when wait
                             (wait instance))))
                       started)
             (register-services started)
             (when vc
               (for-each (lambda (instance)
                           (log-info vc
                                     (format #f "~a is up, logging to ~a"
                                             (service-instance-name instance)
                                             (service-instance-log-path
                                              instance))))
                         started))
             started)
           (lambda (key . args)
             (for-each stop-service (reverse started))
             (apply throw key args)))))

(define (stop-services instances)
  "Stop @var{instances}, newest first, and forget them."
  (for-each stop-service (reverse instances))
  (unregister-services instances)
  #t)

(define* (with-services thunk #:key (services '()))
  "Start @var{services}, call @var{thunk} with the instances, and stop them after.

The stop runs however @var{thunk} ends, and it waits for the fiber to be done
rather than for a suspension: a sleep or a socket read in @var{thunk} suspends
it, and a plain @code{dynamic-wind} would tear the services down in the middle
of the test that is using them.

@example
(with-services (lambda (instances)
                 (let ((api (car instances)))
                   (assert-service-running #:instance api)))
               #:services (list api-spec))
@end example"
  (let ((instances (start-services services)))
    (call-with-fiber-safe-wind
     (lambda () #t)
     (lambda () (thunk instances))
     (lambda () (stop-services instances)))))

;; ====== A suite over a running system ======

;; Every keyword `suite' takes is passed through, so `#:timeout', `#:tags',
;; `#:skip?' and `#:concurrent?' mean here what they mean there.

(define-syntax e2e-suite
  (lambda (stx)
    (syntax-case stx ()
      ((_ name services args ...)
       (call-with-values (lambda ()
                           (split-keyword-arguments #'(args ...)))
         (lambda (keywords body)
           (with-syntax (((keyword ...) keywords)
                         ((thunk ...) (defer-body-forms body)))
             ;; The started instances are kept in this closure rather than read
             ;; back from the registry, so a suite stops what it started even
             ;; when another suite runs beside it.
             ;;
             ;; A suite is built here and run later, which is why there is no
             ;; `dynamic-wind' around it: `#:after-all' is the teardown, and a
             ;; suite aborted by its own `#:timeout' never reaches it.  Such a
             ;; suite leaves its services running, each in a session of its own
             ;; with a log apiece; `ps' and `reports/services' are where to look
             ;; for what a timed-out end-to-end run left behind.
             #`(let ((started '()))
                 (%suite name
                         #:before-all
                         (lambda (vc)
                           (set! started (start-services services #:vc vc))
                           '())
                         #:after-all
                         (lambda (vc)
                           (stop-services started)
                           '())
                         keyword ... thunk ...)))))))))
