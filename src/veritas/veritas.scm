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

(define-module (veritas veritas)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:export ())

;;
;; logging helpers
;;

(define (log-message msg)
  (display (format #f "\n~a\n" msg)))

(define (log msg . args)
  (log-message (apply format
                      (append (list #f msg) args))))

(define (log-info msg . args)
  (log-message (format #f "[INFO] ~a"
                       (apply format
                              (append (list #f msg) args)))))

(define (log-error msg . args)
  (log-message (format #f "[ERROR] ~a"
                       (apply format
                              (append (list #f msg) args)))))

;;
;; string checks
;;

(define (non-empty-string? s)
  (and s
       (not (equal? "" s))))

(define (empty-string? s)
  (equal? "" s))

;;
;; string manipulation
;;

(define (string-drop-first-last s)
  (if (> (string-length s) 2)

      (string-take (string-drop s 1)
                   (- (string-length s) 2)) s))

(define (string-drop-first-last-n s n)
  (if (> (string-length s) 2)
      (string-take (string-drop s n)
                   (- (string-length s)
                      (+ 1 n))) s))

(define (string-drop-first-last-line s)
  (let* ((lines (string-split s #\newline))
         (middle (if (> (length lines) 2)
                     (drop-right (cdr lines) 1)
                     '())))
    (string-join middle "\n")))

;;
;; alist helpers
;;

(define (assoc-ref-or-empty-str alist field)
  (let ((sut (assoc-ref alist field)))
    (if (equal? #f sut) "" sut)))

;;
;; Asset library is an in-memory cache, a lookup dictionary
;; that can be used for faster performance, memoization, etc
;; this version will read from a list of files specifies with
;; the FILES param
;;
;; to use it, you can simply call the function
;; with a list of file paths:
;;
;; (define my-asset-library
;;   (build-asset-library '("pub/css/fonts.css" "pub/js/tailwind.js")))
;;

(define* (build-asset-library files)
  (let* ((lib '()))
    (for-each (lambda (f)
                (let* ((file-contents (get-string-all (open-input-file f)))
                       (min-contents (cond
                                       ((string-suffix? ".css" f)
                                        (string-replace-substring
                                         file-contents "\n" ""))
                                       (else file-contents))))
                  (set! lib
                        (assoc-set! lib f min-contents)))) files) lib))

;; syscall

(define (syscall cmd)
  (let* ((process (open-input-pipe cmd))
         (process-output (get-string-all process)))
    (close-pipe process) process-output))

(define (get-short-commit-sha)
  (syscall "git log --pretty=format:'%h' -n 1"))

(define html-escape-alist
  '(("'" "&apos;")
    ("\"" "&quot;")
    ("á" "&aacute;")
    ("à" "&agrave;")
    ("Á" "&Aacute;")
    ("À" "&Agrave;")
    ("é" "&eacute;")
    ("è" "&egrave;")
    ("É" "&Eacute;")
    ("È" "&Egrave;")
    ("í" "&iacute;")
    ("ì" "&igrave;")
    ("Í" "&Iacute;")
    ("Ì" "&Igrave;")
    ("ó" "&oacute;")
    ("ò" "&ograve;")
    ("Ó" "&Oacute;")
    ("Ò" "&Ograve;")
    ("ú" "&uacute;")
    ("ù" "&ugrave;")
    ("Ú" "&Uacute;")
    ("Ù" "&Ugrave;")
    ("ç" "&ccedil;")
    ("Ç" "&Ccedil;")
    ("ñ" "&ntilde;")
    ("Ñ" "&Ntilde;")
    ("<" "&lt;")
    (">" "&gt;")
    ("/" "&sol;")
    ("*" "&ast;")
    ("å" "&aring;")
    ("æ" "&aelig;")
    ("ë" "&euml;")
    ("ï" "&iuml;")
    ("ô" "&ocirc;")
    ("ö" "&ouml;")
    ("ø" "&oslash;")
    ("û" "&ucirc;")
    ("ü" "&uuml;")
    ("ä" "&auml;")
    ("â" "&acirc;")
    ("ß" "&szlig;")
    ("Ü" "&Uuml;")
    ("Ö" "&Ouml;")
    ("Ë" "&Euml;")
    ("Ä" "&Auml;")
    ("Ï" "&Iuml;")
    ("$" "&dollar;")
    ("%" "&percnt;")
    ("ã" "&atilde;")
    ("Ã" "&Atilde;")
    ("õ" "&otilde;")
    ("Õ" "&Otilde;")))

(define (html-escape x)
  (fold (lambda (entity accum)
          (string-replace-substring accum
                                    (car entity)
                                    (cadr entity))) x html-escape-alist))

(define (html-unescape x)
  (fold (lambda (entity accum)
          (string-replace-substring accum
                                    (cadr entity)
                                    (car entity))) x html-escape-alist))

(define (alist-from-url-encoded-body body)
  (define (unsafe-mk-alist x)
    (cons (car x)
          (cdr x)))
  (let* ((str (bytevector->string body "utf-8"))
         (raw-kv-pairs (string-split str #\&))
         (xss (map unsafe-mk-alist
                   (map (lambda (x)
                          (string-split x #\=)) raw-kv-pairs)))
         (read-from-pairs (lambda (x)
                            (catch #t
                                   (lambda ()
                                     `(,(uri-decode (car x)) unquote
                                       (uri-decode (cadr x))))
                                   (lambda (key . args)
                                     (display (format #f
                                               "
warning! could not decode pair ~a: ~a: ~a
"
                                               x key args))
                                     `((unquote (uri-decode (car x))) . "")))))
         (unescaped-xss (map read-from-pairs xss))
         (result (append unescaped-xss)))
    (if (equal? result
                '(("" . "")))
        '() result)))
