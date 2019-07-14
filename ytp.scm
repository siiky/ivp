(import (only chicken.process process-execute process-fork process-wait))
(import chicken.io)
(import chicken.process-context)
(import chicken.string)
(import chicken.type)
(import scheme)

(import (only scm-utils foreach/enum))
(import http-client)
(import json)
(import openssl)
(import srfi-1)
(import srfi-13)
(import srfi-14)
(import uri-common)

(define (usage pn) (print "Usage: " pn " SEARCH-TERM..."))

(define *player* "mpv")

(: *default-fields* string)
(define-constant *default-fields* "videoId,title")

(: api-url string)
(define-constant api-url "https://invidio.us/api/v1/")

(: search-api-url string)
(define-constant search-api-url (string-append api-url "search"))

(: search-url ((list-of (pair (or string symbol) string)) --> string))
(define (search-url parms)
  (let ((parms
          (if (member 'fields parms (lambda (f kvp) (eq? f (car kvp))))
              parms
              (alist-cons 'fields *default-fields* parms))))
    (string-append search-api-url "?" (form-urlencode parms))))

(: watch-url (string --> string))
(define (watch-url vid-id) (string-append "https://invidio.us/watch?v=" vid-id))

(: args->can-args ((list-of string) --> (list-of string)))
(define (args->can-args args) (string-join (map string-trim-both args) " "))

; TODO: vector->list & assoc
(define (avector-key avector key)
  (let ((len (vector-length avector)))
    (let loop ((idx 0))
      (cond
        ((>= idx len)
         #f)

        ((string= key (car (vector-ref avector idx)))
         (cdr (vector-ref avector idx)))

        (else (loop (+ idx 1)))))))

(define (search str)
  (define (post-proc results)
    (map (lambda (res) `(,(avector-key res "videoId") . ,(avector-key res "title")))
         results))

  (let* ((parms `((q . ,str)))
         (qurl (search-url parms)))
    (post-proc (with-input-from-request qurl #f json-read))))

(define (print-results results)
  (define (print-result idx result)
    (let ((vid-id (car result))
          (title (cdr result)))
      (print idx "\t" vid-id "\t" title)))

  (foreach/enum print-result results))

; NOTE: process-run from chicken.process lets the child run loose if exec fails
(define (process-run cmd args)
  (process-fork
    (lambda ()
      (handle-exceptions
        _ (lambda _ #f)
        (process-execute cmd args)))))

(define (process-spawn cmd args)
  ; NOTE: process-fork never fails
  (process-wait (process-run cmd args)))

(define (play res idx)
  ; TODO: bounds checking
  (let ((res (list-ref res idx)))
    (print "Will play `" (cdr res) "`")
    (process-spawn *player* `(,(watch-url (car res))))))

(define (user-repl res)
  (print-results res)
  (let ((line (read-line)))
    (cond
      ((or (eof-object? line)
           (string= line "q")
           (string= line "quit"))
       (print "Bye!"))

      ((string-every char-set:digit line)
       (let ((play-res (play res (string->number line))))
         (unless play-res
           (print "An error occured when trying to play the video"))
         (user-repl res)))

      (else
        (print "Not a number!")
        (user-repl res)))))

(define (main args)
  (if (null? args)
      (usage (program-name))
      (let* ((search-string (args->can-args args))
             (res (search search-string)))
        (user-repl res))))

(main (command-line-arguments))
