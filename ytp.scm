(import
  (only chicken.io read-line)
  (only chicken.process process-execute process-fork process-wait)
  (only chicken.process-context command-line-arguments program-name))

(import
  (only http-client with-input-from-request)
  (only json json-read)
  (only scm-utils !f? foreach/enum)
  (only srfi-1 alist-cons assoc map)
  (only srfi-13 string-every string-join string-trim-both string=)
  (only srfi-14 char-set:digit)
  (only uri-common form-urlencode)
  (prefix (only invidious *fields* search) iv:)
  openssl)

(: usage (string -> void))
(define (usage pn) (print "Usage: " pn " SEARCH-TERM..."))

(define *player*
  (make-parameter
    "mpv"
    (lambda (str)
      (assert (string? str) "`*player*` must be a string")
      str)))

(: watch-url (string --> string))
(define (watch-url vid-id) (string-append "https://invidio.us/watch?v=" vid-id))

(: args->can-args ((list-of string) --> string))
(define (args->can-args args) (string-join (map string-trim-both args) " "))

(define (search str)
  (define (post-proc results)
    (define (vec->vid-id/title res)
      (define (assoc-key key alst) (!f? (assoc key alst string=) cdr))
      (let* ((lst (vector->list res))
             (vid-id (assoc-key "videoId" lst))
             (title (assoc-key "title" lst)))
        `(,vid-id . ,title)))
    (map vec->vid-id/title results))

  (let ((qurl (iv:search #:q str)))
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
  (let ((res (list-ref res idx)))
    (print "Will play `" (cdr res) "`")
    (process-spawn (*player*) `(,(watch-url (car res))))))

(define (user-repl res)
  (define (user-repl-int res len)
    (print-results res)
    (let ((line (read-line)))
      (if (or (eof-object? line)
              (string= line "q")
              (string= line "quit"))
          (print "Bye!")

          (let ((idx (string->number line)))
            (if (and idx (>= idx 0) (<= idx len))
                (let ((play-res (play res idx)))
                  (unless play-res
                    (print "An error occured when trying to play the video"))
                  (user-repl-int res len))
                (print "Numbers in range only, please!"))))))

  (user-repl-int res (length res)))

(define (main args)
  (iv:*fields* '(videoId title))
  (if (null? args)
      (usage (program-name))
      (let* ((search-string (args->can-args args))
             (res (search search-string)))
        (user-repl res))))

(main (command-line-arguments))
