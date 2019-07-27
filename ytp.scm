(import srfi-1)

(import
  scheme
  (only chicken.io read-line)
  (only chicken.irregex irregex irregex-match?)
  (only chicken.process process-execute process-fork process-wait)
  (only chicken.process-context command-line-arguments program-name)
  (only chicken.string string-split))

(import
  (only salmonella-log-parser prettify-time)
  (only scm-utils !f? foreach/enum)
  (only srfi-1 append-map assoc map)
  (only srfi-13 string-join string-trim-both string=)
  (prefix (only invidious.req *fields* search) iv:))

(define line->numbers
  (let ((re (irregex "^(\\d+(-\\d+)?)(,\\d+(-\\d+)?)*$"))
        (line->numbers-int
          (lambda (ln max) ; ln :: String
            (let* ((singl? (lambda (l) (null? (cdr l))))
                   (ln (string-split ln ",")) ; [String]
                   (ln (map (cut string-split <> "-") ln)) ; [[String]]
                   (ln (map (cut map string->number <>) ln))) ; [(Int, Int)]
              (if (every (lambda (p)
                           (or (singl? p)
                               (and (>= (car p) 0)
                                    (< (car p) (cadr p))
                                    (< (cadr p) max))))
                         ln) ; all (uncurry (<)) ln
                  ; then
                  (append-map (lambda (p)
                                (if (singl? p)
                                    `(,(car p))
                                    (iota (add1 (- (cadr p)
                                                   (car p)))
                                          (car p))))
                              ln) ; [Int]
                  ; else
                  #f)))))
    (lambda (ln max)
      (let ((trimmed (string-trim-both ln)))
        (and (irregex-match? re trimmed)
             (line->numbers-int trimmed max))))))

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

(define (make-result vid-id title length-seconds)
  `(,vid-id ,title ,length-seconds))

(define (result-vid-id result)
  (car result))

(define (result-title result)
  (cadr result))

(define (result-length-seconds result)
  (caddr result))

(define (search str)
  (define (post-proc results)
    (define (vec->vid-id/title res)
      (define (assoc-key key alst) (!f? (assoc key alst string=) cdr))
      (let* ((lst (vector->list res))
             (vid-id (assoc-key "videoId" lst))
             (title (assoc-key "title" lst))
             (length-seconds (assoc-key "lengthSeconds" lst)))
        (make-result vid-id title length-seconds)))
    (map vec->vid-id/title results))
  (post-proc (iv:search #:q str)))

(define (print-results results)
  (define (print-result idx result)
    (let ((idx (number->string idx))
          (vid-id (result-vid-id result))
          (title (result-title result))
          (len-secs (prettify-time (result-length-seconds result))))
      (print (string-join `(,idx ,len-secs ,vid-id ,title) "\t"))))

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

(define (play-list res idxs)
  (let* ((ids (map (cut list-ref res <>) idxs))
         (watch-urls (map (lambda (p) (watch-url (result-vid-id p))) ids)))
    (process-spawn (*player*) watch-urls)))

(define (user-repl res)
  (define (user-repl-int res len)
    (print-results res)
    (let ((line (read-line)))
      (if (or (eof-object? line)
              (string= line "q")
              (string= line "quit"))
          (print "Bye!")

          (let ((idxs (line->numbers line len)))
            (if idxs
                (let ((play-res (play-list res idxs)))
                  (unless play-res
                    (print "An error occured when trying to play the video(s)")))
                (print "Numbers in range only, please!"))
            (user-repl-int res len)))))

  (user-repl-int res (length res)))

(define (main args)
  (iv:*fields* '(videoId title lengthSeconds))
  (if (null? args)
      (usage (program-name))
      (let* ((search-string (args->can-args args))
             (res (search search-string)))
        (user-repl res))))

(main (command-line-arguments))
