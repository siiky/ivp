(import
  scheme
  (only chicken.base add1 cut)
  (only chicken.io read-line)
  (only chicken.irregex irregex irregex-match?)
  (only chicken.process process-execute process-fork process-wait)
  (only chicken.process-context command-line-arguments program-name)
  (only chicken.string string-split)
  (rename chicken.type (: :type)))

(import
  (only salmonella-log-parser prettify-time)
  (only srfi-1 append-map assoc every iota map)
  (only srfi-13 string-join string-trim-both string=)
  (rename invidious.req (search iv:search) (*fields* *fields*))
  srfi-42)

(define line->numbers
  (let ((re (irregex "^(\\d+(-\\d+)?)(,\\d+(-\\d+)?)*$"))
        (line->numbers-int
          (lambda (ln max) ; ln :: String
            (let* ((singl? (lambda (l) (null? (cdr l))))
                   (ln (string-split ln ",")) ; [String]
                   (ln (map (cut string-split <> "-") ln)) ; [[String]]
                   (ln (map (cut map string->number <>) ln))) ; [[Int]]
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

(:type usage (string -> void))
(define (usage pn) (print "Usage: " pn " SEARCH-TERM..."))

(define *player*
  (make-parameter
    "mpv"
    (lambda (str)
      (assert (string? str) "`*player*` must be a string")
      str)))

(:type watch-url (string --> string))
(define (watch-url vid-id) (string-append "https://invidio.us/watch?v=" vid-id))

(:type args->can-args ((list-of string) --> string))
(define (args->can-args args) (string-join (map string-trim-both args) " "))

(define (make-result vid-id title length-seconds)
  `(,vid-id ,title ,length-seconds))

(define (result-vid-id result)
  (car result))

(define (result-title result)
  (cadr result))

(define (result-length-seconds result)
  (caddr result))

(define (vector->result res)
  (define (!f? x f) (if x (f x) x))
  (define (assoc-key key alst) (!f? (assoc key alst string=) cdr))
  (let* ((lst (vector->list res))
         (vid-id (assoc-key "videoId" lst))
         (title (assoc-key "title" lst))
         (length-seconds (assoc-key "lengthSeconds" lst)))
    (make-result vid-id title length-seconds)))

(define (search str)
  (map vector->result (iv:search #:q str)))

(define (print-results results)
  (define (print-result idx result)
    (let ((idx (number->string idx))
          (vid-id (result-vid-id result))
          (title (result-title result))
          (len-secs (prettify-time (result-length-seconds result))))
      (print (string-join `(,idx ,len-secs ,vid-id ,title) "\t"))))

  (do-ec
    (:list result (index idx) results)
    (print-result idx result)))

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
  (*fields* '(videoId title lengthSeconds))
  (if (null? args)
      (usage (program-name))
      (let* ((search-string (args->can-args args))
             (res (search search-string)))
        (user-repl res))))

(main (command-line-arguments))
