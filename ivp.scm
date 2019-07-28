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
  (rename (only invidious.req *fields* search) (*fields* *fields*) (search iv:search))
  (rename (only invidious.uri watch) (watch iv:watch))
  srfi-42)

(:type line->numbers (string fixnum --> (or false (list-of fixnum))))
(define line->numbers
  (let ((re (irregex "^(\\d+(-\\d+)?)(,\\d+(-\\d+)?)*$"))
        (line->numbers-int
          (lambda (ln max) ; ln :: String
            (let* ((singl? (lambda (l) (null? (cdr l))))
                   (ln (the (list-of string)
                            (string-split ln ","))) ; [String]
                   (ln (the (list-of (list-of string))
                            (map (cut string-split <> "-") ln))) ; [[String]]
                   (ln (the (list-of (list-of fixnum))
                            (map (cut map string->number <>) ln)))) ; [[Int]]
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

(:type *player* (#!optional string -> string))
(define *player*
  (make-parameter
    "mpv"
    (lambda (str)
      (assert (string? str) "`*player*` must be a string")
      str)))

(:type args->can-args ((list-of string) --> string))
(define (args->can-args args) (string-join (map string-trim-both args) " "))

(define-type result (list string string fixnum))
(define-type results (list-of result))

(:type make-result (string string fixnum --> result))
(define (make-result vid-id title length-seconds)
  `(,vid-id ,title ,length-seconds))

(:type result-vid-id (result --> string))
(define (result-vid-id result)
  (car result))

(:type result-title (result --> string))
(define (result-title result)
  (cadr result))

(:type result-length-seconds (result --> fixnum))
(define (result-length-seconds result)
  (caddr result))

(:type vector->result ((vector-of (pair string (or string fixnum))) --> result))
(define (vector->result res)
  (:type !f? ((or false 'a) ('a -> 'b) --> (or false 'b)))
  (define (!f? x f) (if x (f x) x))

  (:type assoc-key (string (list-of (pair string (or string fixnum))) --> (or false string fixnum)))
  (define (assoc-key key alst) (!f? (assoc key alst string=) cdr))

  (let* ((lst (vector->list res))
         (vid-id (assoc-key "videoId" lst))
         (title (assoc-key "title" lst))
         (length-seconds (assoc-key "lengthSeconds" lst)))
    (make-result vid-id title length-seconds)))

(:type search (string -> results))
(define (search str)
  (map vector->result (iv:search #:q str)))

(:type print-results (results -> void))
(define (print-results results)
  (:type print-result (result -> void))
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
  (let* ((filtered-res (map (cut list-ref res <>) idxs))
         (ids (map result-vid-id filtered-res))
         (watch-urls (map iv:watch ids)))
    (process-spawn (*player*) watch-urls)))

(:type user-repl (results -> void))
(define (user-repl res)
  (let ((len (length res)))
    (let loop ()
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
              (loop)))))))

(define (main args)
  (*fields* '(videoId title lengthSeconds))
  (if (null? args)
      (usage (program-name))
      (let* ((search-string (args->can-args args))
             (res (search search-string)))
        (user-repl res))))

(main (command-line-arguments))
