(import
  scheme
  chicken.type
  (only chicken.base add1 cut)
  (only chicken.io read-line)
  (only chicken.irregex irregex irregex-match?)
  (only chicken.process process-execute process-fork process-wait)
  (only chicken.process-context command-line-arguments program-name)
  (only chicken.string string-split))

(import
  (only cling *usage* help process-arguments)
  (only defstruct defstruct)
  (only fmt columnar dsp fmt fmt-join)
  (only salmonella-log-parser prettify-time)
  (only srfi-1 append-map assoc every iota map)
  (only srfi-13 string-concatenate string-join string-trim-both string=)

  (only invidious.uri *fields* *host*)
  (rename (only invidious.req search) (search iv:search))
  (rename (only invidious.uri watch) (watch iv:watch)))

(: *player* (#!optional string -> string))
(define *player*
  (make-parameter
    "mpv"
    (lambda (str)
      (assert (string? str) "`*player*` must be a string")
      str)))

(defstruct options help instance page player region rest sort-by type)

(: usage (string -> void))
(define (usage pn)
  (print "Usage: " pn " [OPTION ...] SEARCH-TERM ..."))

(define (process-args args)
  (define *OPTS*
    `((((-? -h --help))
       "show this help message"
       ,(lambda (ret _) (update-options ret #:help #t)))

      (((--instance) . instance)
       "the instance to use"
       ,(lambda (ret instance) (update-options ret #:instance instance)))

      (((--page) . page)
       "the page number to show"
       ,(lambda (ret page) (update-options ret #:page (string->number page))))

      (((--player) . player)
       "the player to use"
       ,(lambda (ret player) (update-options ret #:player player)))

      (((--region) . region)
       "the search region"
       ,(lambda (ret region) (update-options ret #:region region)))

      (((--sort-by) . sort-by)
       "the sort method"
       ,(lambda (ret sort-by) (update-options ret #:sort-by sort-by)))

      (((--type) . type)
       "the search type (video, playlist, channel or all)"
       ,(lambda (ret type) (update-options ret #:type type)))))

  (define knil (make-options
                 #:help #f
                 #:instance #f
                 #:page #f
                 #:player "mpv"
                 #:region #f
                 #:rest '()
                 #:sort-by #f
                 #:type #f))

  (define (rest-kons ret rest)
    (update-options ret #:rest rest))

  (let ((ret (process-arguments *OPTS* knil rest-kons args)))
    (*usage* usage)
    (*player* (options-player ret))
    (when (options-instance ret)
      (*host* (options-instance ret)))
    ret))

(: args->can-args ((list-of string) --> string))
(define (args->can-args args) (string-join (map string-trim-both args) " "))

(: line->numbers (string fixnum --> (or false (list-of fixnum))))
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
                           (and (>= (car p) 0)
                                (if (singl? p)
                                    (< (car p) max)
                                    (and (< (car p) (cadr p))
                                         (< (cadr p) max)))))
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

(defstruct result type id title length-seconds)
(define-type results (list-of (struct result)))

(: vector->result ((vector-of (pair string (or string fixnum))) --> (struct result)))
(define (vector->result res)
  (: !f? ((or false 'a) ('a -> 'b) --> (or false 'b)))
  (define (!f? x f) (if x (f x) x))

  (: assoc-key (string (list-of (pair string (or string fixnum))) --> (or false string fixnum)))
  (define (assoc-key key alst) (!f? (assoc key alst string=) cdr))

  (define (id-by-type type)
    (cdr (assoc type
                '(("video" . "videoId")
                  ("playlist" . "playlistId")
                  ("channel" . "authorId")))))

  (let* ((lst (vector->list res))
         (id (assoc-key "videoId" lst))
         (title (assoc-key "title" lst))
         (length-seconds (assoc-key "lengthSeconds" lst)))
    (make-result #:type #f
                 #:id id
                 #:title title
                 #:length-seconds length-seconds)))

(: search ((struct options) -> results))
(define (search options)
  (map vector->result
       (iv:search
         #:q       (args->can-args (options-rest options))
         #:page    (options-page options)
         #:region  (options-region options)
         #:sort-by (options-sort-by options)
         ; TODO: Get ready for types other than video
         ;(options-type options)
         #:type    #f)))

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

(: play-list (results (list-of fixnum) --> void))
(define (play-list res idxs)
  (let* ((filtered-res (map (cut list-ref res <>) idxs))
         (ids (map result-id filtered-res))
         (watch-urls (map iv:watch ids)))
    (process-spawn (*player*) watch-urls)))

(: results->columns (results fixnum --> (list-of string)))
(define (results->columns results len)
  (map (compose string-concatenate (cut intersperse <> "\n"))
       `(,(map number->string (iota len))
          ,(map (compose prettify-time result-length-seconds) results)
          ,(map result-id results)
          ,(map result-title results))))

(: print-results ((list-of string) -> void))
(define (print-results columns)
  (fmt #t (apply columnar (map dsp columns))))

(: user-repl (results -> void))
(define (user-repl res)
  (define (quit? line)
    (or (eof-object? line)
        (string= line "q")
        (string= line "quit")))
  (let* ((len (length res))
         (columns (results->columns res len)))
    (let loop ()
      (print-results columns)
      (let ((line (read-line)))
        (if (quit? line)
            (print "Bye!")
            (let ((idxs (line->numbers line len)))
              (if idxs
                  (let ((play-res (play-list res idxs)))
                    (unless play-res
                      (print "An error occured when trying to play the video(s)")))
                  (print "Numbers in range only, please!"))
              (loop)))))))

(define (main args)
  ;(*fields* '(authorId lengthSeconds playlistId title type videoId))
  (*fields* '(lengthSeconds title videoId))
  (let ((options (process-args args)))
    (cond
      ((options-help options)
       (help (program-name)))
      ((null? (options-rest options))
       (usage (program-name)))
      (else
        (user-repl (search options))))))

(main (command-line-arguments))
