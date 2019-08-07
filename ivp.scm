(import
  (except scheme map member)
  chicken.type
  (only chicken.base add1 butlast cut intersperse)
  (only chicken.io read-line)
  (only chicken.irregex irregex irregex-match?)
  (only chicken.process process-execute process-fork process-wait)
  (only chicken.process-context command-line-arguments program-name)
  (only chicken.string string-split))

(import
  (only cling *usage* arg cling help process-arguments)
  (only defstruct defstruct)
  (only fmt dsp fmt fmt-join tabular)
  (only salmonella-log-parser prettify-time)
  (only srfi-1 append-map every iota last map member)
  (only srfi-13 string-concatenate string-join string-trim-both string=)

  (only invidious.uri *fields* *host*)
  (rename (only invidious.req search) (search iv:search))
  (rename (only invidious.uri watch) (watch iv:watch)))

(: !f? ((or false 'a) ('a -> 'b) --> (or false 'b)))
(define (!f? x f) (if x (f x) x))

(: ?? ((or false 'a) 'a --> 'a))
(define (?? x d) (or x d))

(define (singl? l)
  (and (pair? l)
       (null? (cdr l))))

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

(: sep-list-of-options ((list-of string) string string --> string))
(define (sep-list-of-options options sep last-sep)
  (string-append (string-join (butlast options) sep) last-sep (last options)))

(define (string-surround str sur)
  (string-append sur str sur))

(define (die . rest)
  (error (string-concatenate rest)))

(define *OPTS*
  (cling
    (lambda (ret _ rest)
      (update-options ret #:rest rest))

    (arg '((-? -h --help))
         #:help "show this help message"
         #:kons (lambda (ret _ _) (update-options ret #:help #t)))

    (arg '((--instance) . instance)
         #:help "the instance to use"
         #:kons (lambda (ret _ instance) (update-options ret #:instance instance)))

    (arg '((--page) . page)
         #:help "the page number to show"
         #:kons (lambda (ret _ page) (update-options ret #:page (string->number page))))

    (arg '((--player) . player)
         #:help "the player to use"
         #:kons (lambda (ret _ player) (update-options ret #:player player)))

    (arg '((--region) . region)
         #:help "the search region"
         #:kons (lambda (ret _ region) (update-options ret #:region region)))

    (arg '((--sort-by) . sort-by)
         #:help "the sort method"
         #:kons (lambda (ret _ sort-by) (update-options ret #:sort-by sort-by)))

    (arg '((--type) . type)
         #:help "the search type (video, playlist, channel or all)"
         #:kons (lambda (ret _ type)
                  (let ((types '("all" "channel" "playlist" "video")))
                    (if (member type types string=)
                        (update-options ret #:type type)
                        (die "--type: Expected one of "
                             (sep-list-of-options
                               (map (cut string-surround <> "`") types)
                               ", " " or ")
                             " but got " (string-surround type "`"))))))))

(define (process-args args)
  (define knil
    (make-options
      #:help #f
      #:instance #f
      #:page #f
      #:player "mpv"
      #:region #f
      #:rest '()
      #:sort-by #f
      #:type #f))

  (let ((ret (process-arguments *OPTS* knil args)))
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
            (let* ((ln (the (list-of string)
                            (string-split ln ","))) ; [String]
                   (ln (the (list-of (list-of string))
                            (map (cut string-split <> "-") ln))) ; [[String]]
                   (ln (the (list-of (list-of fixnum))
                            (map (cut map string->number <>) ln)))) ; [[Int]]
              (if (every
                    (lambda (p)
                      (and (>= (car p) 0)
                           (if (singl? p)
                               (< (car p) max)
                               (and (< (car p) (cadr p))
                                    (< (cadr p) max)))))
                    ln) ; all (uncurry (<)) ln
                  ; then
                  (append-map
                    (lambda (p)
                      (if (singl? p)
                          p
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

(defstruct result type id name length-seconds)
(define-type results (list-of (struct result)))

(: vector->result ((vector-of (pair string (or string fixnum))) --> (struct result)))
(define (vector->result res)
  (: assoc-key (string (list-of (pair string (or string fixnum))) --> (or false string fixnum)))
  (define (assoc-key key alst)
    (!f? (assoc key alst) cdr))

  (define (id-by-type type)
    (case type
      ((channel)  "authorId")
      ((playlist) "playlistId")
      ((video)    "videoId")
      (else #f)))

  (define (name-by-type type)
    (case type
      ((channel)        "author")
      ((playlist video) "title")
      (else #f)))

  (let* ((lst (vector->list res))
         (type (string->symbol (assoc-key "type" lst)))
         (id-key (id-by-type type))
         (id (assoc-key id-key lst))
         (name-key (name-by-type type))
         (name (assoc-key name-key lst))
         ; channel and playlist dont have time
         (length-seconds (and (eq? 'video type)
                              (assoc-key "lengthSeconds" lst))))
    (make-result #:type type
                 #:id id
                 #:name name
                 #:length-seconds length-seconds)))

(: search ((struct options) -> results))
(define (search options)
  (map vector->result
       (iv:search
         #:q       (args->can-args (options-rest options))
         #:page    (options-page options)
         #:region  (options-region options)
         #:sort-by (options-sort-by options)
         #:type    (options-type options))))

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
         (watch-urls (map (lambda (r)
                            (iv:watch (result-id r)
                                      (result-type r)))
                          filtered-res)))
    (process-spawn (*player*) watch-urls)))

(: results->columns (results fixnum --> (list-of string)))
(define (results->columns results len)
  (define (type->type-tag type)
    (case type
      ((video)    "V")
      ((playlist) "P")
      ((channel)  "C")
      (else " "))) ; Shouldn't happen

  (map (cut fmt-join dsp <> "\n")
       (list
         (map number->string (iota len))
         (map (compose type->type-tag result-type) results)
         (map result-id results)
         (map ; video length ("" for playlists and channels)
           (compose
             (cut ?? <> "")
             (cut !f? <> prettify-time)
             result-length-seconds)
           results)
         (map result-name results))))

(: print-results ((list-of string) -> void))
(define print-results
  (let ((res #f))
    (lambda (columns)
      (unless res
        (set! res (intersperse (map dsp columns) (dsp " "))))
      (fmt #t (apply tabular res)))))

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
  (*fields* '(author authorId lengthSeconds playlistId title type videoId))
  (let ((options (process-args args)))
    (cond
      ((options-help options)
       (help *OPTS* (program-name)))
      ((null? (options-rest options))
       (usage (program-name)))
      (else
        (user-repl (search options))))))

(main (command-line-arguments))
