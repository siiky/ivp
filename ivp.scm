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
  (only defstruct defstruct)
  (only fmt columnar dsp fmt fmt-join)
  (only optimism parse-command-line)
  (only salmonella-log-parser prettify-time)
  (only srfi-1 append-map assoc every iota map)
  (only srfi-13 string-concatenate string-join string-trim-both string=)
  (rename (only invidious.req *fields* search) (*fields* *fields*) (search iv:search))
  (rename (only invidious.uri watch) (watch iv:watch)))

(:type *player* (#!optional string -> string))
(define *player*
  (make-parameter
    "mpv"
    (lambda (str)
      (assert (string? str) "`*player*` must be a string")
      str)))

(:type usage (string -> void))
(define (usage pn)
  (print "Usage: " pn "[OPTIONS]... SEARCH-TERM..."))

(define-constant *PLAYER-OPTS* '(--player))
(define-constant *HELP-OPTS* '(-h --help))
(define-constant
  *OPTS*
  `((,*PLAYER-OPTS* . player)
    (,*HELP-OPTS*)))

(:type help (string -> void))
(define (help pn)
  (print
    pn " [OPTION...] [--] [SEARCH_TERM...]\n"
    "   -h --help                  show this help message\n"
    "      --player PLAYER         player to use"))

(defstruct options help player rest)

(define (process-args args)
  (define (kons ret opt/args)
    (let ((opt (car opt/args))
          (args (cdr opt/args)))
      (cond
        ((memq opt *PLAYER-OPTS*)
         (update-options ret #:player args))
        ((memq opt *HELP-OPTS*)
         (update-options ret #:help #t))
        ((eq? '-- opt)
         (update-options ret #:rest args)))))

  (let* ((pargs (parse-command-line args *OPTS*))
         (knil (make-options #:help #f #:player "mpv" #:rest '()))
         (ret (foldl kons knil pargs)))
    (*player* (options-player ret))
    ret))

(:type args->can-args ((list-of string) --> string))
(define (args->can-args args) (string-join (map string-trim-both args) " "))

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

(define-type result (list string string fixnum))
(define-type results (list-of result))

(defstruct result vid-id title length-seconds)

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
    (make-result #:vid-id vid-id
                 #:title title
                 #:length-seconds length-seconds)))

(:type search (string -> results))
(define (search str)
  (map vector->result (iv:search #:q str)))

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

(:type play-list (results (list-of fixnum) --> void))
(define (play-list res idxs)
  (let* ((filtered-res (map (cut list-ref res <>) idxs))
         (ids (map result-vid-id filtered-res))
         (watch-urls (map iv:watch ids)))
    (process-spawn (*player*) watch-urls)))

(:type results->columns (results fixnum --> (list-of string)))
(define (results->columns results len)
  (map (compose string-concatenate (cut intersperse <> "\n"))
       `(,(map number->string (iota len))
          ,(map (compose prettify-time result-length-seconds) results)
          ,(map result-vid-id results)
          ,(map result-title results))))

(:type print-results ((list-of string) -> void))
(define (print-results columns)
  (fmt #t (apply columnar (map dsp columns))))

(:type user-repl (results -> void))
(define (user-repl res)
  (let* ((len (length res))
         (columns (results->columns res len)))
    (let loop ()
      (print-results columns)
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
  (let ((options (process-args args)))
    (cond
      ((options-help options)
       (help (program-name)))
      ((null? (options-rest options))
       (usage (program-name)))
      (else
        (let* ((search-string (args->can-args (options-rest options)))
               (res (search search-string)))
          (user-repl res))))))

(main (command-line-arguments))
