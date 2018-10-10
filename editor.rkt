#lang racket

(require racket/gui
         "lib.rkt"
         "game.rkt")

(define POS1 "RNBQKBNRPPPP PPP            P      p        p   ppp  ppprnbqkbnr")
(define POS2 "RNBQKB RPPPP PPP     N      P      p        p   ppp  ppprnbqkbnr")

(define curr-index 0)

(define frame
  (new frame%
       [label "dalkire"]
       [width 700]
       [height 700]))

(define my-canvas%
  (class editor-canvas%
    (define/override (on-char key-event)
      (cond
        [(equal? (send key-event get-key-code) 'right)
         (forward)]
        [(equal? (send key-event get-key-code) 'left)
         (backward)]))
    (super-new)))

(define canvas
  (new my-canvas% [parent frame]))

(define board (read-bitmap "./boards/board_pinstripes.png"))

(define my-pb%
  (class pasteboard%
    (inherit set-before move-to)
    (super-new)

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (when before?
        (send dc draw-bitmap board 8 8)))

    (define (after-select snip on?)
      (when (and on? (is-a? snip piece-snip%))
          (set-before snip #f)))

    (define (after-move-to snip x y dragging?)
      (unless dragging?
          ;; (printf "after-move-to (~a, ~a) ~a ~a\n" x y snip dragging?)
          (when (is-a? snip piece-snip%)
            (let ((snap-pt (snap-to (pt (exact-floor x) (exact-floor y)) 80)))
              (move-to snip (pt-x snap-pt) (pt-y snap-pt))))))

    (augment after-select after-move-to)))

(define pb (new my-pb%))
(send pb set-selection-visible #f)
(send canvas set-editor pb)

(define piece-snip%
  (class image-snip%
    (inherit get-flags set-flags set-bitmap resize)
    (super-new)

    (define/public (set-img path)
      (set-bitmap (read-bitmap path)))

    (resize 80 80)
    (set-flags (cons 'handles-all-mouse-events (get-flags)))))

;; Should be the state of the board. A hash of square symbol keys to piece struct
;; values
(define (pos->board-hash pos)
  (make-hash
   (map pos-pair->piece-struct (pos->pos-pairs pos))))

;; Given a position string, return a list of (index . piece-char)
(define (pos->pos-pairs pos)
  (build-list (string-length pos)
              (λ (i)
                (cons i
                      (list-ref (string->list pos) i)))))

(define (piece->snip piece)
  (let ((piece-path (string-append
                     "./pieces/alpha/alpha_"
                     (symbol->string piece)
                     ".png")))
    (make-object piece-snip% piece-path 'png/alpha #t)))

(define piece-char->snip
  (compose piece->snip piece-char->piece))

(define (piece-char->maybe-piece piece-char)
  (if (equal? piece-char #\space)
      (None)
      (Some (piece-char->piece piece-char))))

(define (maybe-piece->maybe-snip maybe-piece)
  (match maybe-piece
    [(Some piece) (piece->snip)]
    [(None) (None)]))

(define maybe-piece-char->maybe-snip
  (compose maybe-piece->maybe-snip piece-char->maybe-piece))

;; A position pair (pos-pair) is an index and piece char.
;; A square piece here is a square struct of maybe piece and maybe piece snip.
(define (pos-pair->piece-struct pos-pair)
  (let* ((index (car pos-pair))
         (piece-char (cdr pos-pair))
         (maybe-piece-struct
          (if (equal? piece-char #\space)
              (None)
              (Some (piece
                     (piece-char->piece piece-char)
                     (piece-char->snip piece-char))))))
         (cons (index->square index) maybe-piece-struct)))

(define (style12->pos style12)
  (string-replace
   (string-replace style12 " " "")
   "-" " "))

(define game-list (map style12->pos moves))

(define board-hash (pos->board-hash (list-ref game-list curr-index)))

(define (forward)
  (let ((move-pair (diff (list-ref game-list curr-index)
                         (list-ref game-list (add1 curr-index)))))
    (displayln (cons curr-index move-pair))
    (print-pos (list-ref game-list (add1 curr-index)))
    (move pb (car move-pair) (cdr move-pair)))
  (set! curr-index (add1 curr-index)))

(define (backward)
  (let ((move-pair (diff (list-ref game-list curr-index)
                         (list-ref game-list (sub1 curr-index)))))
    (move pb (car move-pair) (cdr move-pair)))
  (set! curr-index (sub1 curr-index)))

;; (define (set-board board board-string)
;;   (map char->snip
;;        (string->list board-string)))

;; Set the board from a board-hash. Important to know that board-hash is mutable
;; and holds references to the piece snips
(define (set-board pb board-hash)
  (hash-for-each board-hash
                 (λ (square maybe-piece-struct)
                   (match maybe-piece-struct
                     [(Some piece-struct)
                      (let ((x (pt-x (square->pt square 80)))
                            (y (pt-y (square->pt square 80))))
                        (send pb insert (piece-snip piece-struct) x y))]
                     [(None) void]))))

(define (move-piece-snip pb snip square)
  (let ((pt (square->pt square 80)))
    (send pb move-to snip (pt-x pt) (pt-y pt))))

(define (move pb src-square dest-square)
  (let ((src-piece (hash-ref board-hash src-square))
        (dest-piece (hash-ref board-hash dest-square)))
    (match dest-piece
      [(Some piece-struct) (send pb delete (piece-snip piece-struct))]
      [(None) void])
    (match src-piece
      [(Some piece-struct)
       (move-piece-snip pb (piece-snip piece-struct) dest-square)]
      [(None) void]))
  (update-board-hash board-hash src-square dest-square))

(define (update-board-hash board-hash src-square dest-square)
   (hash-set! board-hash
              dest-square
              (hash-ref board-hash src-square))
  (hash-set! board-hash
             src-square
             (None)))

;; Attempting to build a differ (extract move from two positions)
(define (diff pos1 pos2)
  (define (diff-move move square p1 p2)
    (if (empty? move)
        square
        (if (symbol? move)
            ;; If the square becomes empty, it is the source square
            ;; otherwise it is the destination
            (if (equal? p2 #\space)
                (cons square move)
                (cons move square))
            move)))

  (second
   (foldl (λ (p1 p2 result)
            (let ((index (first result)))
              (if (equal? p1 p2)
                  (list (add1 index) (second result))
                  (list (add1 index)
                        (diff-move (second result) (index->square index) p1 p2)))))
          (list 0 empty)
          (string->list pos1)
          (string->list pos2))))

;; '((e8 #\k . #\space) (f8 #\space . #\r) (g8 #\space . #\k) (h8 #\r . #\space))
(define (delta->square-delta delta-list)
  (filter (λ (square-delta)
            (not (false? (cdr square-delta))))
          (map (λ (index delta)
                 (cons (index->square index) delta))
               (build-list 64 values)
               delta-list)))

;; '((e8 #\k . #\space) (f8 #\space . #\r) (g8 #\space . #\k) (h8 #\r . #\space))
(define (square-delta->move-hash square-deltas)
  ;; maybe use reduce to make hash w/ piece as key and square pair as value
  ;; if the piece isn't in the hash, add it w/ the square. if it is in the
  ;; hash, look at whether it's the source or destination and combine w/
  ;; current hash value accordingly

  ;; given a diff cons, return the piece-char that's moving
  (define (which-piece diff)
    ;; if the cdr is #\space, the piece vacating its square is the one moving
    (if (equal? #\space (cdr diff))
        (car diff)
        (cdr diff)))

  (foldl (λ (square-delta move-hash)
           (let ((piece-char (which-piece (cdr square-delta))))
             ;; if the piece is already in the hash, set the square to either
             ;; source or destination
             (if (hash-has-key? move-hash piece-char)
                 ;; if the piece moving is in the car, we are looking at the
                 ;; source square
                 (if (equal? (cadr square-delta) piece-char)
                     (hash-set move-hash piece-char
                               (cons (car square-delta)
                                     (hash-ref move-hash piece-char)))
                     (hash-set move-hash piece-char
                               (cons (hash-ref move-hash piece-char)
                                     (car square-delta))))
                 ;; the piece wasn't found in the hash so set the piece char
                 ;; as the key and the square as the value
                 (hash-set move-hash piece-char (car square-delta)))))
         (make-immutable-hash empty)
         square-deltas))

;; Trying to account for castling. See:
;; (filter pair? (delta (list-ref game-list 21) (list-ref game-list 22)))
(define (delta pos1 pos2)
  ;; Map over a list of 64 values and mark whether there is a difference between
  ;; the positions
  (map (λ (piece-char1 piece-char2)
         (if (equal? piece-char1 piece-char2)
             #f
             (cons piece-char1 piece-char2)))
       (string->list pos1)
       (string->list pos2)))

;; This function should generate a new position string when given the initial
;; position string and a src-square/dest-square string
(define (move->pos move curr-pos)
  (let ((src-index (square->index
                    (string->symbol (substring move 0 2))))
        (dest-index (square->index
                     (string->symbol (substring move 2 4))))
        (new-pos (substring curr-pos 0)))
    (string-set! new-pos src-index #\space)
    (string-set! new-pos dest-index (string-ref curr-pos src-index))
    new-pos))

(define (clear-square pb board-hash square)
  (let ((snip (piece-snip (hash-ref board-hash square))))
    (match snip
      [(Some ps) (send pb remove ps)]
      [(None) void])))

(define (print-pos pos)
  (displayln "")
  (map (λ (i)
         (displayln (string-join (map string (string->list (string-replace
                     (substring pos
                                (* 8 i)
                                (+ (* 8 i) 8))
                     " "
                     "-"))))))
       (build-list 8 values))
  (displayln ""))

(set-board pb board-hash)

(send frame show #t)
