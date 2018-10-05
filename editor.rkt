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
              (lambda (i)
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
         (maybe-piece (if (equal? piece-char #\space)
                         (None)
                         (Some (piece-char->piece piece-char))))
         (maybe-snip (if (equal? piece-char #\space)
                         (None)
                         (Some (piece-char->snip piece-char)))))
    (cons (index->square index) (piece maybe-piece maybe-snip))))

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
    (displayln (list-ref game-list (add1 curr-index)))
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
                 (lambda (square piece-struct)
                   (let ((maybe-snip (piece-snip piece-struct))
                         (x (pt-x (square->pt square 80)))
                         (y (pt-y (square->pt square 80))))
                       (match maybe-snip
                         [(Some snip)
                          (send pb insert snip x y)]
                         [(None) void])))))

(define (move-piece-snip pb snip square)
  (let ((pt (square->pt square 80)))
    (send pb move-to snip (pt-x pt) (pt-y pt))))

(define (move pb src-square dest-square)
  (let ((src-piece (hash-ref board-hash src-square))
        (dest-piece (hash-ref board-hash dest-square)))
    (match (piece-snip dest-piece)
      [(Some dest-snip) (send pb delete dest-snip)]
      [(None) void])
    (match (piece-snip src-piece)
      [(Some src-snip) (move-piece-snip pb src-snip dest-square)]
      [(None) void]))
  (update-board-hash board-hash src-square dest-square))
  ;; (let ((p1 (square-origin->square-center (square->pt src-square 80) 80))
  ;;       (p2 (square->pt dest-square 80)))
  ;;   (let ((dest-snip (send pb find-snip (pt-x p2) (pt-y p2)))
  ;;         (src-snip (send pb find-snip (pt-x p1) (pt-y p1))))
  ;;     (when dest-snip
  ;;       (send pb delete dest-snip))
  ;;     (when src-snip
  ;;       (send pb move-to src-snip (pt-x p2) (pt-y p2))))))


(define (update-board-hash board-hash src-square dest-square)
   (hash-set! board-hash
              dest-square
              (hash-ref board-hash src-square))
  (hash-set! board-hash
             src-square
             (piece (None) (None))))

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
   (foldl (lambda (p1 p2 result)
            (let ((index (first result)))
              (if (equal? p1 p2)
                  (list (add1 index) (second result))
                  (list (add1 index)
                        (diff-move (second result) (index->square index) p1 p2)))))
          (list 0 empty)
          (string->list pos1)
          (string->list pos2))))


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
  ;; (let* ((pt (square->pt square 80))
  ;;        (snip (send pb find-snip (pt-x pt) (pt-y pt))))
  ;;   (when snip (send pb remove snip))))

(set-board pb board-hash)

(send frame show #t)
