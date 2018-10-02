#lang racket

(require racket/gui
         "lib.rkt")

(define POS1 "RNBQKBNRPPPP PPP            P      p        p   ppp  ppprnbqkbnr")
(define POS2 "RNBQKB RPPPP PPP     N      P      p        p   ppp  ppprnbqkbnr")

(define frame
  (new frame%
       [label "dalkire"]
       [width 700]
       [height 700]))

(define canvas
  (new editor-canvas% [parent frame]))

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

(define (set-board board board-string)
  (map char->snip
       (string->list board-string)))

(define (char->snip char)
  (make-object piece-snip% "./pieces/alpha/alpha_bp.png" 'png/alpha #t))

;; Should be the state of the board. A hash of square symbol keys to piece struct
;; values
(define (board-hash pos)
  (apply hash
         (flatten
          (map (lambda (index)
                 (let ((square (index->square index)))
                   (cons square (square->piece square pos))))
               (build-list 64 values)))))

(define (update-board pb board-hash)
  (hash-for-each board-hash
                 (lambda (square maybe-piece)
                   (let ((x (pt-x (square->pt square 80)))
                         (y (pt-y (square->pt square 80))))
                       (match maybe-piece
                         [(Some piece) (send pb insert (piece->snip piece) x y)]
                         [(None) void])))))

(define (piece->snip piece)
  (let ((piece-path (string-append
                     "./pieces/alpha/alpha_"
                     (symbol->string piece)
                     ".png")))
    (make-object piece-snip% piece-path 'png/alpha #t)))

(define (move pb src-square dest-square)
  (let ((p1 (square-origin->square-center (square->pt src-square 80) 80))
        (p2 (square->pt dest-square 80)))
    (let ((dest-snip (send pb find-snip (pt-x p2) (pt-y p2)))
          (piece-snip (send pb find-snip (pt-x p1) (pt-y p1))))
      (unless (equal? dest-snip #f)
        (send pb delete dest-snip))
      (unless (equal? piece-snip #f)
        (send pb move-to piece-snip (pt-x p2) (pt-y p2))))))

;; (string-append
;;              (substring POS1 0 3)
;;              " "
;;              (substring POS1 4 17)
;;              "Q"
;;              (substring POS1 18))

(update-board pb (board-hash POS1))

(send frame show #t)
