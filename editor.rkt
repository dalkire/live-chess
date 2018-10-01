#lang racket

(require racket/gui
         "lib.rkt")

(define POS "RNBQKB RPPPP PPP     N      P      p        p   ppp  ppprnbqkbnr")

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
          (printf "after-move-to (~a, ~a) ~a ~a\n" x y snip dragging?)
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

(define snip01
  (make-object piece-snip% "./pieces/alpha/alpha_br.png" 'png/alpha #t))
(define snip02
  (make-object piece-snip% "./pieces/alpha/alpha_bn.png" 'png/alpha #t))
(define snip03
  (make-object piece-snip% "./pieces/alpha/alpha_bb.png" 'png/alpha #t))
(define snip04
  (make-object piece-snip% "./pieces/alpha/alpha_bq.png" 'png/alpha #t))
(define snip05
  (make-object piece-snip% "./pieces/alpha/alpha_bk.png" 'png/alpha #t))
(define snip06
  (make-object piece-snip% "./pieces/alpha/alpha_bb.png" 'png/alpha #t))
(define snip07
  (make-object piece-snip% "./pieces/alpha/alpha_bn.png" 'png/alpha #t))
(define snip08
  (make-object piece-snip% "./pieces/alpha/alpha_br.png" 'png/alpha #t))

(define snip09
  (make-object piece-snip% "./pieces/alpha/alpha_bp.png" 'png/alpha #t))
(define snip10
  (make-object piece-snip% "./pieces/alpha/alpha_bp.png" 'png/alpha #t))
(define snip11
  (make-object piece-snip% "./pieces/alpha/alpha_bp.png" 'png/alpha #t))
(define snip12
  (make-object piece-snip% "./pieces/alpha/alpha_bp.png" 'png/alpha #t))
(define snip13
  (make-object piece-snip% "./pieces/alpha/alpha_bp.png" 'png/alpha #t))
(define snip14
  (make-object piece-snip% "./pieces/alpha/alpha_bp.png" 'png/alpha #t))
(define snip15
  (make-object piece-snip% "./pieces/alpha/alpha_bp.png" 'png/alpha #t))
(define snip16
  (make-object piece-snip% "./pieces/alpha/alpha_bp.png" 'png/alpha #t))


(define (set-board board board-string)
  (map char->snip
       (string->list board-string)))

(define (char->snip char)
  (make-object piece-snip% "./pieces/alpha/alpha_bp.png" 'png/alpha #t))

;; Should be the state of the board. A hash of square symbol keys to piece struct
;; values
(define (board-hash pos)
  (make-hash
   (map (lambda (index)
          (let ((square (index->square index)))
            (cons square (square->piece square pos))))
       (build-list 64 values))))


(send pb insert snip01 0 0)
(send pb insert snip02 80 0)
(send pb insert snip03 160 0)
(send pb insert snip04 240 0)
(send pb insert snip05 320 0)
(send pb insert snip06 400 0)
(send pb insert snip07 480 0)
(send pb insert snip08 560 0)

(send pb insert snip09 0 80)
(send pb insert snip10 80 80)
(send pb insert snip11 160 80)
(send pb insert snip12 240 80)
(send pb insert snip13 320 80)
(send pb insert snip14 400 80)
(send pb insert snip15 480 80)
(send pb insert snip16 560 80)

(define snip17
  (make-object piece-snip% "./pieces/alpha/alpha_wp.png" 'png/alpha #t))
(define snip18
  (make-object piece-snip% "./pieces/alpha/alpha_wp.png" 'png/alpha #t))
(define snip19
  (make-object piece-snip% "./pieces/alpha/alpha_wp.png" 'png/alpha #t))
(define snip20
  (make-object piece-snip% "./pieces/alpha/alpha_wp.png" 'png/alpha #t))
(define snip21
  (make-object piece-snip% "./pieces/alpha/alpha_wp.png" 'png/alpha #t))
(define snip22
  (make-object piece-snip% "./pieces/alpha/alpha_wp.png" 'png/alpha #t))
(define snip23
  (make-object piece-snip% "./pieces/alpha/alpha_wp.png" 'png/alpha #t))
(define snip24
  (make-object piece-snip% "./pieces/alpha/alpha_wp.png" 'png/alpha #t))

(define snip25
  (make-object piece-snip% "./pieces/alpha/alpha_wr.png" 'png/alpha #t))
(define snip26
  (make-object piece-snip% "./pieces/alpha/alpha_wn.png" 'png/alpha #t))
(define snip27
  (make-object piece-snip% "./pieces/alpha/alpha_wb.png" 'png/alpha #t))
(define snip28
  (make-object piece-snip% "./pieces/alpha/alpha_wq.png" 'png/alpha #t))
(define snip29
  (make-object piece-snip% "./pieces/alpha/alpha_wk.png" 'png/alpha #t))
(define snip30
  (make-object piece-snip% "./pieces/alpha/alpha_wb.png" 'png/alpha #t))
(define snip31
  (make-object piece-snip% "./pieces/alpha/alpha_wn.png" 'png/alpha #t))
(define snip32
  (make-object piece-snip% "./pieces/alpha/alpha_wr.png" 'png/alpha #t))

(send pb insert snip17 0 480)
(send pb insert snip18 80 480)
(send pb insert snip19 160 480)
(send pb insert snip20 240 480)
(send pb insert snip21 320 480)
(send pb insert snip22 400 480)
(send pb insert snip23 480 480)
(send pb insert snip24 560 480)

(send pb insert snip25 0 560)
(send pb insert snip26 80 560)
(send pb insert snip27 160 560)
(send pb insert snip28 240 560)
(send pb insert snip29 320 560)
(send pb insert snip30 400 560)
(send pb insert snip31 480 560)
(send pb insert snip32 560 560)

(send frame show #t)
