#lang racket

(require racket/gui)

(define frame
  (new frame%
       [label "dalkire"]
       [width 800]
       [height 800]))

(define canvas
  (new editor-canvas% [parent frame]))

(define board (read-bitmap "./boards/board_pinstripes.png"))

(define my-pb%
  (class pasteboard%
    (inherit set-before move-to)
    (super-new)

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (when before?
        (send dc draw-bitmap board 0 0)))

    (define (after-select snip on?)
      (when (and on? (is-a? snip piece-snip%))
          (set-before snip #f)))

    (define (after-move-to snip x y dragging?)
      (unless dragging?
          (printf "after-move-to (~a, ~a) ~a ~a\n" x y snip dragging?)
          (print "is this snip a piece? ")
          (println (is-a? snip piece-snip%))
          (if (is-a? snip piece-snip%)
              (move-to snip 400 400)
              (move-to snip 0 0))))

    (augment after-select after-move-to)))

(define pb (new my-pb%))
(send pb set-selection-visible #f)
(send canvas set-editor pb)

(define piece-snip%
  (class image-snip%
    (inherit get-flags set-flags set-bitmap)
    (super-new)

    (define/public (set-img path)
      (set-bitmap (read-bitmap path)))

    (set-flags (cons 'handles-all-mouse-events (get-flags)))))

(define snip1 (new piece-snip%))
(send snip1 set-img "./pieces/alpha/alpha_bb.png")

(define snip2 (new piece-snip%))
(send snip2 set-img "./pieces/alpha/alpha_br.png")

(define snip3 (new piece-snip%))
(send snip3 set-img "./pieces/alpha/alpha_bp.png")

(define snip4 (new piece-snip%))
(send snip4 set-img "./pieces/alpha/alpha_bp.png")

(define snip5 (new piece-snip%))
(send snip5 set-img "./pieces/alpha/alpha_bp.png")

(define snip6 (new piece-snip%))
(send snip6 set-img "./pieces/alpha/alpha_bp.png")

(define snip7 (new piece-snip%))
(send snip7 set-img "./pieces/alpha/alpha_bp.png")

(define snip8 (new piece-snip%))
(send snip8 set-img "./pieces/alpha/alpha_bp.png")


(send pb insert snip1 0 0)
(send pb insert snip2 80 0)
(send pb insert snip3 160 0)
(send pb insert snip4 240 0)
(send pb insert snip5 320 0)
(send pb insert snip6 400 0)
(send pb insert snip7 480 0)
(send pb insert snip8 560 0)

(send frame show #t)
