#lang racket

(require racket/gui)

(define frame
  (new frame%
       [label "dalkire"]
       [width 800]
       [height 800]))

(define canvas
  (new editor-canvas% [parent frame]))

(define my-pb%
  (class pasteboard%
    (inherit set-before)
    (super-new)

    (define (after-select snip on?)
      (when on?
        (set-before snip #f)))

    (augment after-select)))

(define pb (new my-pb%))
(send pb set-selection-visible #f)
(send canvas set-editor pb)

(define piece-snip%
  (class image-snip%
    (super-new)

    (define/public (set-img path)
      (send this set-bitmap (read-bitmap path)))))

(define snip1 (new piece-snip%))
(send snip1 set-img "/home/dalkire/programming/racket/live-chess/pieces/alpha/alpha_bb.png")

(define snip2 (new piece-snip%))
(send snip2 set-img "/home/dalkire/programming/racket/live-chess/pieces/alpha/alpha_br.png")

(define snip3 (new piece-snip%))
(send snip3 set-img "/home/dalkire/programming/racket/live-chess/pieces/alpha/alpha_bp.png")

(define snip4 (new piece-snip%))
(send snip4 set-img "/home/dalkire/programming/racket/live-chess/pieces/alpha/alpha_bp.png")

(define snip5 (new piece-snip%))
(send snip5 set-img "/home/dalkire/programming/racket/live-chess/pieces/alpha/alpha_bp.png")

(define snip6 (new piece-snip%))
(send snip6 set-img "/home/dalkire/programming/racket/live-chess/pieces/alpha/alpha_bp.png")

(define snip7 (new piece-snip%))
(send snip7 set-img "/home/dalkire/programming/racket/live-chess/pieces/alpha/alpha_bp.png")

(define snip8 (new piece-snip%))
(send snip8 set-img "/home/dalkire/programming/racket/live-chess/pieces/alpha/alpha_bp.png")

(send pb insert snip1 0 0)
(send pb insert snip2 80 0)
(send pb insert snip3 160 0)
(send pb insert snip4 240 0)
(send pb insert snip5 320 0)
(send pb insert snip6 400 0)
(send pb insert snip7 480 0)
(send pb insert snip8 560 0)

(send frame show #t)
