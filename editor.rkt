#lang racket

(require racket/gui
         racket/draw)

(define frame
  (new frame%
       [label "dalkire"]
       [width 800]
       [height 800]))

(define canvas
  (new editor-canvas% [parent frame]))

(define pb (new pasteboard%))
(send pb set-selection-visible #f)
(send canvas set-editor pb)

;; (define bb-dc
;;   (new bitmap-dc%
;;        [bitmap (read-bitmap "/home/dalkire/programming/racket/live-chess/pieces/alpha/alpha_bb.png")]))

;; (send pb on-paint #f bb-dc 30 30 200 200 10 10 'no-caret)

(define snip1
  (make-object image-snip% (read-bitmap "/home/dalkire/programming/racket/live-chess/pieces/alpha/alpha_bb.png")))

(define snip2
  (make-object image-snip% (read-bitmap "/home/dalkire/programming/racket/live-chess/pieces/alpha/alpha_br.png")))

(define snip3
  (make-object image-snip% (read-bitmap "/home/dalkire/programming/racket/live-chess/pieces/alpha/alpha_bp.png")))

(define snip4
  (make-object image-snip% (read-bitmap "/home/dalkire/programming/racket/live-chess/pieces/alpha/alpha_bp.png")))

(define snip5
  (make-object image-snip% (read-bitmap "/home/dalkire/programming/racket/live-chess/pieces/alpha/alpha_bp.png")))

(define snip6
  (make-object image-snip% (read-bitmap "/home/dalkire/programming/racket/live-chess/pieces/alpha/alpha_bp.png")))

(define snip7
  (make-object image-snip% (read-bitmap "/home/dalkire/programming/racket/live-chess/pieces/alpha/alpha_bp.png")))

(define snip8
  (make-object image-snip% (read-bitmap "/home/dalkire/programming/racket/live-chess/pieces/alpha/alpha_bp.png")))

(send pb insert snip1 0 0)
(send pb insert snip2 80 0)
(send pb insert snip3 160 0)
(send pb insert snip4 240 0)
(send pb insert snip5 320 0)
(send pb insert snip6 400 0)
(send pb insert snip7 480 0)
(send pb insert snip8 560 0)

(send pb set-before snip8 #f)

(send frame show #t)
