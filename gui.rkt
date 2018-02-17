#lang racket/gui

(require racket/gui
         pict)

(define padding 8)
(define square-size (+ 84 (* 2 padding)))
(define frame (new frame%
                   [label "dalkire"]
                   [width (* square-size 8)]
                   [height (* square-size 8)]))

(define piece-bb (bitmap "./pieces/alpha/alpha_bb.png"))
(define piece-wq (bitmap "./pieces/alpha/alpha_wq.png"))

(define pieces "rbnqknbrpppppppp                                PPPPPPPPRBNQKNBR")
(set! pieces "R BQ RK P    PPP BP  N     P              p  n  pp  bpppr bq rk ")

(define (char->piece ch)
  ;; check to-lower is valid
  (define base "./pieces/alpha/alpha_")
  (define suffix ".png")
  (define color
    (cond [(char-lower-case? ch) "b"]
          [(char-upper-case? ch) "w"]))
  (define piece (string (char-downcase ch)))
  (define path (string-append base color piece suffix))
  (inset (bitmap path) padding))

(define black-sq
  (filled-rectangle square-size square-size
                    #:color (make-color 120 150 86)
                    #:draw-border? #f))

(define white-sq
  (filled-rectangle square-size square-size
                    #:color (make-color 239 237 209)
                    #:draw-border? #f))

(new canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (define count 0)
        (for ([p pieces])
          (define-values (row col) (quotient/remainder count 8))
          (define y (* row square-size))
          (define x (* col square-size))
          (cond [(even? (+ row col))
                 (draw-pict white-sq dc x y)]
                [else
                 (draw-pict black-sq dc x y)])
          (unless (char-whitespace? p)
            (draw-pict (char->piece p) dc x y))
          (set! count (add1 count))))])

(send frame show #t)
