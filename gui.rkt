#lang racket/gui

(require racket/gui
         pict
         "positions.rkt")

(define movenum 0)
(define padding 8)
(define square-size (+ 84 (* 2 padding)))
(define frame (new frame%
                   [label "dalkire"]
                   [width (* square-size 8)]
                   [height (+ (* square-size 8) 50)]))

(define piece-bb (bitmap "./pieces/alpha/alpha_bb.png"))
(define piece-wq (bitmap "./pieces/alpha/alpha_wq.png"))

(define pieces (list-ref states movenum))

(define (char->piece ch)
  ;; check to-lower is valid
  (define base "./pieces/alpha/alpha_")
  (define suffix ".png")
  (define color
    (cond [(char-lower-case? ch) "w"]
          [(char-upper-case? ch) "b"]))
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

(define my-canvas
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
          (set! count (add1 count))))]))

(new button% [parent frame]
     [label "<<"]
     [callback (lambda (button event)
                 (set! movenum (sub1 movenum))
                 (set! pieces (list-ref states movenum))
                 (send my-canvas refresh-now))])

(new button% [parent frame]
     [label ">>"]
     [callback (lambda (button event)
                 (set! movenum (add1 movenum))
                 (set! pieces (list-ref states movenum))
                 (send my-canvas refresh-now))])

(send frame show #t)
