#lang racket

(require racket/gui
         pict)

(provide board draw-board)

(define SQUARE-SIZE 100)
(define SQUARES-PER-COL 8)
(define SQUARES-PER-ROW 8)

(define (color->square color)
  (cond [(string=? color "white")
         (filled-rectangle SQUARE-SIZE SQUARE-SIZE
                    #:color (make-color 239 237 209)
                    #:draw-border? #f)]
        [else
         (filled-rectangle SQUARE-SIZE SQUARE-SIZE
                    #:color (make-color 120 150 86)
                    #:draw-border? #f)]))

(define (index->color index)
  (define-values (row col) (quotient/remainder index 8))
  (cond [(even? (+ row col)) "white"]
        [else "black"]))

(define index->square (compose color->square index->color))

(define (draw-board p)
  (define-values (width height) (send p get-size))
  (define board-size (cond [(< width height) width]
                           [else height]))
  (set! board-size 200)
  (println (send p stretchable-width))
  (println (send p stretchable-height))
  (define square-size (quotient board-size 8))
  (board p square-size))

(define (board p square-size)
  (new canvas%
       [parent p]
       [min-height (* square-size 8)]
       [min-width (* square-size 8)]
       [stretchable-height #f]
       [stretchable-width #f]
       [paint-callback
        (lambda (canvas dc)
          (for-each (lambda (index)
                      (define-values (row col) (quotient/remainder index 8))
                      (define y (* row square-size))
                      (define x (* col square-size))
                      (draw-pict (index->square index) dc x y))
                    (build-list 64 identity)))]))
