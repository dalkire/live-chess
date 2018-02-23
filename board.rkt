#lang racket

(require racket/gui
         pict)

(provide pos->squares index->row-col)

(define SQUARE-SIZE 100)
(define SQUARES-PER-COL 8)
(define SQUARES-PER-ROW 8)

(define POS "RNBQKB RPPPP PPP     N      P      p        p   ppp  ppprnbqkbnr")

(define piece-style "usual")
(define padding 8)

(define (char->piece ch)
  (define base (string-append "./pieces/" piece-style "/" piece-style "_"))
  (define suffix ".png")
  (define color-char
    (cond [(char-lower-case? ch) "w"]
          [(char-upper-case? ch) "b"]
          [else ""]))
  (define piece-char (string (char-downcase ch)))
  (define piece (string-append base color-char piece-char suffix))
  (cond [(char-whitespace? ch) (filled-rectangle 0 0)]
        [else (inset (bitmap piece) padding)]))


;; Get a square with the proper piece (or none)
;; given the board state and position (index in 64-element state string)
(define (pos-sq state index)
  (cc-superimpose (index->square index)
                  (char->piece (string-ref state index))))

;; From 64-char board state to list of composed squares
(define (pos->squares state)
  (define piece-chars (string->list state))
  (build-list (length piece-chars)
              (lambda (i)
                (pos-sq state i))))

(define (color->square color)
  (cond [(string=? color "white")
         (filled-rectangle SQUARE-SIZE SQUARE-SIZE
                    #:color (make-color 239 237 209)
                    #:draw-border? #f)]
        [else
         (filled-rectangle SQUARE-SIZE SQUARE-SIZE
                    #:color (make-color 120 150 86)
                    #:draw-border? #f)]))

;; Translate an index to the row-col values
(define (index->row-col index)
  (quotient/remainder index 8))

(define (index->color index)
  (define-values (row col) (quotient/remainder index 8))
  (cond [(even? (+ row col)) "white"]
        [else "black"]))

(define index->square (compose color->square index->color))

(define (draw-board p)
  ;; (define-values (width height) (send dc get-size))
  (define width (send p min-width))
  (define height (send p min-height))
  (define board-size (cond [(< width height) width]
                           [else height]))
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
