#lang racket

(require racket/gui
         pict)

(provide (all-defined-out))

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

(define r (char->piece #\r))
(define b (char->piece #\b))
(define n (char->piece #\n))
(define q (char->piece #\q))
(define k (char->piece #\k))
(define p (char->piece #\p))
(define R (char->piece #\R))
(define B (char->piece #\B))
(define N (char->piece #\N))
(define Q (char->piece #\Q))
(define K (char->piece #\K))
(define P (char->piece #\P))
(define | | (char->piece #\space))

(define w-square (filled-rectangle SQUARE-SIZE SQUARE-SIZE
                    #:color (make-color 239 237 209)
                    #:draw-border? #f))

(define b-square (filled-rectangle SQUARE-SIZE SQUARE-SIZE
                    #:color (make-color 120 150 86)
                    #:draw-border? #f))

;; Get a square with the proper piece (or none)
;; given the board state and position (index in 64-element state string)
(define (pos-sq state index)
  (cc-superimpose (index->square index)
                  (eval (string->symbol (substring state index (add1 index))))))

;; From 64-char board state to list of composed squares
(define (pos->squares state)
  (define piece-chars (string->list state))
  (map (lambda (i)
         (pos-sq state i))
       (build-list 64 identity)))

(define (color->square color)
  (cond [(string=? color "white") w-square]
        [else b-square]))

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

(define select-piece-style
  (lambda (choice)
    (define selection (send choice get-string-selection))
    (set! piece-style selection)))
