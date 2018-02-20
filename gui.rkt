#lang racket/gui

(require racket/gui
         pict
         "positions.rkt"
         "paths.rkt"
         "connect.rkt")

(define movenum 0)
(define padding 8)
(define piece-style "usual")
(define square-size (+ 84 (* 2 padding)))
(define frame (new frame%
                   [label "dalkire"]))

(define pieces (list-ref states movenum))

(define main-panel (new horizontal-panel%
                        [parent frame]))
(define board-panel (new panel%
                         [parent main-panel]))
(define control-panel (new vertical-panel%
                           [parent main-panel]
                           [min-width 400]
                           [stretchable-width #f]))

(define (char->piece ch)
  (define base (string-append "./pieces/" piece-style "/" piece-style "_"))
  (define suffix ".png")
  (define color-char
    (cond [(char-lower-case? ch) "w"]
          [(char-upper-case? ch) "b"]))
  (define piece-char (string (char-downcase ch)))
  (define piece (string-append base color-char piece-char suffix))
  ;; (define piece (string-append piece-style "-" color-char piece-char))
  ;; (inset (bitmap (eval (string->symbol piece))) padding)
  (inset (bitmap piece) padding))

(define black-sq
  (filled-rectangle square-size square-size
                    #:color (make-color 120 150 86)
                    #:draw-border? #f))

(define white-sq
  (filled-rectangle square-size square-size
                    #:color (make-color 239 237 209)
                    #:draw-border? #f))

(define my-canvas
  (new canvas%
       [parent board-panel]
       [min-height (* square-size 8)]
       [min-width (* square-size 8)]
       [stretchable-height #f]
       [stretchable-width #f]
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

(define message (new text-field% [parent control-panel]
                     [label #f]
                     [style '(multiple)]
                     [min-width 400]
                     [stretchable-width #f]
                     [init-value ""]))

(define button-panel (new horizontal-panel%
                          [parent control-panel]
                          [min-height 50]
                          [stretchable-height #f]))

(new button%
     [parent button-panel]
     [label "<<"]
     [callback (lambda (button event)
                 (set! movenum (sub1 movenum))
                 (set! pieces (list-ref states movenum))
                 (send message set-value (number->string movenum))
                 (send my-canvas refresh-now))])

(new button%
     [parent button-panel]
     [label ">>"]
     [callback (lambda (button event)
                 (set! movenum (add1 movenum))
                 (set! pieces (list-ref states movenum))
                 (send message set-value (number->string movenum))
                 (send my-canvas refresh-now))])

(new button%
     [parent button-panel]
     [label "connect"]
     [callback (lambda (button event)
                 (connect message))])

(new choice%
     [parent button-panel]
     [label #f]
     [choices '("alpha" "line" "magnetic" "mark" "motif" "usual" "utrecht")]
     [selection 5]
     [callback (lambda (choice event)
                 (define selection (send choice get-string-selection))
                 (set! piece-style selection)
                 (send my-canvas refresh-now))])

(send frame show #t)
