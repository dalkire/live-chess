#lang racket/gui

(require racket/gui
         pict
         "positions.rkt"
         "paths.rkt"
         "connect.rkt"
         "board.rkt")

(define movenum 0)
(define padding 8)
(define piece-style "usual")
(define square-size (+ 84 (* 2 padding)))
(define frame (new frame%
                   [label "dalkire"]))

(define pieces (list-ref states movenum))

(define main-panel (new horizontal-panel%
                        [parent frame]))

;; (define my-on-size-panel% (class panel% (super-new)
;;                             (define/override (on-size width height)
;;                               (lambda ()
;;                                 (print (send this get-width))
;;                                 (print (send this get-height))))))

(define board-panel (new panel%
                         [parent main-panel]
                         [min-width 800]
                         [min-height 800]))
;; (draw-board board-panel)

(define control-panel (new vertical-panel%
                           [parent main-panel]
                           [min-width 400]
                           [stretchable-width #f]))

;; (define (char->piece ch)
;;   (define base (string-append "./pieces/" piece-style "/" piece-style "_"))
;;   (define suffix ".png")
;;   (define color-char
;;     (cond [(char-lower-case? ch) "w"]
;;           [(char-upper-case? ch) "b"]))
;;   (define piece-char (string (char-downcase ch)))
;;   (define piece (string-append base color-char piece-char suffix))
;;   ;; (define piece (string-append piece-style "-" color-char piece-char))
;;   ;; (inset (bitmap (eval (string->symbol piece))) padding)
;;   (inset (bitmap piece) padding))


;; (define (draw-pos row col sq-size dc)
;;   (define y (* row sq-size))
;;   (define x (* col sq-size))
;;   (define sq
;;     (cond [(even? (+ row col)) white-sq]
;;           [else black-sq]))
;;   (draw-square sq x y)
;;   (unless (char))
;;   (draw-piece ))


(define my-canvas
  (new canvas%
       [parent board-panel]
       [style (list 'transparent)]
       [min-height (* square-size 8)]
       [min-width (* square-size 8)]
       [stretchable-height #f]
       [stretchable-width #f]
       [paint-callback
        (lambda (canvas dc)
          (for-each (lambda (i)
                      (define-values (row col) (index->row-col i))
                      (define y (* row square-size))
                      (define x (* col square-size))
                      (draw-pict (list-ref (pos->squares pieces) i) dc x y))
                    (build-list 64 identity)))]))

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
