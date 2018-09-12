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

(define control-panel (new vertical-panel%
                           [parent main-panel]
                           [min-width 400]
                           [stretchable-width #f]))

(define dragging #f)
(define dragging-x 0)
(define dragging-y 0)
(define throttle 0)

(define my-canvas%
  (class canvas%
    (define/override (on-char key-event)
      (cond
        [(equal? (send key-event get-key-code) 'right)
         (forward)]
        [(equal? (send key-event get-key-code) 'left)
         (backward)]))
    (define/override (on-event mouse-event)
      (set! dragging (send mouse-event dragging?))
      (set! throttle (add1 throttle))
      (when (and dragging (= 0 (modulo throttle 8)))
        (set! dragging-x (- (send mouse-event get-x) 50))
        (set! dragging-y (- (send mouse-event get-y) 50))
        (send this refresh-now)))
    (super-new)))

(define my-canvas
  (new my-canvas%
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
                      (cond
                        [(= i 3)
                         (draw-pict (list-ref (pos->squares pieces) i) dc dragging-x dragging-y)]
                        [else (draw-pict (list-ref (pos->squares pieces) i) dc x y)]))
                    (build-list 64 identity)))]))

(define forward
  (lambda ()
    (when (< movenum (sub1 (length states)))
      (set! movenum (add1 movenum))
      (set! pieces (list-ref states movenum))
      (send my-canvas refresh-now))))

(define backward
  (lambda ()
    (when (> movenum 0)
      (set! movenum (sub1 movenum))
      (set! pieces (list-ref states movenum))
      (send my-canvas refresh-now))))

(define message
  (new text-field%
       [parent control-panel]
       [label #f]
       [style (list 'multiple)]
       [min-width 400]
       [min-height 700]
       [stretchable-width #f]
       [init-value ""]))

(define prompt-panel
  (new horizontal-panel%
       [parent control-panel]))

(define prompt
  (new text-field%
       [parent prompt-panel]
       [label #f]
       ;; [min-width 400]
       ;; [stretchable-width #f]
       [init-value ""]))

(define prompt-button
  (new button%
       [parent prompt-panel]
       [label "Send"]
       [callback (lambda (button event)
                   (print "prompt send"))]))

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
                 (select-piece-style choice)
                 (send my-canvas refresh-now))])

(send frame show #t)
