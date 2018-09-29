#lang typed/racket

(define POS "RNBQKB RPPPP PPP     N      P      p        p   ppp  ppprnbqkbnr")

(provide (struct-out pt)
         (struct-out coord)
         snap-to)

(struct None ())
(struct (a) Some ([v : a])
  #:transparent)
(define-type (Maybe a) (U None (Some a)))

(struct pt ([x : Natural]
            [y : Natural])
  #:transparent)

(struct Board ([position : String]
               [square-size : Positive-Integer]
               [squares-per-row : Positive-Integer]
               [squares-per-column : Positive-Integer])
  #:transparent)

(struct coord ([column : Column]
               [row : Row])
  #:transparent)

(define board (Board POS 80 8 8))

(define-type Square (U 'a8 'b8 'c8 'd8 'e8 'f8 'g8 'h8
                       'a7 'b7 'c7 'd7 'e7 'f7 'g7 'h7
                       'a6 'b6 'c6 'd6 'e6 'f6 'g6 'h6
                       'a5 'b5 'c5 'd5 'e5 'f5 'g5 'h5
                       'a4 'b4 'c4 'd4 'e4 'f4 'g4 'h4
                       'a3 'b3 'c3 'd3 'e3 'f3 'g3 'h3
                       'a2 'b2 'c2 'd2 'e2 'f2 'g2 'h2
                       'a1 'b1 'c1 'd1 'e1 'f1 'g1 'h1))

(define-type SquareString (U "a8" "b8" "c8" "d8" "e8" "f8" "g8" "h8"
                             "a7" "b7" "c7" "d7" "e7" "f7" "g7" "h7"
                             "a6" "b6" "c6" "d6" "e6" "f6" "g6" "h6"
                             "a5" "b5" "c5" "d5" "e5" "f5" "g5" "h5"
                             "a4" "b4" "c4" "d4" "e4" "f4" "g4" "h4"
                             "a3" "b3" "c3" "d3" "e3" "f3" "g3" "h3"
                             "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2"
                             "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1"))

(define-type PieceChar (U #\p #\n #\b #\r #\q #\k
                          #\P #\N #\B #\R #\Q #\K
                          #\space))

(define-type Piece (U 'wp 'wn 'wb 'wr 'wq 'wk
                      'bp 'bn 'bb 'br 'bq 'bk
                      'none))

(define-type RowOrColumn (U 1 2 3 4 5 6 7 8))
(define-type Column RowOrColumn)
(define-type ColumnChar (U #\a #\b #\c #\d #\e #\f #\g #\h))
(define-type Row RowOrColumn)
(define-type RowChar (U #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8))


;; Translates a valid square symbol into the point that defines its origin
;; at the top-left of the given square
(: square->pt (-> Square Natural pt))
(define (square->pt sq square-size)
  (coord->pt (square->coord sq) square-size))

(: coord->pt (-> coord Natural pt))
(define (coord->pt c square-size)
  (pt (* (cast (- (coord-column c) 1) Natural)
         square-size)
      (* (cast (- 8 (coord-row c)) Natural)
         square-size)))

(: pt->coord (-> pt Natural coord))
(define (pt->coord p square-size)
  (square->coord (pt->square p square-size)))

;; Translates a valid square symbol into it's column and row coordinates
(: square->coord (-> Square coord))
(define (square->coord sq)
  (coord (square->column sq) (square->row sq)))

;; Given a valid square symbol, returns its row integer
(: square->row (-> Square Row))
(define (square->row sq)
  (row-char->row
   (square-string->row-char
    (square->string sq))))

;; Given a valid square symbol, returns its column integer
(: square->column (-> Square Column))
(define (square->column sq)
  (column-char->column
   (square-string->column-char
    (square->string sq))))

;; Translates a valid square symbol into its string representation
(: square->string (-> Square SquareString))
(define (square->string sq)
  (cast (symbol->string sq) SquareString))

;; Given a valid square string, returns the character representation of its row
(: square-string->row-char (-> SquareString RowChar))
(define (square-string->row-char square-string)
   (cast (string-ref square-string 1) RowChar))

;; Given a valid square string, returns its column character
(: square-string->column-char (-> SquareString ColumnChar))
(define (square-string->column-char square-string)
  (cast (string-ref square-string 0) ColumnChar))

;; Translates a character representing a row into a row integer
(: row-char->row (-> RowChar Row))
(define (row-char->row row-char)
  (let ((anchor (- (char->integer #\1) 1)))
    (cast (- (char->integer row-char) anchor) Row)))

;; Translates a character representing a column into a column integer
(: column-char->column (-> ColumnChar Column))
(define (column-char->column column-char)
   (let ((anchor (- (char->integer #\a) 1)))
     (cast (- (char->integer column-char) anchor) Column)))

;; Translates an y position to an integer value that represents
;; the column value. An integer between 1 and 8
(: pt-column->square-index (-> Natural Natural RowOrColumn))
(define (pt-column->square-index pt-column square-size)
  (let ((index (add1 (quotient pt-column square-size))))
    (if (> index 8)
        (cast 8 Column)
        (cast index Column))))

(: snap-to (-> pt Natural pt))
(define (snap-to p square-size)
  (coord->pt
   (pt->coord
    (square-origin->square->center p square-size) square-size) square-size))

;; Translates a x position to an integer value that represents
;; the row value. An integer between 1 and 8
(: pt-row->square-index (-> Natural Natural RowOrColumn))
(define (pt-row->square-index pt-row square-size)
  (let ((index (- 8 (quotient pt-row square-size))))
    (if (> index 8)
        (cast 8 Row)
        (cast index Row))))

;; Translate the point of a square's origin (upper-left) to its center
(: square-origin->square->center (-> pt Natural pt))
(define (square-origin->square->center origin square-size)
  (pt (cast (+ (pt-x origin) (quotient square-size 2)) Natural)
      (cast (+ (pt-y origin) (quotient square-size 2)) Natural)))

;; Translate the point of a square's center to its center (upper-left)
(: square-center->square->origin (-> pt Natural pt))
(define (square-center->square->origin center square-size)
  (pt (cast (- (pt-x center) (quotient square-size 2)) Natural)
      (cast (- (pt-y center) (quotient square-size 2)) Natural)))

;; Translates a column value from 1 to 8 into a column character from #\a to #\h
(: column->column-char (-> Column ColumnChar))
(define (column->column-char column)
  (let ((anchor (- (char->integer #\a) 1)))
    (cast (integer->char (+ anchor column)) ColumnChar)))

;; Translates a row value from 1 to 8 into a row character from #\a to #\h
(: row->row-char (-> Row RowChar))
(define (row->row-char row)
  (let ((anchor (- (char->integer #\1) 1)))
    (cast (integer->char (+ anchor row)) RowChar)))

;; Given separate characters for column and row, convert to square string
(: column-row-chars->square-string (-> ColumnChar RowChar SquareString))
(define (column-row-chars->square-string column-char row-char)
  (cast (string column-char row-char) SquareString))

;; Translates a valid square string into a square symbol
(: square-string->square (-> SquareString Square))
(define (square-string->square square-string)
  (cast (string->symbol square-string) Square))

;; Given a point and a square size, converts that point to a valid square symbol
(: pt->square (-> pt Natural Square))
(define (pt->square p square-size)
  (let ((column-char (column->column-char
                      (pt-column->square-index (pt-x p) square-size)))
        (row-char (row->row-char
                   (pt-row->square-index (pt-y p) square-size))))
    (square-string->square
     (column-row-chars->square-string column-char row-char))))

;; Given a square and the string state of the board, return the piece on that
;; square
(: square->piece (-> Square String (Maybe Piece)))
(define (square->piece sq board-string)
  (let ((char-at-index (string-ref board-string (square->index sq))))
    (if (equal? char-at-index #\space)
        (None)
        (Some (piece-char->piece (cast char-at-index PieceChar))))))

;; Translate a piece-char (black pieces are uppercase, white lower) to a piece
;; symbol
(: piece-char->piece (-> PieceChar Piece))
(define (piece-char->piece piece-char)
  (if (char-upper-case? piece-char)
      (cast
       (string->symbol (string #\b (char-downcase piece-char)))
       Piece)
      (cast
       (string->symbol (string #\w piece-char))
       Piece)))

;; Given a square symbol, what is that square's index in the 64-element string
;; representation
(: square->index (-> Square Integer))
(define (square->index sq)
  (+
   (* (- 8 (square->row sq)) 8)
   (- (square->column sq) 1)))

;; Translate in index of a 64-element string representation of a board into its
;; square symbol
(: index->square (-> Integer Square))
(define (index->square index)
  (coord->square
   (index->coord index)))

;; Translate in index of a 64-element string representation of a board into a
;; coord struct
(: index->coord (-> Integer coord))
(define (index->coord index)
  (let ((column (cast (+ 1 (modulo index 8)) Column))
        (row (cast (- 8 (floor (/ index 8))) Row)))
    (coord column row)))

;; Convert a coord struct into a square symbol
(: coord->square (-> coord Square))
(define (coord->square coord)
  (square-string->square
   (cast
    (string (column->column-char (coord-column coord))
            (row->row-char (coord-row coord)))
    SquareString)))

;; Whether the move is a legal one (currenly only concerned with the destination
;; being an empty square)
(: legal-move? (-> Square Square String Boolean))
(define (legal-move? orig dest board-string)
  (match (square->piece dest board-string)
    [(Some piece) #f]
    [(None) #t]))
