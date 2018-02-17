#lang racket

(require racket/tcp)

(define (connect)
  (define-values (in out) (tcp-connect "freechess.org" 5000))
  (for ([i 26])
    (writeln (read-line in)))
  (writeln "guest\n\n" out)
  (for ([i 20])
    (writeln (read-line in)))
  (close-input-port in)
  (close-output-port out))
