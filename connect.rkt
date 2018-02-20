#lang racket

(require racket/tcp)

(provide connect)

(define (connect msg-dest)
  (define-values (in out) (tcp-connect "freechess.org" 5000))
  (for ([i 24])
    (send msg-dest set-value
          (string-append (send msg-dest get-value) (read-line in))))
  (writeln "guest\n\n" out)
  ;; (for ([i 20])
  ;;   (send msg-dest set-value
  ;;         (string-append (send msg-dest get-value) (read-line in))))
  (close-input-port in)
  (close-output-port out))
