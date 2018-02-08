#lang racket

(require racket/tcp)

(let-values ([(in out) (tcp-connect "freechess.org" 5000)])
    (for ([i 40])
      (writeln (read-line in)))
    (close-input-port in)
    (close-output-port out))
