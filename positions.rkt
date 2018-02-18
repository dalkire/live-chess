#lang racket/base

(provide states)

(define states
  '("RNBQKB RPPPP PPP     N      P      p        p   ppp  ppprnbqkbnr"
    "RNBQKB RPPPP PPP     N             P        p   ppp  ppprnbqkbnr"
    "RNBQKB RPPPP PPP     N             p            ppp  ppprnbqkbnr"
    "RNBQKB RPPP  PPP     N     P       p            ppp  ppprnbqkbnr"
    "RNBQKB RPPP  PPP     N     P       p         n  ppp  ppprnbqkb r"
    "RNBQKB RPP   PPP     N    PP       p         n  ppp  ppprnbqkb r"
    "RNBQKB RPP   PPP     N    PP       p      p  n  pp   ppprnbqkb r"
    "RNBQKB RPP   PPP     N     P       P      p  n  pp   ppprnbqkb r"
    "RNBQKB RPP   PPP     N     P       n      p     pp   ppprnbqkb r"
    "RNBQK  RPP   PPP     N    BP       n      p     pp   ppprnbqkb r"
    "RNBQK  RPP   PPP     N    BP       n      p     pp  bppprnbqk  r"
    "R BQK  RPP   PPP  N  N    BP       n      p     pp  bppprnbqk  r"
    "R BQK  RPP   PPP  n  N    BP              p     pp  bppprnbqk  r"
    "R BQK  RP    PPP  P  N    BP              p     pp  bppprnbqk  r"
    "R BQK  RP    PPP  P  N    BP              p     pp nbpppr bqk  r"
    "R BQ RK P    PPP  P  N    BP              p     pp nbpppr bqk  r"
    "R BQ RK P    PPP  P  N    BP              p     pp nbpppr bq rk "
    "R BQ RK P    PPP BP  N     P              p     pp nbpppr bq rk "
    "R BQ RK P    PPP BP  N     P              p  n  pp  bpppr bq rk "
    "R  Q RK P    PPP BP  N     P          B   p  n  pp  bpppr bq rk "
    "R  Q RK P    PPP BP  N     P          B   p  n ppp  bpp r bq rk "
    "R  Q RK P    PPP BP  N     P   B          p  n ppp  bpp r bq rk "
    "R  Q RK P    PPP BP  N     P   B   n      p    ppp  bpp r bq rk "
    "R  Q RK P    PPP BP  NB    P       n      p    ppp  bpp r bq rk "))
