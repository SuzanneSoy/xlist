#lang typed/racket/base

(provide between/c)

(: between/c (→ Real Real (→ Real Boolean)))
(define ((between/c a b) v)
  (and (<= a v) (<= v b)))