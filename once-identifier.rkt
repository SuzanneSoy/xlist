#lang racket/base
(provide once)

(require (for-syntax racket/base))

(define-syntax once
  (λ (stx)
    (raise-syntax-error
     'once
     "The \"once\" identifier can only be used in some contexts"
     stx)))