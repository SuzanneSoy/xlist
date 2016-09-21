#lang racket/base
(provide ^)

(require (for-syntax racket/base))

(define-syntax ^
  (λ (stx)
    (raise-syntax-error
     '^
     "The ^ identifier can only be used in some contexts"
     stx)))