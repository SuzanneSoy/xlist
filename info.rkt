#lang info
(define collection "xlist")
(define deps '("base"
               "rackunit-lib"
               "mutable-match-lambda"
               "scribble-enhanced"
               "multi-id"
               "type-expander"
               "typed-racket-lib"
               "typed-racket-more"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "typed-racket-doc"
                     "scribble-math"
                     "match-string"))
(define scribblings '(("scribblings/xlist.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(georges))
