#lang info
(define collection "xlist")
(define deps '("base"
               "rackunit-lib"
               "mutable-match-lambda"
               "scribble-enhanced"
               "multi-id"
               "type-expander"
               "typed-racket-lib"
               "typed-racket-more"
               "phc-toolkit"
               "reprovide-lang"
               "match-string"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "typed-racket-doc"
                     "scribble-math"))
(define scribblings '(("scribblings/xlist.scrbl" () ("Data Structures"))))
(define pkg-desc
  (string-append "Fancy lists, with bounded or unbounded repetition of"
                 " elements. Can be used as a type or match pattern."))
(define version "0.9")
(define pkg-authors '(georges))
