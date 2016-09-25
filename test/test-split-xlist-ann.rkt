#lang typed/racket

(require phc-toolkit
         xlist
         type-expander
         "../split-xlist.rkt")


(check-equal?:
 (((inst f-split-list Number (Listof Symbol))
   (make-predicate (Listof Symbol))) (ann '(1 2 3 a b)
                                          (xlist Number⃰ Symbol⃰)))
 : (List (Listof Number)
         (Listof Symbol))
 '((1 2 3) (a b)))

(check-equal?:
   (let ()
     (m-split-xlist* (ann '(1 2 3 d e f 7 8 9 . 42)
                          (xlist Number⃰ Symbol⃰ Number⃰ . Number))
                     [n1 s n2 r]
                     Number ^ {*}
                     Symbol ^ {*}
                     Number ^ {*}
                     #:rest Number)
     (list n1 s n2 r))
   : (List (Listof Number)
           (Listof Symbol)
           (Listof Number)
           Number)
   '((1 2 3) (d e f) (7 8 9) 42))

(check-equal?:
   (let ()
     (m-split-xlist* (ann '(1 2 3 d e f 7 8 9) (xlist Number⃰ Symbol⃰ Number⃰))
                     [n1 s n2 nul]
                     Number ^ {*}
                     Symbol ^ {*}
                     Number ^ {*}
                     #:rest Null)
     (list n1 s n2 nul))
   : (List (Listof Number)
           (Listof Symbol)
           (Listof Number)
           Null)
   '((1 2 3) (d e f) (7 8 9) ()))

(check-equal?:
   (match (ann '(1 2 3 d e f 7 8 9) (xlist Number⃰ Symbol⃰ Number⃰))
     [(split-xlist (list a b c d) Number⃰ Symbol⃰ Number⃰)
      (list d c b a)])
   : (List Null (Listof Number) (Listof Symbol) (Listof Number))
   '(() (7 8 9) (d e f) (1 2 3)))


(check-equal?:
 (match (ann '(1 2 3 d e f 7 8 9) (xlist Number Number⃰ Symbol⃰ Number⃰))
   [(split-xlist (list a b c d e) Number Number⃰ Symbol⃰ Number⃰)
    (list e d c b a)])
 : (List Null (Listof Number) (Listof Symbol) (Listof Number) Number)
 '(() (7 8 9) (d e f) (2 3) 1))

(check-equal?:
 (match (ann '(1 2 3 d e f 7 8 9) (xlist Number² Number⃰ Symbol⃰ Number⃰))
   [(split-xlist (list a b c d e) Number² Number⃰ Symbol⃰ Number⃰)
    (list e d c b a)])
 : (List Null
         (Listof Number)
         (Listof Symbol)
         (Listof Number)
         (List Number Number))
 '(() (7 8 9) (d e f) (3) (1 2)))

(check-equal?:
 (match (ann '(1 2 3 d e f 7 8 9) (xlist Number²⁻³ Symbol⃰ Number⃰))
   [(split-xlist (list a b c d) Number²⁻³ Symbol⃰ Number⃰)
    (list d c b a)])
 : (List Null
         (Listof Number)
         (Listof Symbol)
         (List* Number Number (U Null (List Number))))
 '(() (7 8 9) (d e f) (1 2 3)))

(check-equal?:
 (match (ann '(1 2 3 4 5 d e f 7 8 9) (xlist Number³⁻⁵ Symbol⃰ Number⃰))
   [(split-xlist (list a b c d) Number³⁻⁵ Symbol⃰ Number⃰)
    (list d c b a)])
 : (List Null
         (Listof Number)
         (Listof Symbol)
         (List* Number Number Number (U Null
                                        (List Number)
                                        (List Number Number))))
 '(() (7 8 9) (d e f) (1 2 3 4 5)))

(check-equal?:
 (match (ann '(1 2 3 4 d e f 7 8 9) (xlist Number³⁻⁵ Symbol⃰ Number⃰))
   [(split-xlist (list a b c d) Number³⁻⁵ Symbol⃰ Number⃰)
    (list d c b a)])
 : (List Null
         (Listof Number)
         (Listof Symbol)
         (List* Number Number Number (U Null
                                        (List Number)
                                        (List Number Number))))
 '(() (7 8 9) (d e f) (1 2 3 4)))

(check-equal?:
 (match (ann '(1 2 3 d e f 7 8 9) (xlist Number³⁻⁵ Symbol⃰ Number⃰))
   [(split-xlist (list a b c d) Number³⁻⁵ Symbol⃰ Number⃰)
    (list d c b a)])
 : (List Null
         (Listof Number)
         (Listof Symbol)
         (xlist Number³⁻⁵))
 '(() (7 8 9) (d e f) (1 2 3)))

(check-equal?:
 (match (ann '(1 2 3 4 d e f g 7 8 9) (xlist Number³⁻⁵ Symbol²⁻⁶ Number⃰))
   [(split-xlist (list a b c d) Number³⁻⁵ Symbol²⁻⁶ Number⃰)
    (list d c b a)])
 : (List Null
         (Listof Number)
         (List* Symbol Symbol (U Null
                                 (List Symbol)
                                 (List Symbol Symbol)
                                 (List Symbol Symbol Symbol)
                                 (List Symbol Symbol Symbol Symbol)))
         (List* Number Number Number (U Null
                                        (List Number)
                                        (List Number Number))))
 '(() (7 8 9) (d e f g) (1 2 3 4)))