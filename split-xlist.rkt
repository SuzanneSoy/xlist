#lang typed/racket

(require (for-syntax phc-toolkit/untyped
                     syntax/parse
                     syntax/parse/experimental/template)
         xlist
         "caret-identifier.rkt"
         type-expander)

(provide f-split-list)

(: f-split-list (∀ (A B) (→ (→ Any Boolean : B)
                            (→ (Rec R (U (Pairof A R) B))
                               (List (Listof A)
                                     B)))))
(define (f-split-list  pred-b?)
  (: recur (→ (Rec R (U (Pairof A R) B))
              (List (Listof A)
                    B)))
  (define (recur l)
    (if (null? l)
        (list '() (ann l B))
        (if (pred-b? l)
            (list '() l)
            (let ([split-rest (recur (cdr l))])
              (cons (cons (car l)
                          (car split-rest))
                    (cdr split-rest)))
            )))
  recur)

(define-syntax-rule (m-split-list v (xlist τ₁ ^ *₁ . whole-τ-rest))
  (((inst f-split-list τ₁ (xlist . whole-τ-rest))
    ;; TODO: could drop the tail type after the first mandatory repeat
    ;;       Not sure if that would make it possible to typecheck more easily
    ;;       though, as the rest of the type will be used to split the rest
    ;;       anyway.
    (make-predicate (xlist . whole-τ-rest)))
   v))

(module+ test
  (require phc-toolkit)

  (check-equal?:
   (((inst f-split-list Number (Listof Symbol))
     (make-predicate (Listof Symbol))) '(1 2 3 a b))
   : (List (Listof Number)
           (Listof Symbol))
   '((1 2 3) (a b))))

(define-syntax m-split-xlist*
  (syntax-parser
    #:literals (^)
    [(_ v [v₁ vᵢ …] {~seq τ₁ ^ *₁} {~seq τᵢ ^ *ᵢ} … #:rest r)
     ((λ (x) #;(displayln x) x)
      (template
       (begin
         (define split (m-split-list v (xlist τ₁ ^ *₁ (?@ τᵢ ^ *ᵢ) … #:rest r)))
         (define v₁ (car split))
         (m-split-xlist* (cadr split) [vᵢ …] (?@ τᵢ ^ *ᵢ) … #:rest r))))]
    [(_ v [vr] #:rest r)
     #'(define vr v)]))

(module+ test
  (require phc-toolkit)
  (check-equal?:
   (let ()
     (m-split-xlist* '(1 2 3 d e f 7 8 9 . 42)
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
     (m-split-xlist* '(1 2 3 d e f 7 8 9)
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
   '((1 2 3) (d e f) (7 8 9) ())))

(define-match-expander split-xlist
  (syntax-parser
    #:literals (^)
    [(_ pat . whole-τ)
     #:with ({~seq normalized-τᵢ ^ normalized-*ᵢ} … #:rest τ-rest)
     (normalize-xlist-type #'whole-τ this-syntax)

     (define-temp-ids "~a/v" (normalized-τᵢ …))
     ((λ (x) (displayln x) x)
      (template
       (app (λ (l)
              (m-split-xlist* l
                              [normalized-τᵢ/v … rest/v]
                              (?@ normalized-τᵢ ^ normalized-*ᵢ) …
                              #:rest τ-rest)
              (list normalized-τᵢ/v … rest/v))
            pat)))]))

(module+ test
  (check-equal?:
   (match '(1 2 3 d e f 7 8 9)
     [(split-xlist (list a b c d) Number⃰ Symbol⃰ Number⃰)
      (list d c b a)])
   : (List Null (Listof Number) (Listof Symbol) (Listof Number))
   '(() (7 8 9) (d e f) (1 2 3))))