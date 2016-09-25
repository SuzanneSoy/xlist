#lang typed/racket

(require (for-syntax phc-toolkit/untyped
                     syntax/parse
                     syntax/parse/experimental/template
                     racket/pretty
                     racket/list)
         (submod "implementation.rkt" typed)
         "caret-identifier.rkt"
         "infinity-identifier.rkt"
         "once-identifier.rkt"
         type-expander)

(provide split-xlist f-split-list m-split-xlist*)

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

#;(: cons2 (∀ (A B ...) (→ A (List B ...) (List A B ...))))
#;(define (cons2 a b)
    (cons a b))

(define-syntax (bounded-filter stx)
  (syntax-case stx ()
    [(_ 0 heads t l)
     #'(values (list . heads) l)]
    [(_ n (headᵢ …) t l)
     #`(if ((make-predicate t) l)
           (values (list headᵢ …) l)
           (bounded-filter #,(sub1 (syntax-e #'n))
                           (headᵢ … (car l))
                           t
                           (cdr l)))]))

(define-syntax m-split-xlist*
  (λ (stx)
    (displayln (syntax->datum stx))
    ((syntax-parser
       #:literals (^ + - * once ∞)
       [(_ v [v₁ vᵢ …] τ₁ ^ (once) {~seq τᵢ ^ *ᵢ} … #:rest r)
        (template
         (begin
           (define v₁ (car v))
           (m-split-xlist* (cdr v) [vᵢ …] (?@ τᵢ ^ *ᵢ) … #:rest r)))]
       [(_ v [v₁ vᵢ …] τ₁ ^ (power:nat) {~seq τᵢ ^ *ᵢ} … #:rest r)
        #:with (tmp-car …) (map (λ _ (gensym 'car)) (range (syntax-e #'power)))
        (template
         (begin
           (define-values (v₁ remaining-v)
             (let* ([remaining-v v]
                    (?@ [tmp-car (car remaining-v)]
                        [remaining-v (cdr remaining-v)])
                    …)
               (values (list tmp-car …) remaining-v)))
           (m-split-xlist* remaining-v [vᵢ …] (?@ τᵢ ^ *ᵢ) … #:rest r)))]
       [(_ v [v₁ vᵢ …] τ₁ ^ (power:nat +) {~seq τᵢ ^ *ᵢ} … #:rest r)
        #:with (tmp-car …) (map (λ _ (gensym 'car)) (range (syntax-e #'power)))
        (template
         (begin
           (define-values (v₁ remaining-v)
             (let* ([remaining-v v]
                    (?@ [tmp-car (car remaining-v)]
                        [remaining-v (cdr remaining-v)])
                    …)
               (define remaining-split
                 (m-split-list remaining-v
                               (xlist τ₁ ^ *₁ (?@ τᵢ ^ *ᵢ) … #:rest r)))
               (values (list* tmp-car … (car remaining-split))
                       (cdr remaining-split))))
           (m-split-xlist* remaining-v
                           [vᵢ …] (?@ τᵢ ^ *ᵢ) … #:rest r)))]
       [(_ v [v₁ vᵢ …] τ₁ ^ (from:nat - to:nat) {~seq τᵢ ^ *ᵢ} … #:rest r)
        #:with (tmp-car …) (map (λ _ (gensym 'car)) (range (syntax-e #'from)))
        #:with difference (- (syntax-e #'to) (syntax-e #'from))
        (when (< (syntax-e #'difference) 0)
          (raise-syntax-error 'xlist "invalid range: m is larger than n" #'-))
        (template
         (begin
           (define-values (v₁ remaining-v)
             (let* ([remaining-v v]
                    (?@ [tmp-car (car remaining-v)]
                        [remaining-v (cdr remaining-v)])
                    …)
               (define-values (before remaining-after)
                 (bounded-filter difference
                                 (tmp-car …)
                                 (xlist (?@ τᵢ ^ *ᵢ) … #:rest r)
                                 remaining-v))
               (values before
                       remaining-after)))
           (m-split-xlist* remaining-v
                           [vᵢ …] (?@ τᵢ ^ *ᵢ) … #:rest r)))]
       [(_ v [v₁ vᵢ …] τ₁ ^ *₁ {~seq τᵢ ^ *ᵢ} … #:rest r)
        (template
         (begin
           (define split
             (m-split-list v (xlist τ₁ ^ *₁ (?@ τᵢ ^ *ᵢ) … #:rest r)))
           (define v₁ (car split))
           (m-split-xlist* (cadr split) [vᵢ …] (?@ τᵢ ^ *ᵢ) … #:rest r)))]
       [(_ v [vr] #:rest r)
        #'(define vr v)])
     stx)))

(define-match-expander split-xlist
  (syntax-parser
    #:literals (^)
    [(_ pat . whole-τ)
     (define/with-parse ({~seq normalized-τᵢ ^ normalized-*ᵢ} … #:rest τ-rest)
       (normalize-xlist-type #'whole-τ this-syntax))
     
     (define-temp-ids "~a/v" (normalized-τᵢ …))
     ((λ (x) #;(pretty-write (syntax->datum x)) x)
      (template
       (app (λ (l)
              (m-split-xlist* l
                              [normalized-τᵢ/v … rest/v]
                              (?@ normalized-τᵢ ^ normalized-*ᵢ) …
                              #:rest τ-rest)
              (list normalized-τᵢ/v … rest/v))
            pat)))]))
