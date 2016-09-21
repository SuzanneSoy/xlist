#lang typed/racket/base

(require type-expander
         multi-id
         "caret-identifier.rkt"
         "infinity-identifier.rkt"
         (for-syntax
          (rename-in racket/base
                     [* mul]
                     [+ plus]
                     [compose ∘]
                     [... …])
          racket/syntax
          racket/match
          racket/contract
          racket/list
          racket/function
          racket/string
          (rename-in syntax/parse
                     [...+ …+])
          syntax/stx
          type-expander/expander))

(provide xlist xList ^ ∞)

(define-syntax stop
  (λ (stx) (raise-syntax-error 'stop "This is a private marker" stx)))

(begin-for-syntax
  (define number/rx #px"^(.*?)([⁰¹²³⁴⁵⁶⁷⁸⁹]+)$")
  (define */rx #px"^(.*?)⃰$")
  (define +/rx #px"^(.*?)([⁰¹²³⁴⁵⁶⁷⁸⁹]*)⁺$")
  (define -/rx #px"^(.*?)([⁰¹²³⁴⁵⁶⁷⁸⁹]*)⁻([⁰¹²³⁴⁵⁶⁷⁸⁹]*)$")
  
  (define (regexp-match/c rx)
    (and/c string? (λ (s) (regexp-match? rx s))))
  
  (define (id/c id)
    (and/c identifier? (λ (i) (free-identifier=? i id))))

  
  (define string-superscript-number/c (regexp-match/c number/rx))
  (define string-superscript-*/c      (regexp-match/c */rx))
  (define string-superscript-+/c      (regexp-match/c +/rx))
  (define string-superscript--/c      (regexp-match/c -/rx))
  
  (define string-superscript-any/c
    (or/c string-superscript-number/c
          string-superscript-*/c
          string-superscript-+/c
          string-superscript--/c))

  (define normal-rest/c
    (or/c (list/c (id/c #'^) exact-nonnegative-integer?)
          (list/c (id/c #'^) (id/c #'*))
          (list/c (id/c #'^) exact-nonnegative-integer? (id/c #'+))
          (list/c (id/c #'^)
                  exact-nonnegative-integer?
                  (id/c #'-)
                  (or/c (id/c #'∞) exact-nonnegative-integer?))))

  (define normal-string/c (cons/c string?
                                  normal-rest/c))
  (define normal-id/c     (cons/c (and/c identifier? (not/c (syntax/c '||)))
                                  normal-rest/c))

  (define/contract (string-superscripts->number superscripts)
    (-> string-superscript-number/c exact-nonnegative-integer?)
    (string->number
     (string-join
      (map (match-lambda ["⁰" "0"] ["¹" "1"] ["²" "2"] ["³" "3"] ["⁴" "4"]
                         ["⁵" "5"] ["⁶" "6"] ["⁷" "7"] ["⁸" "8"] ["⁹" "9"])
           (map string (string->list superscripts))))))

  (define/contract (string-superscripts->normal superscripts)
    (-> string-superscript-any/c
        normal-string/c)
    (define ->num string-superscripts->number)
    (match superscripts
      ;; Order is important, the regexpes overlap
      [(regexp -/rx      (list _ base n m))
       (list base
             #'^
             (if (string=? n "") 0 (->num n))
             #'-
             (if (string=? m "") #'∞ (->num m)))]
      [(regexp number/rx (list _ base n))   (list base #'^ (->num n))]
      [(regexp */rx      (list _ base))     (list base #'^ #'*)]
      [(regexp +/rx      (list _ base n))
       (list base #'^ (if (string=? n "") 1 (->num n)) #'+)]))

  (define/contract (id-superscripts->normal id)
    (-> identifier? (or/c #f normal-id/c))
    (define str (symbol->string (syntax-e id)))
    (if (string-superscript-any/c str)
        (match (string-superscripts->normal str)
          [(cons "" _) #f]
          [(cons base rest) (cons (format-id id "~a" base) rest)])
        #f))

  (define/contract (only-superscripts->normal id)
    (-> identifier? (or/c #f normal-rest/c))
    (define str (symbol->string (syntax-e id)))
    (if (string-superscript-any/c str)
        (match (string-superscripts->normal str)
          [(cons "" rest) rest]
          [_ #f])
        #f))

  (define-splicing-syntax-class with-superscripts
    (pattern (~seq id:id)
             #:do [(define normal (id-superscripts->normal #'id))]
             #:when normal
             #:with (expanded …) normal)
    (pattern (~seq base:expr super:id)
             #:do [(define normal (only-superscripts->normal #'super))]
             #:when normal
             #:with (expanded …) (cons #'base normal)))

  (define-syntax-class not-stx-list
    (pattern {~not (_ …)}))

  (define-syntax-class base
    #:literals (^ + *)
    (pattern {~and base {~not {~or ^ + *}}}))

  (define-splicing-syntax-class fixed-repeat
    (pattern {~seq :base {~literal ^} power:number}
             #:with (expanded …) (map (const #'base)
                                      (range (syntax-e #'power))))
    (pattern {~literal stop}
             #:with (expanded …) #'())
    (pattern e:base
             #:with (expanded …) #'(e)))

  (define-syntax-class repeat-spec
    #:literals (* + - ∞)
    (pattern (:number))
    (pattern ({~optional :number} +))
    (pattern ({~optional :number} - {~optional {~or ∞ :number}}))
    (pattern (*)))

  #;(define-splicing-syntax-class xlist-*-element
      #:attributes (base)
      (pattern :split-superscript-*-id)
      (pattern (~seq base :superscript-ish-*)))

  #;(define-splicing-syntax-class xlist-+-element
      #:attributes (base min)
      (pattern :split-superscript-+-id)
      (pattern (~seq base :superscript-ish-+)))

  (define (xlist-type context)
    ;; The order of clauses is important, as they otherwise overlap.
    (syntax-parser
      #:context context
      #:literals (^ * + - ∞ stop)
      [()
       #'Null]
      [rest:not-stx-list
       #'rest]
      [(stop . rest) ;; eliminate the private marker
       #'(xlist . rest)]
      [(s:with-superscripts . rest)
       #'(xlist s.expanded … . rest)]
      [(:base {~optional ^} *)
       #'(Listof base)]
      [(:base {~optional ^} * . rest)
       #:with R (gensym 'R)
       #'(Rec R (U (Pairof base R)
                   (xList . rest)))]
      [(:base {~optional ^} + . rest)
       #'(xlist base ^ 1 + . rest)]
      [(:base ^ power:nat + . rest)
       #'(xlist base ^ power stop base * . rest)]
      [(:base ^ - . rest)
       #'(xlist base ^ 0 - . rest)]
      [(:base ^ from:nat - ∞ . rest)
       #'(xlist base ^ from stop base * . rest)]
      [(:base ^ 0 - to:nat . rest)
       #`(U . #,(foldl (λ (iteration u*)
                         (syntax-case u* ()
                           [[(_ . base…rest) . _]
                            #`[(xlist base . base…rest) . #,u*]]))
                       #'[(xlist . rest)]
                       (range (syntax-e #'to))))]
      [(:base ^ from:nat - to:nat . rest)
       #:with difference (- (syntax-e #'to) (syntax-e #'from))
       (when (< (syntax-e #'difference) 0)
         (raise-syntax-error 'xlist
                             "invalid range: m is larger than n"
                             #'-))
       #`(xlist base ^ from stop base ^ 0 - difference . rest)]
      [(:base ^ from:nat - . rest)
       ;; "-" is not followed by a number, nor by ∞, so default to ∞.
       #`(xlist base ^ from - ∞ . rest)]
      [(e:fixed-repeat . rest)
       #'(List* e.expanded … (xList . rest))])))

(define-type-expander (xList stx)
  ((xlist-type stx) (stx-cdr stx)))

(define-multi-id xlist
  #:type-expander (λ (stx) ((xlist-type stx) (stx-cdr stx))))
