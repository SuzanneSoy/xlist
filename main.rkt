#lang typed/racket/base

(require phc-toolkit/typed-untyped)
(define-typed/untyped-modules #:no-test
  (require (only-in type-expander define-type-expander)
           multi-id
           "caret-identifier.rkt"
           "infinity-identifier.rkt"
           "between.rkt"
           match-string
           racket/match
           (only-in phc-toolkit/typed-untyped when-typed)
           (only-in syntax/parse ...+)
           (for-syntax (rename-in racket/base
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
                       syntax/parse/experimental/template
                       syntax/stx
                       type-expander/expander)
           (for-meta 2 racket/base)
           (for-meta 2 syntax/parse))

  (provide xlist ^ ∞)

  (define-syntax stop
    (λ (stx) (raise-syntax-error 'stop "This is a private marker" stx)))

  (begin-for-syntax
    (define-syntax ~^
      (pattern-expander
       (λ (stx)
         (syntax-case stx ()
           [(_ pat ...)
            #`{~or {~seq #,(syntax-local-introduce #'^) pat ...}
                   {~seq {~optional #,(syntax-local-introduce #'^)}
                         (pat ...)}}]))))
    
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
      (pattern {~seq :base {~literal ^} power:nat}
               #:with (expanded …) (map (const #'base)
                                        (range (syntax-e #'power))))
      (pattern {~literal stop}
               #:with (expanded …) #'())
      (pattern e:base
               #:with (expanded …) #'(e)))

    (define-syntax-class repeat-spec
      #:literals (* + - ∞)
      (pattern (:nat))
      (pattern ({~optional :nat} +))
      (pattern ({~optional :nat} - {~optional {~or ∞ :nat}}))
      (pattern (*)))

    #;(define-splicing-syntax-class xlist-*-element
        #:attributes (base)
        (pattern :split-superscript-*-id)
        (pattern (~seq base :superscript-ish-*)))

    #;(define-splicing-syntax-class xlist-+-element
        #:attributes (base min)
        (pattern :split-superscript-+-id)
        (pattern (~seq base :superscript-ish-+)))

    (define ((xlist-type context) stx)
      ;; The order of clauses is important, as they otherwise overlap.
      (define xl
        (syntax-parser
          #:context context
          #:literals (^ * + - ∞ stop)
          [()
           #'Null]
          [rest:not-stx-list
           #'rest]
          [(#:rest rest)
           #'rest]
          [(stop . rest) ;; eliminate the private marker
           (xl #'rest)]
          [(s:with-superscripts . rest)
           (xl #'(s.expanded … . rest))]
          [(:base {~optional ^} *)
           #'(Listof base)]
          [(:base {~optional ^} * . rest)
           #:with R (gensym 'R)
           #`(Rec R (U (Pairof base R)
                       #,(xl #'rest)))]
          [(:base {~optional ^} + . rest)
           (xl #'(base ^ 1 + . rest))]
          [(:base ^ power:nat + . rest)
           (xl #'(base ^ power stop base * . rest))]
          [(:base ^ - . rest)
           (xl #'(base ^ 0 - . rest))]
          [(:base ^ from:nat - ∞ . rest)
           (xl #'(base ^ from + . rest))]
          [(:base ^ 0 - to:nat . rest)
           #`(U . #,(foldl (λ (iteration u*)
                             (syntax-case u* ()
                               [[(_ . base…rest) . _]
                                #`[(List* base . base…rest) . #,u*]]))
                           #`[(List* #,(xl #'rest))]
                           (range (syntax-e #'to))))]
          [(:base ^ from:nat - to:nat . rest)
           #:with difference (- (syntax-e #'to) (syntax-e #'from))
           (when (< (syntax-e #'difference) 0)
             (raise-syntax-error 'xlist
                                 "invalid range: m is larger than n"
                                 #'-))
           (xl #'(base ^ from stop base ^ 0 - difference . rest))]
          [(:base ^ from:nat - . rest)
           ;; "-" is not followed by a number, nor by ∞, so default to ∞.
           (xl #'(base ^ from - ∞ . rest))]
          [(e:fixed-repeat . rest)
           #`(List* e.expanded … #,(xl #'rest))]))
      (xl stx))





    ;; Match

    (define-syntax-class xlist-pattern
      (pattern (({~literal unquote-splicing} splice))
               #:with expanded #'splice)
      (pattern (pat)
               #:with expanded #'(list pat)))

    (define ((xlist-match context) stx)
      ;; The order of clauses is important, as they otherwise overlap.
      (define/with-syntax ooo #'(... ...))
      (define xl
        (syntax-parser
          #:context context
          #:literals (^ * + - ∞ stop)
          [()
           #'(list)]
          [rest:not-stx-list
           #'rest]
          [(#:rest rest)
           #'rest]
          [(stop . rest) ;; eliminate the private marker
           (xl #'rest)]
          [(({~literal unquote-splicing} splice) …+ . rest)
           #`(append splice … #,(xl #'rest))]
          [(s:with-superscripts . rest)
           (xl #'(s.expanded … . rest))]
          [(:base {~optional ^} * . rest)
           #:with R (gensym 'R)
           #`(list-rest-ish [] base ooo #,(xl #'rest))]
          [(:base {~optional ^} + . rest)
           (xl #'(base ^ 1 + . rest))]
          [(:base ^ power:nat + . rest)
           #:with ..power (format-id #'power "..~a" (syntax-e #'power))
           #`(list-rest-ish [] base ..power #,(xl #'rest))]
          [(:base ^ - . rest)
           (xl #'(base ^ 0 - . rest))]
          [(:base ^ from:nat - ∞ . rest)
           (xl #'(base ^ from + . rest))]
          [(:base ^ from:nat - to:nat . rest)
           (when (> (syntax-e #'from) (syntax-e #'to))
             (raise-syntax-error 'xlist
                                 "invalid range: m is larger than n"
                                 #'-))
           #`(list-rest-ish
              [(? (λ (_) ((between/c from to) (length occurrences))))]
              (and occurrences base) ooo #,(xl #'rest))]
          [(:base ^ from:nat - . rest)
           ;; "-" is not followed by a number, nor by ∞, so default to ∞.
           (xl #'(base ^ from - ∞ . rest))]
          ;; aliases
          [(:base {~or {~literal ...} {~literal ___}} . rest)
           #`(list-rest-ish [] base ooo #,(xl #'rest))]
          [(:base {~literal ...+} . rest)
           #`(list-rest-ish base ..1 #,(xl #'rest))]
          [(:base ellipsis:id . rest)
           #:when (regexp-match? #px"^\\.\\.[0-9]+$"
                                 (symbol->string (syntax-e #'ellipsis)))
           #`(list-rest-ish [] base ellipsis #,(xl #'rest))]
          [(e:fixed-repeat . rest)
           #`(list-rest-ish [] e.expanded … #,(xl #'rest))]))
      (xl stx))

    #;("This is completely wrong"
       ;; Expands 0 or more mandatory-doms for ->*
       (define-splicing-syntax-class fixed-repeated-type
         #:attributes ([mandatory 1])
         #:literals (^ * + - ∞ stop)
         (pattern {~seq :base {~^ power:nat}}
                  #:with (mandatory …) (map (const #'base)
                                            (range (syntax-e #'power))))
         (pattern {~seq :base {~^ from:nat - to:nat}}
                  #:when (= (syntax-e #'from) (syntax-e #'to))
                  #:with (mandatory …) (map (const #'base)
                                            (range (syntax-e #'from))))
         (pattern s:with-superscripts
                  #:with (:fixed-repeated-type) #'(s.expanded …))
         (pattern (~seq {~peek-not :mandatory-bounded-variadic-repeated-type}
                        {~peek-not :optional-bounded-variadic-repeated-type}
                        {~peek-not :mandatory-variadic-repeated-type}
                        {~peek-not :optional-variadic-repeated-type}
                        :base)
                  #:with (mandatory …) #'(base)))

       ;; Expands to 0 or more mandatory-doms and 0 or more optional-doms
       ;; for ->*
       (define-splicing-syntax-class mandatory-bounded-variadic-repeated-type
         #:attributes ([mandatory 1] [optional 1])
         #:literals (^ * + - ∞ stop)
         (pattern {~seq :base {~^ {~and from:nat {~not 0}} - to:nat}}
                  #:with (mandatory …) (map (const #'base)
                                            (range (syntax-e #'from)))
                  #:with (optional …) (map (const #'base)
                                           (range (- (syntax-e #'to)
                                                     (syntax-e #'from)))))
         (pattern s:with-superscripts
                  #:with (:mandatory-bounded-variadic-repeated-type)
                  #'(s.expanded …)))
    
       ;; Expands to 1 or more optional-doms for ->*
       (define-splicing-syntax-class optional-bounded-variadic-repeated-type
         #:attributes ([optional 1])
         #:literals (^ * + - ∞ stop)
         (pattern {~seq :base {~^ {~optional 0} - to:nat}}
                  #:with (optional …) (map (const #'base)
                                           (range (syntax-e #'to))))
         (pattern s:with-superscripts
                  #:with (:optional-bounded-variadic-repeated-type)
                  #'(s.expanded …)))

       ;; Expands to 0 or more mandatory-doms for ->* and possibly a rest clause
       (define-splicing-syntax-class mandatory-variadic-repeated-type
         #:attributes ([mandatory 1] [rest-clause 1])
         (pattern {~seq :base {~^ from:nat +}}
                  #:with (mandatory …) (map (const #'base)
                                            (range (syntax-e #'from)))
                  #:with (rest-clause …) #'(#:rest base))
         (pattern {~seq :base {~or + {~^ +}}}
                  #:with (:mandatory-variadic-repeated-type) #'(base ^ 1 +))
         (pattern {~seq :base {~^ from:nat - {~optional ∞}}}
                  #:with (:mandatory-variadic-repeated-type) #'(base ^ from +))
         (pattern s:with-superscripts
                  #:with (:mandatory-variadic-repeated-type)
                  #'(s.expanded …)))
    
       ;; Expands to a #:rest clause for ->*
       (define-splicing-syntax-class optional-variadic-repeated-type
         #:attributes ([rest-clause 1])
         #:literals (^ * + - ∞ stop)
         (pattern {~or {~seq :base {~^ {~optional 0} - {~optional ∞}}}
                       {~seq :base {~^ *}}
                       {~seq :base *}}
                  #:with (rest-clause …) #'(#:rest base))
         (pattern s:with-superscripts
                  #:with (:optional-variadic-repeated-type)
                  #'(s.expanded …)))
    
       (define ((xlist-builder-type context) stx)
         ;; The order of clauses is important, as they otherwise overlap.
         (syntax-parse stx
           #:context context
           #:literals (^ * + - ∞ stop)
           [(τᵢ:fixed-repeated-type
             …
             (~or (~seq τₘᵥ:mandatory-variadic-repeated-type)
                  (~seq {~optional τⱼ:mandatory-bounded-variadic-repeated-type}
                        τₖ:optional-bounded-variadic-repeated-type
                        …
                        {~optional τₙ:optional-variadic-repeated-type})))
            #:with range ((xlist-type context) stx)
            (template (->*
                       ;; mandatory
                       (τᵢ.mandatory
                        … …
                        {?? {?@ τₘᵥ.mandatory …}}
                        {?? {?@ τⱼ.mandatory …}})
                       ;; optional
                       ({?? {?@ τⱼ.optional …}}
                        τₖ.optional … …)
                       ;; #:rest
                       {?? {?@ τₘᵥ.rest-clause …}}
                       {?? {?@ τₙ.rest-clause …}}
                       ;; range
                       range))]))

       (define ((xlist-builder context) stx)
         #`(cast list
                 #,((xlist-builder-type context) stx)))))

  (define-multi-id xlist
    #:type-expander (λ (stx) ((xlist-type stx) (stx-cdr stx)))
    #:match-expander (λ (stx) ((xlist-match stx) (stx-cdr stx)))
    #;{#:call (λ (stx) ((xlist-builder stx) (stx-cdr stx)))})

  (define-match-expander list-rest-ish
    (syntax-parser
      #:literals (list list-rest-ish)
      #:datum-literals (list-rest)
      [(_ [c₁ …] e₁ … (list-rest-ish [c₂ …] e₂ … r))
       #'(list-rest-ish [c₁ … c₂ …] e₁ … e₂ … r)]
      [(_ [c₁ …] e₁ … (list-rest e₂ … r))
       #'(list-rest-ish [c₁ …] e₁ … e₂ … r)]
      [(_ [c₁ …] e₁ … (list e₂ …))
       #'(and (list e₁ … e₂ …) c₁ …)]
      [(_ [c₁ …] e₁ … r)
       #'(and (list-rest e₁ … r)
              c₁ …)]))

  (when-typed
   (provide xList #;xListBuilder)
   (define-type-expander (xList stx)
     ((xlist-type stx) (stx-cdr stx)))

   #;(define-type-expander (xListBuilder stx)
     ((xlist-builder-type stx) (stx-cdr stx)))))
