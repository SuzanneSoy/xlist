#lang typed/racket/base

(require phc-toolkit/typed-untyped)
(define-typed/untyped-modules #:no-test
  (require racket/require
           (only-in type-expander define-type-expander)
           multi-id
           "caret-identifier.rkt"
           "infinity-identifier.rkt"
           "once-identifier.rkt"
           "between.rkt"
           match-string
           racket/match
           (only-in phc-toolkit/typed-untyped when-typed)
           (only-in syntax/parse ...+)
           (for-syntax "caret-identifier.rkt"
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
                       syntax/parse/experimental/template
                       (subtract-in syntax/stx phc-toolkit/untyped)
                       type-expander/expander
                       phc-toolkit/untyped
                       racket/pretty)
           (for-meta 2 racket/base)
           (for-meta 2 syntax/parse))

  (provide xlist ^ ∞ once (for-syntax normalize-xlist-type))

  (begin-for-syntax
    (define-syntax ~^
      (pattern-expander
       (λ (stx)
         (syntax-case stx ()
           [(_ pat ...)
            #`{~or {~seq {~literal ^} pat ...}
                   {~seq {~optional {~literal ^}}
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

    (define-syntax-class not-stx-pair
      (pattern {~not (_ . _)}))

    (define-syntax-class base
      #:literals (^ + *)
      (pattern {~and base {~not {~or ^ + *}}}))

    (define-splicing-syntax-class fixed-repeat
      (pattern {~seq :base {~^ power:nat}}
               #:with (expanded …) (map (const #'base)
                                        (range (syntax-e #'power))))
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
          #:literals (^ * + - ∞ once)
          [()
           #'Null]
          [rest:not-stx-pair
           #'rest]
          [(#:rest rest)
           #'rest]
          [(s:with-superscripts . rest)
           (xl #'(s.expanded … . rest))]
          [(:base {~or * {~^ *}})
           #'(Listof base)]
          [(:base {~or * {~^ *}} . rest)
           #:with R (gensym 'R)
           #`(Rec R (U (Pairof base R)
                       #,(xl #'rest)))]
          [(:base {~or + {~^ +}} . rest)
           (xl #'(base ^ 1 + . rest))]
          [(:base {~^ power:nat +} . rest)
           (xl #'(base ^ {power} base * . rest))]
          [(:base {~optional ^} {-} . rest) ;; If it is ^ {-}, the next thing may be a number but should be used as a pattern, not a repeat
           (xl #'(base ^ * . rest))]
          [(:base ^ - . rest) ;; not with {}, check if there's stuff after
           (xl #'(base ^ 0 - . rest))]
          [(:base {~^ from:nat - ∞} . rest)
           (xl #'(base ^ from + . rest))]
          [(:base {~^ 0 - to:nat} . rest)
           #`(U . #,(foldl (λ (iteration u*)
                             (syntax-case u* ()
                               [[(_ . base…rest) . _]
                                #`[(List* base . base…rest) . #,u*]]))
                           #`[(List* #,(xl #'rest))]
                           (range (syntax-e #'to))))]
          [(:base {~^ from:nat - to:nat} . rest)
           #:with difference (- (syntax-e #'to) (syntax-e #'from))
           (when (< (syntax-e #'difference) 0)
             (raise-syntax-error 'xlist
                                 "invalid range: m is larger than n"
                                 #'-))
           (xl #'(base ^ {from} base ^ 0 - difference . rest))]
          [(:base {~^ from:nat -} . rest)
           ;; "-" is not followed by a number, nor by ∞, so default to ∞.
           (xl #'(base ^ from - ∞ . rest))]
          [(:base {~^ power:nat} . rest)
           #:with (expanded …) (map (const #'base)
                                    (range (syntax-e #'power)))
           #`(List* expanded … #,(xl #'rest))]
          [(:base {~optional {~^ once}} . rest)
           #`(Pairof base #,(xl #'rest))]))
      (xl stx))

    ;; normalize the xlist type
    ;; The normalized form has one type followed by ^ followed by a repeat
    ;; within braces (possibly {1}) for each position in the original type. It
    ;; always finishes with #:rest rest-type
    
    (define (normalize-xlist-type stx context)
      (define nt
        (syntax-parser
          #:context context
          #:literals (^ * + - ∞ once)
          [()
           #'(#:rest Null)]
          [rest:not-stx-pair
           #'(#:rest rest)]
          [(#:rest rest)
           #'(#:rest rest)]
          [(s:with-superscripts . rest)
           (nt #'(s.expanded … . rest))]
          [(:base {~or * {~^ *}} . rest)
           #`(base ^ {*} . #,(nt #'rest))]
          [(:base {~or + {~^ +}} . rest)
           #`(base ^ {1 +} . #,(nt #'rest))]
          [(:base {~^ 0 +} . rest)
           #`(base ^ {*} . #,(nt #'rest))]
          [(:base {~^ power:nat +} . rest)
           #`(base ^ {power +} . #,(nt #'rest))]
          [(:base {~optional ^} {-} . rest)
           #`(base ^ {*} . #,(nt #'rest))]
          [(:base ^ - . rest) ;; not with {}, check if there's stuff after
           (nt #'(base ^ 0 - . rest))]
          [(:base {~^ 0 - ∞} . rest)
           #`(base ^ {*} . #,(nt #'rest))]
          [(:base {~^ from:nat - ∞} . rest)
           (nt #'(base ^ from + . rest))]
          [(:base {~^ from:nat - to:nat} . rest)
           #`(base ^ {from - to} . #,(nt #'rest))]
          [(:base {~^ from:nat -} . rest)
           ;; "-" is not followed by a number, nor by ∞, so default to ∞.
           (nt #'(base ^ from - ∞ . rest))]
          [(:base {~^ power:nat} . rest)
           #`(base ^ {power} . #,(nt #'rest))]
          [(:base {~^ once} . rest)
           #`(base ^ {once} . #,(nt #'rest))]
          [(:base . rest)
             #`(base ^ {once} . #,(nt #'rest))]))
      (nt stx))



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
          #:literals (^ * + - ∞)
          [()
           #'(list)]
          [rest:not-stx-pair
           #'rest]
          [(#:rest rest)
           #'rest]
          [(({~literal unquote-splicing} splice) …+ . rest)
           #`(append splice … #,(xl #'rest))]
          [(s:with-superscripts . rest)
           (xl #'(s.expanded … . rest))]
          [(:base {~or * {~^ *}} . rest)
           #:with R (gensym 'R)
           #`(list-rest-ish [] base ooo #,(xl #'rest))]
          [(:base {~or + {~^ +}} . rest)
           (xl #'(base ^ 1 + . rest))]
          [(:base {~^ power:nat +} . rest)
           #:with ..power (format-id #'power "..~a" (syntax-e #'power))
           #`(list-rest-ish [] base ..power #,(xl #'rest))]
          [(:base {~optional ^} {-} . rest) ;; If it is ^ {-}, the next thing may be a number but should be used as a pattern, not a repeat
           (xl #'(base ^ {*} . rest))]
          [(:base ^ - . rest) ;; not with {}, check if there's stuff after
           (xl #'(base ^ 0 - . rest))]
          [(:base {~^ from:nat - ∞} . rest)
           (xl #'(base ^ {from +} . rest))]
          [(:base {~^ from:nat - to:nat} . rest)
           #:with occurrences (gensym 'occurrences)
           (when (> (syntax-e #'from) (syntax-e #'to))
             (raise-syntax-error 'xlist
                                 "invalid range: m is larger than n"
                                 #'-))
           #`(list-rest-ish
              [(? (λ (_) ((between/c from to) (length occurrences))))]
              (and occurrences base) ooo
              #,(xl #'rest))]
          [(:base {~^ from:nat -} . rest)
           ;; "-" is not followed by a number, nor by ∞, so default to ∞.
           (xl #'(base ^ {from - ∞} . rest))]
          ;; aliases
          [(:base {~or {~literal ...} {~literal ___}
                       {~^ {~literal ...}} {~^ {~literal ___}}}
                  . rest)
           #`(list-rest-ish [] base ooo #,(xl #'rest))]
          [(:base {~or {~literal ...+} {~^ {~literal ...+}}} . rest)
           #`(list-rest-ish base ..1 #,(xl #'rest))]
          [(:base {~or ellipsis:id {~^ ellipsis:id}} . rest)
           #:when (regexp-match? #px"^\\.\\.[0-9]+$"
                                 (symbol->string (syntax-e #'ellipsis)))
           #`(list-rest-ish [] base ellipsis #,(xl #'rest))]
          [(:base {~^ once})
           #`(list-rest-ish [] base #|no ellipsis|# . #,(xl #'rest))]
          [(:base {~^ power:nat})
           #:with occurrences (gensym 'occurrences)
           #`(list-rest-ish [(? (λ (_) (= (length occurrences) power)))]
                            (and occurrences base) ooo
                            #,(xl #'rest))]
          [(:base . rest)
           #`(list-rest-ish [] base #|no ellipsis|# #,(xl #'rest))]))
      (xl stx))

    #;("This is completely wrong"
       ;; Expands 0 or more mandatory-doms for ->*
       (define-splicing-syntax-class fixed-repeated-type
         #:attributes ([mandatory 1])
         #:literals (^ * + - ∞)
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
         #:literals (^ * + - ∞)
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
         #:literals (^ * + - ∞)
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
         #:literals (^ * + - ∞)
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
           #:literals (^ * + - ∞)
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
    (λ (stx)
      ((λ (x) #;(pretty-write (syntax->datum x)) x)
       ((syntax-parser
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
                  c₁ …)])
        stx))))

  (when-typed
   (provide xList #;xListBuilder)
   (define-type-expander (xList stx)
     ((xlist-type stx) (stx-cdr stx)))

   #;(define-type-expander (xListBuilder stx)
       ((xlist-builder-type stx) (stx-cdr stx)))))
