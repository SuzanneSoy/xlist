#lang scribble/manual
@require[scribble-enhanced/with-manual
         xlist/scribble-enhanced
         scribble-math
         racket/require
         @for-label[xlist
                    (subtract-in typed/racket/base match-string)
                    (only-in syntax/parse ...+)
                    match-string]]

@title[#:style (with-html5 manual-doc-style)]{xlist}
@author[@author+email["Georges Dupéron" "georges.duperon@gmail.com"]]

@(define ddd (racket ...))

@defmodule[xlist]

Fancy lists, with bounded or unbounded repetition of elements. Can be used as a
type or match pattern.

To use the type expander, you must first require the
@racketmodname[type-expander] library.

@deftogether[
 [@defform*[#:kind "type-expander"
            [(xList τᵢ ...)
             (xList τᵢ ... . rest)
             (xList τᵢ ... #:rest rest)]]
  @defform*[#:kind "type-expander"
            #:literals (^ * + - ∞ once)
            [(xlist τᵢ ...)
             (xlist τᵢ ... . rest)
             (xlist τᵢ ... #:rest rest)]
            #:grammar
            [(τᵢ type
                 repeated-type)
             (repeated-type (code:line type ^ repeat)
                            (code:line type ^ {repeat})
                            (code:line type {repeat})
                            (code:line type superscripted-repeat)
                            (code:line type *)
                            (code:line type +)
                            (code:line superscripted-id))
             (repeat (code:line once)
                     (code:line nat)
                     (code:line nat +)
                     (code:line +)
                     (code:line nat - nat)
                     (code:line nat - ∞)
                     (code:line nat -)
                     (code:line - nat)
                     (code:line -)
                     (code:line - ∞)
                     (code:line *))]
            #:contracts
            [(nat (syntax/c exact-nonnegative-integer?))]]]]{
 The notation @racket[type ^ _n], where @racket[_n] is a number, indicates that
 the given type should be repeated @racket[_n] times within the list. Therefore,
 the following two types are equivalent:
 
 @racketblock[
 (xList Number ^ 3 Symbol String ^ 2)
 
 (List Number Number Number Symbol String String)]

 The notation @racket[type *] indicates that the given type may be repeated zero
 or more times. Therefore, the following two types are equivalent:

 @racketblock[
 (xList Number * Symbol String *)
 
 (Rec R1 (U (Pairof Number R1)
            (List* Symbol (Rec R2 (U (Pairof String R2)
                                     Null)))))]

 The notation @racket[type ^ _n +] indicates that the given type may be repeated
 @racket[_n] or more times. Therefore, the following two types are equivalent:
 
 @racketblock[
 (xList Number ^ {2 +} String)
 
 (List* Number Number (Rec R1 (U (Pairof Number R1)
                                 (List String))))]

 When the number preceding @racket[+] is omitted, it defaults to @racket[1].

 The notation @racket[type ^ once] yields the same type as @racket[type ^ 1],
 but other forms recognise @racket[once] and treat it specially. For example,
 @racket[xlist-split] splits the corresponding element as a standalone value,
 not as a list of length one.

 The notation @racket[type ^ _n - _m] indicates that the given type may be
 repeated between @racket[_n] (inclusive) and @racket[_m] (inclusive) times.
 Therefore, the following two types are equivalent:
 
 @racketblock[
 (xList Number ^ {2 - 5} String)
 
 (U (List Number Number String)
    (List Number Number Number String)
    (List Number Number Number Number String)
    (List Number Number Number Number Number String))]

 Be aware that the tail of the @racket[xList] following the use of
 @racket[type ^ _n - _m] is repeated @${n - m} times, so if the tail itself
 contains uses of @racket[-], the resulting macro-expanded type will be huge,
 and may easily make Typed/Racket run out of memory, or slow down the type
 checking.

 If the first bound is omitted, it defaults to @racket[0], and if the second
 bound is omitted, it defaults to @racket[∞]. This means that @racket[-] on its
 own is equivalent to @racket[*], but the latter form is preferred.
 
 The @racket[superscripted-repeat] is a representation of @racket[repeat] using
 superscripted unicode characters, without spaces (i.e. the
 @racket[superscripted-repeat] is a single identifier):

 @itemlist[
 @item{Digits are replaced by their unicode superscripted counterparts
   @elem[#:style 'tt "⁰¹²³⁴⁵⁶⁷⁸⁹"]}
 @item{@racket[+] and @racket[-] are replaced by their unicode superscripted
   counterparts, respectively @elem[#:style 'tt "⁺"] and @elem[#:style 'tt "⁻"]}
 @item{@racket[*] is replaced by the unicode character ``COMBINING ASTERISK
   ABOVE'' @tt{ ⃰} (code point U+20F0)}
 @item{@racket[∞] is always omitted, as @racket[_n - ∞] and @racket[- ∞] are
   equivalent to @racket[_n -] and @racket[0 -]}]
                                    
 A @racket[superscripted-id] is a type identifier ending with a sequence of
 characters which would otherwise be valid for @racket[superscripted-repeat]. In
 other words, if the @racket[type] is an identifier, the type and the
 @racket[superscripted-repeat] can be coalesced into a single identifier.
   
 The identifier @racket[String³] is equivalent to the notations
 @racket[String ³] (with a space between the identifier and the @racket[³]) and
 @racket[String ^ 3].

 Similarly, the identifier @racket[String⃰] is equivalent to the notations
 @racket[String  ⃰] (with a space between the identifier and the @racket[ ⃰]),
 @racket[String ^ *] (using a regular asterisk, i.e. the multiplication function
 in Racket) and @racket[String *] (using a regular asterisk, i.e. the
 multiplication function in Racket).

 The same logic applies to the other cases.}

@defform*[#:kind "match-expander"
          #:link-target? #f
          #:literals (^ * + - ...+ ∞)
          [(xlist patᵢ ...)
           (xlist patᵢ ... . rest)
           (xlist patᵢ ... #:rest rest)]
          #:grammar
          [(patᵢ pattern-or-spliced
                 repeated-pattern
                 spliced-pattern)
           (pattern-or-spliced pattern
                               spliced-pattern)
           (spliced-pattern ,@pattern)
           (repeated-pattern (code:line pattern-or-spliced ^ repeat)
                             (code:line pattern-or-spliced ^ {repeat})
                             (code:line pattern-or-spliced superscripted-repeat)
                             (code:line pattern-or-spliced *)
                             (code:line pattern-or-spliced +)
                             (code:line pattern-or-spliced ooo)
                             (code:line superscripted-id))
           (repeat (code:line once)
                   (code:line nat)
                   (code:line nat +)
                   (code:line +)
                   (code:line nat - nat)
                   (code:line nat - ∞)
                   (code:line nat -)
                   (code:line - nat)
                   (code:line - ∞)
                   (code:line -)
                   (code:line *)
                   (code:line ooo))
           (ooo #,ddd
                ..k
                ____
                ___k
                ...+)]
          #:contracts
          [(nat (syntax/c exact-nonnegative-integer?))]]{
                                    
 This match expander works like the @racket[xList] type expander, but instead
 controls the repetition of match patterns. The repeated patterns are not
 literally copied, as this would likely cause errors related to duplicate
 attributes. Instead, the @racket[repeat] forms control the number of times a
 pattern may be bound, like @racket[...] does.

 If the @racket[_repeat] is @racket[once], or if the pattern does not have a
 @racket[_repeat], then the pattern is not put under ellipses, so that
 @racket[(match '(42) [(xlist a ^ once) a])] returns @racket[42], whereas
 @racket[(match '(42) [(xlist a ^ 1) a])] returns @racket['(42)].

 For convenience and compatibility with existing match patterns, the following
 equivalences are provided:
 @itemlist[
 @item{@racket[...] is equivalent to @racket[*]}
 @item{@racket[_..k] is equivalent to @racket[_k +]}
 @item{@racket[____] is equivalent to @racket[*]}
 @item{@racket[___k] is equivalent to @racket[_k +]}
 @item{@racket[...+] is equivalent to @racket[+]}]

 Additionally, when @RACKET[#,@pattern] appears as one of the @racket[xlist]
 elements, the given @racket[pattern] may match any number of elements in the
 list. This is implemented in terms of @racket[append] from the
 @racketmodname[match-string] library.

 The following two match patterns are therefore equivalent:

 @racketblock[
 (xlist number?³⁻⁵ ,@(list-no-order number? string?) symbol?⁺)
 
 (append (and (list number? ...) (app length (? (between/c 3 5))))
         (list-no-order number? string?)
         (list symbol? ..1))]

 Applying a repeat indicator on a splice is not supported yet, i.e.
 @racket[(xlist ,@(list-no-order number? string?)⁵)] will not work.

 @emph{Note :} Typed/Racket's type inference is not strong enough (yet) to
 support some match patterns, and there is no @elem[#:style 'tt "typed/match"]
 library which would help with that (yet). This means that although by
 construction @racket[xlist] tries to avoid generating such patterns, a few of
 the patterns supported by @racket[xlist] will not work in
 @racketmodname[typed/racket] (rest values and spliced lists are the most
 likely to cause problems). As an alternative, try the @racket[split-xlist]
 pattern, which produces code which should propagate type information to the
 different sub-lists.}

@;{This is completely wrong.
 @defform*[#:link-target? #f
 #:literals (^ *)
 [(xlist τᵢ ... maybe-τⱼ τₖ ... maybe-τₙ)
  (xlist τᵢ ... τₘᵥ)]
 #:grammar
 [(τᵢ type
      fixed-repeated-type)
  (τₘᵥ mandatory-variadic-repeated-type)
  (maybe-τⱼ (code:line)
            mandatory-bounded-variadic-repeated-type)
  (τₖ optional-bounded-variadic-repeated-type)
  (maybe-τₙ (code:line)
            optional-variadic-repeated-type)
  (fixed-repeated-type
   (code:line type ^ fixed-repeat)
   (code:line type ^ {fixed-repeat})
   (code:line type {fixed-repeat})
   (code:line type superscripted-fixed-repeat)
   (code:line superscripted-fixed-id))
  (mandatory-bounded-variadic-repeated-type
   (code:line type ^ mandatory-bounded-variadic-repeat)
   (code:line type ^ {mandatory-bounded-variadic-repeat})
   (code:line type {mandatory-bounded-variadic-repeat})
   (code:line type superscripted-mandatory-bounded-variadic-repeat)
   (code:line superscripted-mandatory-bounded-variadic-id))
  (optional-bounded-variadic-repeated-type
   (code:line type ^ optional-bounded-variadic-repeat)
   (code:line type ^ {optional-bounded-variadic-repeat})
   (code:line type {optional-bounded-variadic-repeat})
   (code:line type superscripted-optional-bounded-variadic-repeat)
   (code:line superscripted-optional-bounded-variadic-id))
  (mandatory-variadic-repeated-type
   (code:line type ^ mandatory-variadic-repeat)
   (code:line type ^ {mandatory-variadic-repeat})
   (code:line type {mandatory-variadic-repeat})
   (code:line type superscripted-mandatory-variadic-repeat)
   (code:line superscripted-mandatory-variadic-id)
   (code:line type +))
  (optional-variadic-repeated-type
   (code:line type ^ optional-variadic-repeat)
   (code:line type ^ {optional-variadic-repeat})
   (code:line type {optional-variadic-repeat})
   (code:line type superscripted-optional-variadic-repeat)
   (code:line superscripted-optional-variadic-id)
   (code:line type *))
  (fixed-repeat (code:line nat)
                (code:line from - to (code:comment "from = to")))
  (mandatory-bounded-variadic-repeat (code:line nat - nat))
  (optional-bounded-variadic-repeat (code:line 0 - nat)
                                    (code:line - nat))
  (mandatory-variadic-repeat (code:line nat +)
                             (code:line +)
                             (code:line nat -)
                             (code:line nat - ∞))
  (optional-variadic-repeat (code:line 0 - ∞)
                            (code:line 0 -)
                            (code:line - ∞)
                            (code:line -)
                            (code:line *))]]{
  Macro form which returns a builder function for a list with the given type.
  The simplified syntax compared to @racket[xList] is due to the fact that there
  are some function types that Typed/Racket cannot express (yet).}
}

@defproc[(normalize-xlist-type [stx syntax?] [context syntax?]) syntax?]{
 Normalizes the xlist type. The normalized form has one type followed by ^
 followed by a repeat within braces (a @racket[type] without a repeat is
 transformed into @racket[type ^ {once}]) for each position in the original
 type. It always finishes with #:rest rest-type. This function also performs a
 few simplifications on the type, like transforming @racket[^ {3 -}] into
 @racket[^ {3 +}], and transforming @racket[^ {0 -}] into @racket[^ {*}].}

@include-section{split-xlist.scrbl}
@include-section{xlist-untyped.scrbl}
@include-section{identifiers.scrbl}