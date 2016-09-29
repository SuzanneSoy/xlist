#lang scribble/manual
@require[phc-toolkit/scribblings/utils
         scribble/examples
         @for-label[xlist
                    typed/racket/base]]

@title{Splitting an xlist in its constituent sublists}
@(declare-exporting xlist)

@(define make-eval (make-eval-factory '(xlist type-expander)
                                      #:lang 'typed/racket))

@defform*[#:kind "match-expander"
          #:literals (^ * + - ∞)
          [(split-xlist pat τᵢ ...)
           (split-xlist pat τᵢ ... . rest)
           (split-xlist pat τᵢ ... #:rest rest)]
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
          [(nat (syntax/c exact-nonnegative-integer?))]]{
                                                           
 This match patterns splits an xlist into a list of lists, and matches the
 result against @racket[pat]. Each repeated element of the xlist is extracted
 into one of these sublists. The type for each sublist is determined base on
 the element's type and its @racket[_repeat]:
 @itemlist[
 @item{If the @racket[_repeat] for that element is @racket[once], then the
   element is inserted directly, without nesting it within a sublist. In
   contrast, it the @racket[_repeat] were @racket[1], the element would be
   inserted in a sublist of length one.}
 @item{If the @racket[_repeat] for that element is @racket[*] or an
   equivalent, the type of the sublist will be @racket[(Listof type)]}
 @item{If the @racket[_repeat] for that element is @racket[_n +] or an
   equivalent, the type of the sublist will be @racket[(xList type ^ _n +)]}
 @item{If the @racket[_repeat] for that element is @racket[_n] or an
   equivalent, the type of the sublist will be @racket[(xList type ^ _n)]}
 @item{If the @racket[_repeat] for that element is @racket[_from - _to] or an
   equivalent, the type of the sublist will be
   @racket[(xList type ^ _from - _to)]}
 @item{The @racket[#:rest] or dotted rest is included as the last element of
   the list matched against @racket[pat]. If the first form without a rest type
   is used, the list matched against @racket[pat] still contains @racket['()] as
   a last element:
   @examples[#:eval (make-eval)
             (match '(1 2 3)
               [(split-xlist (list (list a) (list b c) (? null?))
                             Number¹ Number⃰)
                (vector c b a)])]}]

 Note that @racket[split-xlist] assumes the value it is matched against has
 the type @racket[(xlist τᵢ ... maybe-rest)], but does not apply
 @racket[(? (make-predicate (xlist τᵢ ... maybe-rest)))] to the value itself.
 The rationale is that the @racket[make-predicate] may fail at compile-time if
 it cannot generate a contract for the given type. In some cases, however
 @racket[split-xlist] will still manage to successfully generate the match
 pattern, and can be used on its own, provided that the value is statically
 known to be of the right type.

 It is therefore recommended to use @racket[split-xlist] as follows when the
 type of the value is not known to be acceptable by @racket[split-xlist]:

 @examples[#:eval (make-eval)
           (define v : Any '(1 2 3))
           (match '(1 2 3)
             [(and (? (make-predicate (xlist Number¹ Number⃰)))
                   (split-xlist (list (list a) (list b c) (? null?))
                                Number¹ Number⃰))
              'success])]}
