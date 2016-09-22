#lang scribble/manual
@require[phc-toolkit/scribblings/utils
         @for-label[xlist/untyped]]
@(def-orig typed [xlist]
   xlist)

@title{Special identifiers recognised by @racket[xlist]}
@defmodule*[(xlist
             xlist/untyped)
            #:link-target? #f
            #:use-sources
            [(lib "xlist/infinity-identifier.rkt")
             (lib "xlist/caret-identifier.rkt")]]

@defidform[^]{This identifier can only be used within xlist forms.}

@defidform[∞]{
 This identifier is meant to be used within xlist forms, but is also equal to
 @racket[+inf.0] as a convenience. In the future, this package will make it
 possible for other packages to overload the meaning of the @racket[^] and
 @racket[∞] identifiers, so that the value of @racket[∞] may depend on the
 packages loaded (for example a symbolic math package may want to attach a
 special value to @racket[∞].}