#lang scribble/manual
@require[phc-toolkit/scribblings/utils
         @for-label[xlist/untyped]]
@(def-orig typed [xlist]
   xlist)

@title{Untyped versions of xlist}
@defmodule[xlist/untyped
           #:use-sources
           [(submod (lib "xlist/main.rkt") untyped)]]

@defidform[xlist]{Untyped version of @|typed:xlist|.}
