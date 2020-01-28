#lang scribble/manual
@(require scribble/eval
          (for-label adqc
                     racket/base
                     racket/contract))

@title[#:tag "stx"]{Syntax}

@(define the-eval (make-base-eval))
@(the-eval '(require adqc))

@defmodule[adqc/stx]

@author[@author+email["Conor Finegan" "theconor4@gmail.com"]]

Syntax and standard library functions for constructing @racketmodname[adqc]
programs.

@; XXX Put define-type, define-fun, define-global, define-prog, etc. together,
@; or in their own sections?

@section{Type Syntax}

@defform[(T ty)
         #:grammar
         ([ty (array dim ty)
              (record f ty ... ...)
              (union m I)])
         #:contracts ([dim exact-positive-integer?]
                      [m symbol?])]{
 Produces a @racket[Type] from @racket[ty].
}

@section{Path Syntax}

@section{Expression Syntax}

@; XXX let, E-expander, E-free-syntax, unsyntax
@defform[(E e)
         #:grammar
         ([e (bin-op e e)
             (e : ty)
             (if e e e)
             n
             p
             x])
         #:contracts ([ty Type?]
                      [n number?]
                      [p Path?]
                      [x symbol?])]{
 Produces an @racket[Expr] from @racket[e].
}

@section{Initializer Syntax}

@section{Statement Syntax}

@section{Function Syntax}
