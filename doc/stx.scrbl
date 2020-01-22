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

@; XXX let, E-expander, E-free-syntax
@defform[(E e)
         #:grammar
         ([e (bin-op e e)
             (e : ty)
             (if e e e)
             (unsyntax v)
             n
             p
             x])
         #:contracts ([ty Type?]
                      [v Expr?]
                      [n number?]
                      [p Path?]
                      [x symbol?])]{
 Produces an @racket[Expr] from @racket[e].
}