#lang scribble/manual
@(require scribble/eval
          (for-label adqc
                     racket/base
                     racket/contract))

@title[#:tag "ast"]{AST}

@(define the-eval (make-base-eval))
@(the-eval '(require adqc))

@defmodule[adqc/ast]

@author[@author+email["Conor Finegan" "theconor4@gmail.com"]]

AST nodes used in construction of ADQC programs.

@; with-cify-couner
@; c-identifier-string?
@; c-library-string?
@; c-header-string?
@; cify

@defstruct*[ExternSrc ([ls (listof c-library-string?)]
                       [hs (listof c-header-string?)])]{

}

@defstruct*[Type ()]{
 Basic type that all ADQC types are derived from.         
}

@defstruct*[(IntT Type) ([signed? boolean?] [bits (or/c 8 16 32 64)])]{
 A @racket[Type] representing a signed or unsigned integer value.
}

@defstruct*[(FloT Type) ([bits (or/c 8 16 32 64)])]{
 A @racket[Type] represending a floating-point value.                         
}

@defstruct*[(ArrT Type) ([dim exact-nonnegative-integer?]
                         [ety non-void-type?])]{
 A @racket[Type] representing an array of length @racket[dim] containing
 values of type @racket[ety]. Arrays in ADQC are reference types.
}

@defstruct*[(RecT Type) ([field->ty (hash/c symbol? non-void-type?)]
                         [field->c (hash/c symbol? c-identifier-string?)]
                         [c-order (listof symbol?)])]{
 A @racket[Type] representing a record. Records in ADQC are reference types,
 and are compiled into native C structs. The ordering of fields in the C struct
 can be controlled with @racket[c-order].
}

@defstruct*[(UniT Type) ([mode->ty (hash/c symbol? non-void-type?)]
                         [mode->c (hash/c symbol? c-identifier-string?)])]{
 A @racket[Type] representing a union. Unions in ADQC are reference types, and
 are compiled into native C unions.
}

@defstruct*[(ExtT Type) ([src ExternSrc?] [name string?])]{
 A @racket[Type] representing an opaque type. The string provided for
 @racket[name] is emitted literally into the resulting C program.
}

@defstruct*[(VoiT Type) ()]{
 A @racket[Type] that corresponds to C's @code["void"] type. Useful for
 declaring functions that return @code["void"], and not much else.
}

@defstruct*[(AnyT Type) ()]{
 A value of type @racket[AnyT] is considered valid for the purpose of type
 checking when compared to any other @racket[Type]. This is used to implement
 ANF support for language features that manipulate program control flow, such
 as @code["error"] and @code["let/ec"]. Most ADQC programs will not need to
 explicitly declare variables of this type.
}

@defproc[(non-void-type? [v any/c]) boolean?]{
 Equivalent to @racket[(and/c Type? (not/c VoiT?))].
}