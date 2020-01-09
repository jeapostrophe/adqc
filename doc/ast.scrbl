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

@defproc[c-identifier-string? ([v any/c]) boolean?]{
 Predicate returns @racket[#t] if @racket[v] is a valid identifier string in C.
 This is a partial test.
}

@defproc[c-library-string? ([v any/c]) boolean?]{
 Predicate returns @racket[#t] if @racket[v] is a valid name for a C library,
 meaning that it would be valid to the right of @code["-l"] in @code["cc"].
 Currently this is equivalent to @racket[string?].
}

@defproc[c-header-string? ([v any/c]) boolean?]{
 Predicate returns @racket[#t] if @racket[v] is a valid name
 for C header file, meaning that it would be valid inside the
 @code["< >"]s of an @code["#include"]. Currently this is equivalent
 to @racket[string?].
}

@defproc[cify ([s symbol?]) c-identifier-string?]{
 Sanitizes a variable name by removing from it any characters which are
 not valid in a C identifier, and appending it with a unique number. These
 numbers are monotonically increasing, although this counter can be reset
 using @racket[with-cify-counter]. Note that although the argument @racket[s]
 is a @racket[symbol?], the return value is a @racket[string?].
}

@defform[(with-cify-counter body ...+)]{
 Calls to @racket[cify] within @racket[body] will use a new counter when
 generating unique variable names, starting at @racket[0].
}

@defstruct*[ExternSrc ([ls (listof c-library-string?)]
                       [hs (listof c-header-string?)])]{
 Represents an external code source, with @racket[ls] being a list of
 libraries to link against, and @racket[hs] being a list of header files
 to include.
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