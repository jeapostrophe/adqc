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

AST nodes used in construction of @racketmodname[adqc] programs.

@; XXX Unpackers?

@defproc[c-identifier-string? ([v any/c]) boolean?]{
 Predicate returns @racket[#t] if @racket[_v] is a valid identifier string in C.
 This is a partial test.
}

@defproc[c-library-string? ([v any/c]) boolean?]{
 Predicate returns @racket[#t] if @racket[_v] is a valid name for a C library,
 meaning that it would be valid to the right of @code["-l"] in @code["cc"].
 Currently this is equivalent to @racket[string?].
}

@defproc[c-header-string? ([v any/c]) boolean?]{
 Predicate returns @racket[#t] if @racket[_v] is a valid name
 for C header file, meaning that it would be valid inside the
 @code["< >"]s of an @code["#include"]. Currently this is equivalent
 to @racket[string?].
}

@defproc[cify ([s symbol?]) c-identifier-string?]{
 Sanitizes a variable name by removing from it any characters which are
 not valid in a C identifier, and appending it with a unique number. These
 numbers are monotonically increasing, although this counter can be reset
 using @racket[with-cify-counter]. Note that although the argument @racket[_s]
 is a @racket[symbol?], the return value is a @racket[string?].
}

@defform[(with-cify-counter body ...+)]{
 Calls to @racket[cify] within @racket[_body] will use a new counter when
 generating unique variable names, starting at @racket[0].
}

@defstruct*[ExternSrc ([ls (listof c-library-string?)]
                       [hs (listof c-header-string?)])]{
 Represents an external code source, with @racket[_ls] being a list of
 libraries to link against, and @racket[_hs] being a list of header files
 to include.
}

@defstruct*[Type ()]{
 Basic type that all @racketmodname[adqc] types are derived from.         
}

@defstruct*[(IntT Type) ([signed? boolean?] [bits (or/c 8 16 32 64)])]{
 A @racket[Type] representing a signed or unsigned integer value.
}

@defstruct*[(FloT Type) ([bits (or/c 32 64)])]{
 A @racket[Type] represending a floating-point value.                         
}

@defstruct*[(ArrT Type) ([dim exact-nonnegative-integer?]
                         [ety non-void-type?])]{
 A @racket[Type] representing an array of length @racket[dim] containing values
 of type @racket[_ety]. Arrays in @racketmodname[adqc] are reference types.
}

@defstruct*[(RecT Type) ([field->ty (hash/c symbol? non-void-type?)]
                         [field->c (hash/c symbol? c-identifier-string?)]
                         [c-order (listof symbol?)])]{
 A @racket[Type] representing a record. Records in @racketmodname[adqc] are
 reference types, and are compiled into native C structs. The ordering of
 fields in the C struct can be controlled with @racket[_c-order].
}

@defstruct*[(UniT Type) ([mode->ty (hash/c symbol? non-void-type?)]
                         [mode->c (hash/c symbol? c-identifier-string?)])]{
 A @racket[Type] representing a union. Unions in @racketmodname[adqc] are
 reference types, and are compiled into native C unions.
}

@defstruct*[(ExtT Type) ([src ExternSrc?] [name string?])]{
 A @racket[Type] representing an opaque type. The string provided for
 @racket[_name] is emitted literally into the resulting C program.
 Programs that use @racket[ExtT] are considered unsafe.
}

@defstruct*[(VoiT Type) ()]{
 A @racket[Type] that corresponds to C's @code["void"] type. Useful for
 declaring functions that return @code["void"], and not much else.
}

@defstruct*[(AnyT Type) ()]{
 A value of type @racket[AnyT] is considered valid for the purpose of type
 checking when compared to any other @racket[Type]. This is used to implement
 ANF support for language features that manipulate program control flow, such
 as @code["error"] and @code["let/ec"]. Most @racketmodname[adqc] programs will
 not need to explicitly declare variables of this type.
}

@defproc[(non-void-type? [v any/c]) boolean?]{
 Equivalent to @racket[(and/c Type? (not/c VoiT?))].
}

@defstruct*[Path ()]{
 All @racketmodname[adqc] paths are derived from this type. A @racket[Path]
 is a reference some previously declared value in memory.
}

@defstruct*[(MetaP Path) ([m any/c] [p Path?])]{
 A "Meta Path" containing some metadata @racket[_m], which describes a
 @racket[Path] @racket[_p]. Multiple layers of nesting are allowed, so
 @racket[_p] may itself a @racket[MetaP?].
}

@defstruct*[(Var Path) ([x symbol?] [ty Type?])]{
 A reference to a local variable with name @racket[_x] and type @racket[_ty].
}

@defstruct*[(Global Path) ([ty non-void-type?] [xi Init?])]{
 A globablly-accessable variable, analagous to a static global variable in C.
 Two references to a @racket[Global] are considered to reference the same
 variable in the resulting program when when they compare equal using
 @racket[eq?]. I.e., two @racket[Global]s can have the same @racket[_ty]
 (be the same type) and @racket[_xi] (be initialized to the same value) without
 necessarily referencing the same global variable, so @racket[equal?] should
 not be used to infer this.
}

@defstruct*[(Select Path) ([p Path?] [ie Expr?])]{
 A @racket[Path] to a value stored in an array. The value stored at @racket[_p]
 must be of type @racket[ArrT?], and the value produced by @racket[_ie] must be
 of type @racket[IntT?].
}

@defstruct*[(Field Path) ([p Path?] [f symbol?])]{
 A @racket[Path] to a value stored in a field of a record. The value stored at
 @racket[_p] must be of type @racket[RecT?], and @racket[_f] must be a field in
 that record.
}

@defstruct*[(Mode Path) ([p Path?] [m symbol?])]{
 A @racket[Path] to a union, the value of which will be interpreted according
 to mode @racket[_m]. The value stored at @racket[_p] must be of type
 @racket[UniT?], and @racket[_m] must be a mode of that union.
}

@defstruct*[(ExtVar Path) ([src ExternSrc?]
                           [name c-identifier-string?]
                           [ty non-void-type?])]{
 A @racket[Path] to an externally-declared variable, e.g., @code["errno"].
 The string provided for @racket[_name] is emitted literally into the resulting
 C program. Programs that use @racket[ExtVar] are considered unsafe.
}

@defstruct*[Expr ()]{

}

@defstruct*[(MetaE Expr) ([m any/c] [e Expr?])]{

}

@defstruct*[(Int Expr) ([signed? boolean?]
                        [bits (or/c 8 16 32 64)]
                        [val exact-integer?])]{

}

@defstruct*[(Flo Expr) ([bits (or/c 32 64)]
                        [val (or/c single-flonum? double-flonum?)])]{

}

@defstruct*[(Cast Expr) ([ty Type?] [e Expr?])]{

}

@defstruct*[(Read Expr) ([p Path?])]{

}

@defstruct*[(BinOp Expr) ([op symbol?] [L Expr?] [R Expr?])]{

}

@defstruct*[(LetE Expr) ([x symbol?] [ty Type?] [xe Expr?] [be Expr?])]{

}

@defstruct*[(IfE Expr) ([ce Expr?] [te Expr?] [fe Expr?])]{

}

@defstruct*[Init ()]{

}

@defstruct*[(UndI Init) ([ty Type?])]{

}

@defstruct*[(ConI Init) ([e Expr?])]{

}

@defstruct*[(ZedI Init) ([ty Type?])]{

}

@defstruct*[(ArrI Init) ([is (listof Init?)])]{

}

@defstruct*[(RecI Init) ([field->i (hash/c symbol? Init?)])]{

}

@defstruct*[(UniI Init) ([mode symbol?] [i Init?])]{

}
