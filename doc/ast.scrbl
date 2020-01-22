#lang scribble/manual
@(require scribble/eval
          (for-label adqc
                     racket/base
                     racket/contract
                     racket/list))

@title[#:tag "ast"]{AST}

@(define the-eval (make-base-eval))
@(the-eval '(require adqc))

@defmodule[adqc/ast]

@author[@author+email["Conor Finegan" "theconor4@gmail.com"]]

AST nodes used in construction of @racketmodname[adqc] programs.

@defproc[(c-identifier-string? [v any/c]) boolean?]{
 Predicate returns @racket[#t] if @racket[v] is a valid identifier string in C.
 This is a partial test.
}

@defproc[(c-library-string? [v any/c]) boolean?]{
 Predicate returns @racket[#t] if @racket[v] is a valid name for a C library,
 meaning that it would be valid to the right of @tt{-l} in @tt{cc}.
 Currently this is equivalent to @racket[string?].
}

@defproc[(c-header-string? [v any/c]) boolean?]{
 Predicate returns @racket[#t] if @racket[v] is a valid name
 for C header file, meaning that it would be valid inside the
 @tt{< >}s of an @tt{#include}. Currently this is equivalent
 to @racket[string?].
}

@defproc[(cify [s symbol?]) c-identifier-string?]{
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
 of type @racket[ety]. Arrays in @racketmodname[adqc] are reference types.
}

@defstruct*[(RecT Type) ([field->ty (hash/c symbol? non-void-type?)]
                         [field->c (hash/c symbol? c-identifier-string?)]
                         [c-order (listof symbol?)])]{
 A @racket[Type] representing a record. Records in @racketmodname[adqc] are
 reference types, and are compiled into native C structs. The ordering of
 fields in the C struct can be controlled with @racket[c-order].
}

@defstruct*[(UniT Type) ([mode->ty (hash/c symbol? non-void-type?)]
                         [mode->c (hash/c symbol? c-identifier-string?)])]{
 A @racket[Type] representing a union. Unions in @racketmodname[adqc] are
 reference types, and are compiled into native C unions.
}

@defstruct*[(ExtT Type) ([src ExternSrc?] [name string?])]{
 A @racket[Type] representing an opaque type. The string provided for
 @racket[name] is emitted literally into the resulting C program.
 Programs that use @racket[ExtT] are considered unsafe.
}

@defstruct*[(VoiT Type) ()]{
 A @racket[Type] that corresponds to C's @tt{void} type. Useful for
 declaring functions that return @tt{void}, and not much else.
}

@defstruct*[(AnyT Type) ()]{
 A value of type @racket[AnyT] is considered valid for the purpose of type
 checking when compared to any other @racket[Type]. This is used to implement
 ANF support for language features that manipulate program control flow, such
 as @tt{error} and @tt{let/ec}. Most @racketmodname[adqc] programs will
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
 A "Meta Path" containing some metadata @racket[m], which describes a
 @racket[Path] @racket[p]. Multiple layers of nesting are allowed, so
 @racket[p] may itself be a @racket[MetaP].
}

@defstruct*[(Var Path) ([x symbol?] [ty Type?])]{
 A reference to a local variable with name @racket[x] and type @racket[ty].
}

@defstruct*[(Global Path) ([ty non-void-type?] [xi Init?])]{
 A globablly-accessable variable, analagous to a static global variable in C.
 Two references to a @racket[Global] are considered to reference the same
 variable in the resulting program when when they compare equal using
 @racket[eq?]. I.e., two @racket[Global]s can have the same @racket[ty]
 (be the same type) and @racket[xi] (be initialized to the same value) without
 necessarily referencing the same global variable, so @racket[equal?] should
 not be used to infer this.
}

@defstruct*[(Select Path) ([p Path?] [ie Expr?])]{
 A @racket[Path] to a value stored in an array. The value stored at @racket[p]
 must be of type @racket[ArrT?], and the value produced by @racket[ie] must be
 of type @racket[IntT?].
}

@defstruct*[(Field Path) ([p Path?] [f symbol?])]{
 A @racket[Path] to a value stored in a field of a record. The value stored at
 @racket[p] must be of type @racket[RecT?], and @racket[f] must be a field in
 that record.
}

@defstruct*[(Mode Path) ([p Path?] [m symbol?])]{
 A @racket[Path] to a union, the value of which will be interpreted according
 to mode @racket[m]. The value stored at @racket[p] must be of type
 @racket[UniT?], and @racket[m] must be a mode of that union.
}

@defstruct*[(ExtVar Path) ([src ExternSrc?]
                           [name c-identifier-string?]
                           [ty non-void-type?])]{
 A @racket[Path] to an externally-defined variable, e.g., @tt{errno}.
 The string provided for @racket[name] is emitted literally into the resulting
 C program. Programs that use @racket[ExtVar] are considered unsafe.
}

@defstruct*[Expr ()]{
 All @racketmodname[adqc] expressions are derived from @racket[Expr].
 Expressions produce a value and have no side effects, other than reading
 from memory.
}

@defstruct*[(MetaE Expr) ([m any/c] [e Expr?])]{
 A "Meta Expression" containing some metadata @racket[m], which describes an
 @racket[Expr] @racket[e]. Multiple layers of nesting are allowed, so
 @racket[e] may itself be a @racket[MetaE].
}

@defstruct*[(Int Expr) ([signed? boolean?]
                        [bits (or/c 8 16 32 64)]
                        [val exact-integer?])]{
 An integer value, signed or unsigned.
}

@defstruct*[(Flo Expr) ([bits (or/c 32 64)]
                        [val (or/c single-flonum? double-flonum?)])]{
 An IEEE 754 floating point value.
}

@defstruct*[(Cast Expr) ([ty Type?] [e Expr?])]{
 Casts @racket[e] to a new type. The types @racket[ty] and
 @racket[e] must be arithmetic (i.e., @racket[IntT?] or @racket[FloT?]).
}

@defstruct*[(Read Expr) ([p Path?])]{
 Produces the value stored at @racket[p].
}

@defstruct*[(BinOp Expr) ([op symbol?] [L Expr?] [R Expr?])]{
 A binary operation with left-hand side @racket[L] and right-hand side
 @racket[R]. XXX: List of valid ops.
}

@defstruct*[(LetE Expr) ([x symbol?] [ty Type?] [xe Expr?] [be Expr?])]{
 Like @racket[Let], but as an @racket[Expr]. The syntax produced by @racket[xe]
 is substituted literally for @racket[x] in @racket[be]. I.e.,
 @tt{(let ([x (+ 1 2)]) (* x x))} is equivalent to @tt{(* (+ 1 2) (+ 1 2))}.
}

@defstruct*[(IfE Expr) ([ce Expr?] [te Expr?] [fe Expr?])]{
 Like @racket[If], but as an @racket[Expr].
 Equivalent to the ternary operator in C.
}

@defstruct*[Init ()]{
 Produces a value that is suitable for initializing a newly-declared
 variable. All @racketmodname[adqc] initializers are derived from @racket[Init].
}

@defstruct*[(UndI Init) ([ty Type?])]{
 Perform no initialization. This is equialent to to declaration like
 @tt{int x;} in C.
}

@defstruct*[(ConI Init) ([e Expr?])]{
 Initialize with the value produced by @racket[e]. If @racket[e] contains
 any @racket[Read]s, then the resulting program is considered unsafe.
}

@defstruct*[(ZedI Init) ([ty Type?])]{
 Initialize with zero. This is equivalent to @tt["{ 0 }"] when initializing
 a value of type @racket[ArrT?], @racket[RecT?], or @racket[UniT?] (an array,
 record, or union).
}

@defstruct*[(ArrI Init) ([is (listof Init?)])]{
 Initialize an array. The value being initialized must be of type
 @racket[ArrT?], and length of @racket[is] must be equal to the length
 of the array being initialized.
}

@defstruct*[(RecI Init) ([field->i (hash/c symbol? Init?)])]{
 Initialize a record. The value being initialized must be of type
 @racket[RecT?]. XXX: Does field->i need entry for every field?
}

@defstruct*[(UniI Init) ([mode symbol?] [i Init?])]{
 Initialize a union. The value being initialized must be of type
 @racket[UniT?], and @racket[mode] must be a mode of that union.
}

@defstruct*[Stmt ()]{
 A statement does not produce a value, but instead has some side effect.
 All @racketmodname[adqc] statements are derived from @racket[Stmt].
}

@defstruct*[(MetaS Stmt) ([m any/c] [bs Stmt?])]{
 A "Meta Statement" containing some metadata @racket[m], which describes a
 @racket[Stmt] @racket[bs]. Multiple layers of nesting are allowed, so
 @racket[bs] may itself be a @racket[MetaS].
}

@defstruct*[(Skip Stmt) ([comment (or/c #f string?)])]{
 A null operation, or no-op. If @racket[comment] is not @racket[#f], then
 it is emitted into the resulting program as a comment.
}

@defstruct*[(Fail Stmt) ([msg string?])]{
 Signal that some error has occurred. Currently, this prints @racket[msg]
 to @tt{stderr}, then calls @tt{exit(1)}.
}

@defstruct*[(Begin Stmt) ([f Stmt?] [s Stmt?])]{
 Execute @racket[f], then @racket[s].
}

@defstruct*[(Assign Stmt) ([p Path?] [e Expr?])]{
 Assign a new value to a previously-declared variable.
}

@defstruct*[(If Stmt) ([p Expr?] [t Stmt] [f Stmt])]{
 If @racket[p] is truthy (non-zero), then execute @racket[t]. Otherwise,
 execute @racket[f]. The @racket[Expr] @racket[p] must produce a value
 of type @racket[IntT?]. Equivalent to an @tt{if} statement in C.
}

@defstruct*[(While Stmt) ([p Expr?] [body Stmt?])]{
 Executes @racket[body] as long as @racket[p] is truthy (non-zero). The
 @racket[Expr] @racket[p] must produce a value of type @racket[IntT?].
 Equivalent to a @tt{while} statement in C.
}

@defstruct*[(Jump Stmt) ([label symbol?])]{
 Jump to @racket[label]. Equivalent to @tt{goto} in C.
}

@defstruct*[(Let/ec Stmt) ([label symbol?] [body Stmt?])]{
 Execute @racket[body]. Within @racket[body], @racket[Jump]ing to
 @racket[label] wil skip the rest of @racket[body]. Like @racket[let/ec]
 in Racket, but without a return value.
}

@defstruct*[(Let Stmt) ([x symbol?] [ty Type?] [xi Init?] [bs Stmt?])]{
 Declares a new variable. Within the body statement @racket[bs], @racket[x]
 refers to a @racket[Var] of type @racket[ty], initialized with @racket[xi].
 Equivalent to a local variable declaration in C.
}

@defstruct*[(Call Stmt) ([x symbol?]
                         [ty Type?]
                         [f Fun?]
                         [as (listof (or/c Expr? Path?))]
                         [bs Stmt?])]{
 Invokes the function @racket[f] with arguments @racket[as], storing the
 result in a new @racket[Var] named @racket[x]. Works similarly to
 @racket[Let], creating a new variable that can be referenced within
 @racket[body].
}

@defstruct*[Fun ()]{
 A function, which can be invoked through @racket[Call].
}

@defstruct*[(MetaFun Fun) ([m any/c] [f Fun?])]{
 A "Meta Function" containing some metadata @racket[m], which describes an
 @racket[Fun] @racket[f]. Multiple layers of nesting are allowed, so
 @racket[f] may itself be a @racket[MetaFun].
}

@defstruct*[(IntFun Fun) ([args (listof Arg?)]
                          [ret-x symbol?]
                          [ret-ty Type?]
                          [ret-lab symbol?]
                          [body Stmt?])]{
 An "Internal Function". Functions written in @racketmodname[adqc] are compiled
 into these. Functions in @racketmodname[adqc] convey their return value by
 declaring a variable @racket[ret-x] to hold it, and return by @racket[Jump]ing
 to the label @racket[ret-lab]. This enables easily inlining an @racket[IntFun]
 by transforming it int an equivalent @racket[Let/ec].
}

@defstruct*[(ExtFun Fun) ([src ExternSrc?]
                          [args (listof Arg?)]
                          [ret-ty Type?]
                          [name c-identifier-string?])]{
 An "External Function" that references some externally-defined native function,
 e.g., @tt{exit} or @tt{write}. Programs that use @racket[ExtFun] are considered
 unsafe.
}

@defstruct*[Arg ([x symbol?]
                 [ty non-void-type?]
                 [mode (or/c 'read-only 'copy 'ref)])]{
 An argument in a function declaration. Specifies the name, @racket[x],
 of the argument, as well as it's type, and whether it is to be passed by
 value (@racket['copy]), reference (@racket['ref]), or immutable reference
 (@racket['read-only]).

 Arrays, records, and unions are always passed by reference, even if
 @racket['copy] is specified for @racket[mode]. Types smaller than a pointer
 will be passed by value if @racket['read-only] is specified for @racket[mode],
 as an optimization. Immutability is still enforced.
}

@defproc[(Fun-args [f (or/c IntFun? ExtFun?)]) (listof Arg?)]{
 Returns a list containing the arguments for @racket[f]. This is defined for
 convenience and works with both @racket[IntFun] and @racket[ExtFun].
}

@defproc[(Fun-ret-ty [f (or/c IntFun? ExtFun?)]) Type?]{
 Returns the return type of @racket[f]. This is defined for convenience and
 works with both @racket[IntFun] and @racket[ExtFun].
}

@; XXX Unpackers

@defproc[(give-name [v (or/c Path? Fun?)] [n symbol?]) (or/c MetaP? MetaFun?)]{
 Attaches metadata to @racket[v] which suggests to the compiler that
 @racket[n] should be used as the name for @racket[v] in the output
 program. The name @racket[n] is not emitted literally in the output
 program - it is still uniquified through @racket[cify].
}

@defproc[(given-name [v (or/c Path? Fun?)]) (or/c symbol? #f)]{
 Returns the name previously given to @racket[v] through @racket[give-name],
 or @racket[#f] if @racket[v] was not previously given a name.
}

@defstruct*[Program ([name->global (hash/c c-identifier-string? Global?)]
                     [name->ty (hash/c c-identifier-string? Type?)]
                     [name->fun (hash/c c-identifier-string IntFun?)])]{
 A whole @racketmodname[adqc] program. @racket[name->global],
 @racket[name->ty], and @racket[name->fun] refer to public global variables,
 type declarations, and funcions respectively.
}