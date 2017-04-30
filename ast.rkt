#lang racket/base
(require racket/contract/base
         plai/datatype)

(define bitwidths '(1 8 16 32 64))
(define IntegerBitWidth?
  (apply or/c bitwidths))
(define FloatBitWidth?
  (or/c 32 64))
(define Dimension?
  exact-nonnegative-integer?)
(define Field? symbol?)
(define-type Type
  ;; This is for external data
  (PtrT)
  ;; DESIGN I'm not adding vectors (for SIMD) because it is
  ;; complicated. In the future, I may but for now I won't and just
  ;; rely on optimizer to add them. If I do, I think I should just add
  ;; what OpenCL/GLSL does.
  (IntT [signed? boolean?] [w IntegerBitWidth?])
  (FloT [w FloatBitWidth?])
  ;; NOTE This means that functions are not polymorphic in the array
  ;; dimension, so we have to use meta-programming to generate many
  ;; versions of the same function specialized for different sizes, or
  ;; we have to slice arrays.
  (ArrT [dim Dimension?] [elem Type?])
  (RecT [list-of-field*ty (listof (cons/c Field? Type?))]))
(define AtomicT?
  (or/c PtrT? IntT? FloT?))
(define NumT?
  (or/c IntT? FloT?))

(define-syntax-rule (define-Integers [id signed? w] ...)
  (begin (define id (IntT signed? w)) ...))

(define-Integers
  [ U1 #f  1]
  [ U8 #f  8] [ S8 #t  8]
  [U16 #f 16] [S16 #t 16]
  [U32 #f 32] [S32 #t 32]
  [U64 #f 64] [S64 #t 64])
(define Bool U1)
(define F32 (FloT 32))
(define F64 (FloT 64))

(define Variable? symbol?)
(define Label? symbol?)

;; xxx global variables (& static as hidden globals)

(define BinOperator?
  (or/c
   ;; IBinOp
   'iadd 'isub 'imul 'iudiv 'isdiv 'iurem 'isrem
   'ishl 'ilshr 'iashr 'iand 'ior 'ixor
   ;; FBinOp
   'fadd 'fsub 'fmul 'fdiv 'frem
   ;; ICmp
   'ieq 'ine 'iugt 'iuge 'iult 'iule 'isgt 'isge 'islt 'isle
   ;; FCmp
   'ffalse 'foeq 'fogt 'foge 'folt 'fole 'fone 'ford
   'ftrue 'fueq 'fuge 'fuge 'fult 'fule 'fune 'funo))
(define-type Expr
  (VarR [x Variable?])
  (IntV [ty IntT?] [i exact-integer?])
  (FloV [ty FloT?] [f flonum?])
  (RecV [list-of-field*exp (listof (cons/c Field? Expr?))])
  (RecR [r Expr?] [f Field?])
  (ArrV [vs (listof Expr?)])
  ;; NOTE In compiler/verifier, assert that i is within bounds. This
  ;; means we need to know a's type during the emit/verify process.
  (ArrR [a Expr?] [i Expr?])
  ;; NOTE This is a static slice (so we can predict what the size of
  ;; the resulting array will be)
  (ArrS [a Expr?] [s exact-nonnegative-integer?] [e exact-nonnegative-integer?])
  ;; DESIGN Embedding p directly like this makes it hard to translate
  ;; this definition to Coq, because it makes everything mutually
  ;; recursive. On the other hand, embedding it makes it easier to do
  ;; code generation because I don't need to use mutation in the
  ;; meta-program to hoist a procedure definition out to some exterior
  ;; scope. It also means that I don't need to reason about procedure
  ;; environments. I think the best thing to do for now is to leave
  ;; this as is and specialize for the meta-program.
  (Call [p Procedure?]
        [ro-cpy (listof Expr?)] [ro-ref (listof LHS?)]
        [rw-ref (listof LHS?)])
  (Cast [ty NumT?] [e Expr?])
  (Bin [op BinOperator?] [l Expr?] [r Expr?]))

(define (BoolV b)
  (IntV Bool (if b 1 0)))

(define-type LHS
  (VarLHS [x Variable?])
  (ArrayLHS [a Expr?] [i Expr?])
  (RecordLHS [r Expr?] [f Field?]))

(define-type Statement
  ;; xxx add note? (or just add srcloc everywhere?)
  (Assert [e Expr?])
  (Assign [l LHS?] [e Expr?])
  (Return [e Expr?])
  (Let [read-only? boolean?] [x Variable?] [e Expr?]
       ;; x is bound in b, not in e
       ;; x MUST be used
       [b Statement?])
  (Seq [f Statement?] [s Statement?])
  (If [c Expr?] [t Statement?] [f Statement?])
  (Loop [lab Label?] [ty IntT?] [idx Variable?]
        [end exact-nonnegative-integer?]
        [end-e Expr?]
        ;; idx is read-only - if you need to skip around an array like
        ;; in binary search, then you need to use another variable and
        ;; this idx becomes a time bound.

        ;; start must be less than end
        ;; idx is bound in body
        ;; lab is valid in body
        [body Statement?])
  (Break [label Label?])
  (Continue [label Label?]))

(define Nop (Assert (BoolV #t)))
(define (When c t)
  (If c t Nop))
(define (Unless c f)
  (If c Nop f))
(define (While lab ty idx end end-e pred body)
  (Loop lab ty idx end end-e
        (Seq body
             (Unless pred
                     (Break lab)))))
(define (For lab ty idx end end-e
             f_id f_init f_pred f_iter
             body)
  (Let #f f_id f_init
       (While lab ty idx end end-e f_pred
              (Seq body
                   f_iter))))

(define-type ProcType
  (ProcArr [ret AtomicT?]
           [ro-cpy (listof Type?)]
           [ro-ref (listof Type?)]
           [rw-ref (listof Type?)]))
(define-type Procedure
  ;; NOTE Procedures do not have names in the core language, because
  ;; there is no recursion. We use their identity to ensure that they
  ;; are only type-checked once.
  
  ;; NOTE A procedure must return an atomic (register-sized) thing, so
  ;; the only way to get a bigger one is to allocate it before and
  ;; then pass it as `rw-ref` to the procedure.

  ;; DESIGN An alternative design would be to specify something like a
  ;; `wo-ref` that said the variable could only be used for writing or
  ;; to make Call a binding form, or something like that. The main
  ;; problem I forsee with this is that a pure procedure cannot return
  ;; a data-structure, but has to return something atomic. This means
  ;; that a function like `map` can't be pure. The only reason purity
  ;; matters (right now) is for asserts, so this isn't so bad.
  (Proc [ret AtomicT?]
        [ro-cpy (listof (cons/c Variable? Type?))]
        [ro-ref (listof (cons/c Variable? Type?))]
        [rw-ref (listof (cons/c Variable? Type?))]
        [body Statement?]))

(provide (all-defined-out))
