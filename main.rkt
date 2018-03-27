#lang racket/base
(require racket/contract/base
         racket/match
         racket/set
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(define-syntax-rule
  (define-type (P [PF PF/ctc] ...)
    (V [F F/ctc] ...) ...)
  (begin (struct P (PF ...))
         (struct V P (F ...))
         ...
         (provide
          (contract-out
           (struct P ([PF PF/ctc] ...))
           (struct (V P) ([PF PF/ctc] ... [F F/ctc] ...))
           ...))))

;; AST

;;; Types
(define bitwidths '(1 8 16 32 64))
(define IntegerBitWidth?
  (apply or/c bitwidths))
(define FloatBitWidth?
  (or/c 32 64))
(define Dimension?
  exact-nonnegative-integer?)
(define Index?
  exact-nonnegative-integer?)
(define Field? symbol?)
(define-type (Type)
  ;; This is for external data
  (PtrT)
  (IntT [signed? boolean?] [w IntegerBitWidth?])
  (FloT [w FloatBitWidth?])
  ;; NOTE This means that functions are not polymorphic in the array
  ;; dimension, so we have to use meta-programming to generate many
  ;; versions of the same function specialized for different sizes, or
  ;; we have to slice arrays.
  (ArrT [dim Dimension?] [elem Type?])
  (RecT [field->ty (hash/c #:immutable #t Field? Type?)]))
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

;; Helpers

(define (integer-fits? ty i)
  (error 'XXX))
(define (float-fits? ty f)
  (error 'XXX))

(define (type=? x y)
  (error 'XXX))

(define (fail! . args)
  (error 'XXX))

(define (merge-v->ty x y)
  (error 'XXX))

;;; Expressions := Only read from memory, perform no allocation

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

;; XXX It should be possible to take an expression like "x.f" and
;; "x[5]" and combine them into "x.f + y[5]" by writing something
;; like:
#;(Bin 'iadd
       (RecR (VarR 'x) 'f)
       (Rename (hasheq 'y 'x)
               (ArrR (VarR 'x) (IntV 5))))
;; Perhaps it should also be possible to do something like a "Prefix"
;; to add a prefix to everything inside... although perhaps that's an
;; operation that can be written on top of Rename.

(define-type (Expr [ty Type?] [v->ty (hash/c Variable? Type?)])
  (*IntV [i exact-integer?])
  (*FloV [f flonum?])
  (*VarR [x Variable?])
  (*RecR [r Expr?] [f Field?])
  ;; NOTE In compiler/verifier, assert that i is within bounds. This
  ;; means we need to know a's type during the emit/verify process.
  (*ArrR [a Expr?] [i Expr?])
  ;; NOTE This is a static slice (so we can predict what the size of
  ;; the resulting array will be)... and is basically pointer
  ;; arithmetic.
  (*ArrS [a Expr?] [s Index?] [e Index?])
  (*Cast [e Expr?])
  (*Bin [op BinOperator?] [l Expr?] [r Expr?]))

(define (IntV ty i)
  (and (and (IntT? ty)
            (fail! ty "must be integer type"))
       (or (integer-fits? ty i)
           (fail! i "doesn't fit in" ty))
       (*IntV ty (hasheq) i)))
(define (FloV ty f)
  (and (and (FloT? ty)
            (fail! ty "must be float type"))
       (or (float-fits? ty f)
           (fail! f "doesn't fit in" ty))
       (*FloV ty (hasheq) f)))
(define (VarR ty v)
  (*VarR ty (hasheq v ty) v))
(define (RecR r f)
  (match (Expr-ty r)
    [(RecT f->t)
     (define f-ty (hash-ref f->t f #f))
     (and (or f-ty
              (fail! r "has no" f "field"))
          (*RecR f-ty (Expr-v->ty r) r f))]
    [_ (fail! r "is not a record")]))
(define (ArrR a i)
  (match (Expr-ty a)
    [(ArrT dim e-ty)
     (*ArrR e-ty (Expr-v->ty a) a i)]
    [_ (fail! a "is not an array")]))
(define (ArrS a s e)
  (and (or (<= s e)
           (fail! s "is not less than or equal to" e))
       (match (Expr-ty a)
         [(ArrT dim e-ty)
          (and (or (<= s dim)
                   (fail! a "is" dim "large, not long enough for slice to start at" s))
               (or (<= e dim)
                   (fail! a "is" dim "large, not long enough for slice to end at" e))
               (*ArrS (ArrT (- e s) e-ty) (Expr-v->ty a) a s e))]
         [_ (fail! a "is not an array")])))
(define (Cast ty e)
  (define ety (Expr-ty e))
  (and (or (NumT? ty)
           (fail! ty "must be number type"))
       (or (NumT? ety)
           (fail! e "is not a number"))
       (*Cast ty (Expr-v->ty e) e)))
(define (Bin op l r)
  (define lt (Expr-ty l))
  (define rt (Expr-ty r))
  (define v->ty (merge-v->ty (Expr-v->ty l) (Expr-v->ty r)))
  (define-values (Which? Result)
    (match op
      [(or 'iadd 'isub 'imul 'iudiv 'isdiv 'iurem 'isrem
           'ishl 'ilshr 'iashr 'iand 'ior 'ixor)
       (values IntT? lt)]
      [(or 'fadd 'fsub 'fmul 'fdiv 'frem)
       (values FloT? lt)]
      [(or 'ieq 'ine 'iugt 'iuge 'iult 'iule 'isgt 'isge 'islt 'isle)
       (values IntT? Bool)]
      [(or 'ffalse 'foeq 'fogt 'foge 'folt 'fole 'fone 'ford
           'ftrue 'fueq 'fuge 'fuge 'fult 'fule 'fune 'funo)
       (values FloT? Bool)]))
  (and (or (type=? lt rt)
           (fail! lt "and" rt "do not have same type for binary" op))
       (or (Which? lt)
           (fail! l "has wrong type," lt ", should be" Which?))
       (*Bin Result v->ty op l r)))

(define (BoolV b)
  (IntV Bool (if b 1 0)))

;;; Statements := May write to memory

#;(RecV [list-of-field*exp (listof (cons/c Field? Expr?))])
#;(ArrV [vs (listof Expr?)])

;;; Programs := Fix a memory rep and gives names

;; /AST
