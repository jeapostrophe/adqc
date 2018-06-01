#lang racket/base
(require racket/contract/base
         syntax/parse/define)

;; Value types
(struct Integer (signed? bits val) #:transparent)

(define Integer-bit-width? (or/c 8 16 32 64))

(define-simple-macro (define-int-type name:id signed? bits)
  (define (name v)
    (Integer signed? bits v)))

(define-int-type i8 #t 8)
(define-int-type i16 #t 16)
(define-int-type i32 #t 32)
(define-int-type i64 #t 64)

(define-int-type u8 #f 8)
(define-int-type u16 #f 16)
(define-int-type u32 #f 32)
(define-int-type u64 #f 64)

;; Binary ops
(struct IBinOp (op L R) #:transparent)

(define-simple-macro (define-bin-op name:id op)
  (define (name L R)
    (IBinOp op L R)))

(define-bin-op IAdd 'iadd)
(define-bin-op ISub 'isub)
(define-bin-op IMul 'imul)
(define-bin-op ISDiv 'isdiv)
(define-bin-op IUDiv 'iudiv)
(define-bin-op ISRem 'isrem)
(define-bin-op IURem 'iurem)
(define-bin-op IShl 'ishl)
(define-bin-op IAShr 'iashr)
(define-bin-op IOr 'ior)
(define-bin-op IAnd 'iand)
(define-bin-op IXor 'ixor)

;; Binary cmp
(struct ICmp (op L R) #:transparent)

(define-simple-macro (define-cmp name:id op)
  (define (name L R)
    (ICmp op L R)))

(define-cmp IEq 'ieq)
(define-cmp INe 'ine)
(define-cmp IUGt 'iugt)
(define-cmp ISGt 'isgt)
(define-cmp IUGe 'iuge)
(define-cmp ISGe 'isge)
(define-cmp IULt 'iult)
(define-cmp ISLt 'islt)
(define-cmp IULe 'iule)
(define-cmp ISLe 'isle)

;; Expr Macros
(define (And L R)
  (IAnd (INe 0 L)
        (INe 0 R)))

(define (Or L R)
  (IOr (INe 0 L)
       (INe 0 R)))

(define (Not e)
  (IEq 0 e))

(define (Implies a b)
  (Or (Not a) b))


;; Statements
(struct Skip () #:transparent)
(struct Begin (L-stmt R-stmt) #:transparent)
(struct Assign (dest exp) #:transparent)
(struct If (pred then else) #:transparent)
(struct While (pred invar do-stmt) #:transparent)


;; Predicates for Expr and Stmt
(define Expr? (or/c Integer? IBinOp? ICmp?))
(define Stmt? (or/c Skip? Begin? Assign? If? While?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 ;; TODO: contracts for integer constructors.
 i64
 (contract-out
  ;; Exprs
  [struct IBinOp ([op symbol?] [L Expr?] [R Expr?])]
  [struct ICmp ([op symbol?] [L Expr?] [R Expr?])]
  [struct Integer ([signed? boolean?] [bits Integer-bit-width?] [val exact-integer?])]
  [IAdd (Expr? Expr? . -> . Expr?)]
  [ISub (Expr? Expr? . -> . Expr?)]
  [IMul (Expr? Expr? . -> . Expr?)]
  [ISDiv (Expr? Expr? . -> . Expr?)]
  [IUDiv (Expr? Expr? . -> . Expr?)]
  [ISRem (Expr? Expr? . -> . Expr?)]
  [IURem (Expr? Expr? . -> . Expr?)]
  [IShl (Expr? Expr? . -> . Expr?)]
  [IAShr (Expr? Expr? . -> . Expr?)]
  [IOr (Expr? Expr? . -> . Expr?)]
  [IAnd (Expr? Expr? . -> . Expr?)]
  [IXor (Expr? Expr? . -> . Expr?)]
  [IEq (Expr? Expr? . -> . Expr?)]
  [INe (Expr? Expr? . -> . Expr?)]
  [IUGt (Expr? Expr? . -> . Expr?)]
  [ISGt (Expr? Expr? . -> . Expr?)]
  [IUGe (Expr? Expr? . -> . Expr?)]
  [ISGe (Expr? Expr? . -> . Expr?)]
  [IULt (Expr? Expr? . -> . Expr?)]
  [ISLt (Expr? Expr? . -> . Expr?)]
  [IULe (Expr? Expr? . -> . Expr?)]
  [ISLe (Expr? Expr? . -> . Expr?)]
  [And (Expr? Expr? . -> . Expr?)]
  [Or (Expr? Expr? . -> . Expr?)]
  [Not (Expr? . -> . Expr?)]
  [Implies (Expr? Expr? . -> . Expr?)]
  ;; Stmts
  [struct Skip ()]
  [struct Begin ([L-stmt Stmt?] [R-stmt Stmt?])]
  [struct Assign ([dest symbol?] [exp Expr?])]
  [struct If ([pred Expr?] [then Stmt?] [else Stmt?])]
  [struct While ([pred Expr?] [invar Expr?] [do-stmt Stmt?])]))
