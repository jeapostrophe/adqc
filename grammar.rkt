#lang racket/base
(require racket/contract/base
         syntax/parse/define)

;; Integer types
(struct Integer (signed? bits val) #:transparent)

(define Integer-bit-width? (or/c 8 16 32 64))

;; Defines constructors for standard integer types, then provides
;; them with appropriate contracts.
(define-simple-macro (define-int-types [name:id signed? bits] ...)
  (begin
    (begin
      (define (name n)
        (Integer signed? bits n)) ...)
    (provide
     (contract-out
      [name (-> (integer-in (- (expt 2 (sub1 bits)))
                            (sub1 (expt 2 (sub1 bits))))
                Integer?)] ...))))

(define-int-types
  [i8 #t 8]
  [i16 #t 16]
  [i32 #t 32]
  [i64 #t 64]
  [u8 #f 8]
  [u16 #f 16]
  [u32 #f 32]
  [u64 #f 64])


;; Binary ops
(struct IBinOp (op L R) #:transparent)

;; Defines constructors for binary ops, then provides
;; them with appropriate contracts
(define-simple-macro (define-bin-ops [name:id op] ...)
  (begin
    (begin
      (define (name L R)
        (IBinOp op L R)) ...)
    (provide
     (contract-out [name (Expr? Expr? . -> . Expr?)] ...))))

(define-bin-ops
  [IAdd 'iadd]
  [ISub 'isub]
  [IMul 'imul]
  [ISDiv 'isdiv]
  [IUDiv 'iudiv]
  [ISRem 'isrem]
  [IURem 'iurem]
  [IShl 'ishl]
  [IAShr 'iashr]
  [IOr 'ior]
  [IAnd 'iand]
  [IXor 'ixor])


;; Binary cmp
(struct ICmp (op L R) #:transparent)

;; Defines constructors for integer comparisons, then provides
;; them with appropriate contracts.
(define-simple-macro (define-cmps [name:id op] ...)
  (begin
    (begin
      (define (name L R)
        (ICmp op L R)) ...)
    (provide
     (contract-out [name (Expr? Expr? . -> . Expr?)] ...))))

(define-cmps
  [IEq 'ieq]
  [INe 'ine]
  [IUGt 'iugt]
  [ISGt 'isgt]
  [IUGe 'iuge]
  [ISGe 'isge]
  [IULt 'iult]
  [ISLt 'islt]
  [IULe 'iule]
  [ISLe 'isle])


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
;; TODO: symbol? should be a struct w/ type info.
(define Expr? (or/c symbol? Integer? IBinOp? ICmp?))
(define Stmt? (or/c Skip? Begin? Assign? If? While?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  ;; Exprs
  [struct IBinOp ([op symbol?] [L Expr?] [R Expr?])]
  [struct ICmp ([op symbol?] [L Expr?] [R Expr?])]
  [struct Integer ([signed? boolean?]
                   [bits Integer-bit-width?]
                   [val exact-integer?])]
  [And (Expr? Expr? . -> . Expr?)]
  [Or (Expr? Expr? . -> . Expr?)]
  [Not (Expr? . -> . Expr?)]
  [Implies (Expr? Expr? . -> . Expr?)]
  ;; Stmts
  [struct Skip ()]
  [struct Begin ([L-stmt Stmt?] [R-stmt Stmt?])]
  [struct Assign ([dest symbol?] [exp Expr?])]
  [struct If ([pred Expr?] [then Stmt?] [else Stmt?])]
  [struct While ([pred Expr?] [invar Expr?] [do-stmt Stmt?])]
  ;; Predicates
  [Expr? (any/c . -> . boolean?)]
  [Stmt? (any/c . -> . boolean?)]))
