#lang racket/base
(require racket/contract/base
         racket/list
         syntax/parse/define)

;; Expressions
(struct Expr () #:transparent)

;; Integer types
(struct Integer Expr (signed? bits val) #:transparent)

(define Integer-bit-width? (or/c 8 16 32 64))

;; Defines constructors for standard integer types, then provides
;; them with appropriate contracts.
(define-simple-macro (define-int-types [name:id signed? bits] ...)
  (begin
    (begin
      (define (name n) (Integer signed? bits n)) ...)
    (provide
     (contract-out
      [name (-> (integer-in (- (expt 2 (sub1 bits)))
                            (sub1 (expt 2 (sub1 bits))))
                Integer?)] ...))))

(define-int-types
  [S8  #t  8]
  [S16 #t 16]
  [S32 #t 32]
  [S64 #t 64]
  [U8  #f  8]
  [U16 #f 16]
  [U32 #f 32]
  [U64 #f 64])

;; Variable
;; TODO: symbol? should be a struct w/ type info.
(struct Var Expr (name) #:transparent)

;; Binary ops
(struct IBinOp Expr (op L R) #:transparent)

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
  [IAdd  'iadd ]
  [ISub  'isub ]
  [IMul  'imul ]
  [ISDiv 'isdiv]
  [IUDiv 'iudiv]
  [ISRem 'isrem]
  [IURem 'iurem]
  [IShl  'ishl ]
  [IAShr 'iashr]
  [IOr   'ior  ]
  [IAnd  'iand ]
  [IXor  'ixor ]
  [IEq   'ieq  ]
  [INe   'ine  ]
  [IUGt 'iugt  ]
  [ISGt 'isgt  ]
  [IUGe 'iuge  ]
  [ISGe 'isge  ]
  [IULt 'iult  ]
  [ISLt 'islt  ]
  [IULe 'iule  ]
  [ISLe 'isle  ])

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
(struct Stmt () #:transparent)
(struct Skip Stmt () #:transparent)
(struct Begin Stmt (f s) #:transparent)
(struct Assign Stmt (x e) #:transparent)
(struct If Stmt (p t f) #:transparent)
(struct While Stmt (p I body) #:transparent)
(struct Return Stmt (label) #:transparent)
(struct Let/ec Stmt (label body) #:transparent)

(define (Begin* . exps)
  (if (empty? exps)
      (Skip)
      (Begin (first exps)
             (apply Begin* (rest exps)))))
(define (When p t)
  (If p t (Skip)))
(define (Unless p f)
  (If p (Skip) f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  ;; Exprs
  [struct Expr ()]
  [struct IBinOp ([op symbol?] [L Expr?] [R Expr?])]
  [struct Integer ([signed? boolean?]
                   [bits Integer-bit-width?]
                   [val exact-integer?])]
  [struct Var ([name symbol?])]
  [And (-> Expr? Expr? Expr?)]
  [Or (-> Expr? Expr? Expr?)]
  [Not (-> Expr? Expr?)]
  [Implies (-> Expr? Expr? Expr?)]

  [struct Stmt ()]
  [struct Skip ()]
  [struct Begin ([f Stmt?] [s Stmt?])]
  [struct Assign ([x Var?] [e Expr?])]
  [struct If ([p Expr?] [t Stmt?] [f Stmt?])]
  [struct While ([p Expr?] [I Expr?] [body Stmt?])]
  [struct Return ([label symbol?])]
  [struct Let/ec ([label symbol?] [body Stmt?])]
  [Begin* (-> Stmt? ... Stmt?)]
  [When (-> Expr? Stmt? Stmt?)]
  [Unless (-> Expr? Stmt? Stmt?)]))
