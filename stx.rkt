#lang racket/base
(require racket/contract/base
         racket/list
         racket/match
         syntax/parse/define
         "ast.rkt")

;; XXX float stx

;; Defines constructors for standard integer types, then provides
;; them with appropriate contracts.
(define-simple-macro (define-int-stx [tyname:id name:id signed? bits] ...)
  (begin
    (begin
      (define tyname (IntT signed? bits))
      (define (name v) (Int signed? bits v))
      (provide
       (contract-out
        [tyname Type?]
        [name (-> (integer-in (- (expt 2 (sub1 bits)))
                              (sub1 (expt 2 (sub1 bits))))
                  Expr?)])))
    ...))

(define-int-stx
  [S8T  S8  #t  8]
  [S16T S16 #t 16]
  [S32T S32 #t 32]
  [S64T S64 #t 64]
  [U8T  U8  #f  8]
  [U16T U16 #f 16]
  [U32T U32 #f 32]
  [U64T U64 #f 64])

;; Defines constructors for binary ops, then provides
;; them with appropriate contracts
(define-simple-macro (define-bin-ops [name:id op] ...)
  (begin
    (begin
      (define (name L R)
        (BinOp op L R)) ...)
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

(define (And L R)
  (IAnd (INe (U32 0) L)
        (INe (U32 0) R)))

(define (Or L R)
  (IOr (INe (U32 0) L)
       (INe (U32 0) R)))

(define (Not e)
  (IEq (U32 0) e))

(define (Implies a b)
  (Or (Not a) b))

(define (Begin* . ss)
  (match ss
    [(list) (Skip)]
    [(list s) s]
    [(cons s ss) (Begin s (apply Begin* ss))]))
(define (When p t)
  (If p t (Skip)))
(define (Unless p f)
  (If p (Skip) f))

(provide
 (contract-out
  [And (-> Expr? Expr? Expr?)]
  [Or (-> Expr? Expr? Expr?)]
  [Not (-> Expr? Expr?)]
  [Implies (-> Expr? Expr? Expr?)]

  [Begin* (-> Stmt? ... Stmt?)]
  [When (-> Expr? Stmt? Stmt?)]
  [Unless (-> Expr? Stmt? Stmt?)]))

;; XXX beautiful macro to make writing nice
