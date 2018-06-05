#lang racket/base
(require racket/contract/base
         racket/list
         racket/match
         syntax/parse/define
         "ast.rkt")

;; XXX This module should use plus not ast (i.e. the thing that does
;; type checking, termination checking, and resource analysis)

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

(provide
 (contract-out
  [And (-> Expr? Expr? Expr?)]
  [Or (-> Expr? Expr? Expr?)]
  [Not (-> Expr? Expr?)]
  [Implies (-> Expr? Expr? Expr?)]))

;; XXX beautiful macro to make writing nice

(require (for-syntax racket/base
                     syntax/parse
                     racket/dict
                     syntax/id-table))

(define-syntax (E stx)
  (syntax-parse stx
    [(_ x) #'x]))
(define-syntax (P stx)
  (syntax-parse stx
    [(_ x) #'x]))

(begin-for-syntax
  (define S-free-macros (make-free-id-table))
  (struct S-expander (impl)
    #:property prop:procedure (λ (se stx) ((S-expander-impl se) stx))))
(define-simple-macro (define-S-free-syntax id impl)
  (begin-for-syntax (dict-set! S-free-macros #'id impl)))
(define-simple-macro (define-S-expander id impl)
  (define-syntax id (S-expander impl)))

(define-syntax (while stx) (raise-syntax-error 'while "Illegal outside S" stx))
(define-syntax (S stx)
  (syntax-parse stx
    #:literals (void error begin set! if let/ec while unsyntax)
    [(_ (void)) (syntax/loc stx (Skip #f))]
    [(_ (void m)) (syntax/loc stx (Skip m))]
    [(_ (error m)) (syntax/loc stx (Fail m))]
    [(_ (begin)) (syntax/loc stx (S (void)))]
    [(_ (begin s)) (syntax/loc stx (S s))]
    [(_ (begin a . d)) (syntax/loc stx (Begin (S a) (S (begin . d))))]
    [(_ (set! x e)) (syntax/loc stx (Assign (P x) (E e)))]
    [(_ (if p t f)) (syntax/loc stx (If (E p) (S t) (S f)))]
    [(_ (let/ec k:id . b))
     (syntax/loc stx
       (let ([k-id (gensym 'k)])
         (Let/ec k-id
                 (let ([the-ret (Jump k-id)])
                   (let-syntax ([k (S-expander
                                    (λ (stx) (syntax-case stx () [(_) #'the-ret])))])
                     (S (begin . b)))))))]
    [(_ (while p . b))
     ;; XXX Add I keyword
     (syntax/loc stx (While (E p) (U32 1) (S (begin . b))))]
    [(_ (~and macro-use (macro-id . _)))
     #:when (dict-has-key? S-free-macros #'macro-id)
     ((dict-ref S-free-macros #'macro-id) #'macro-use)]
    [(_ (~and macro-use (macro-id . _)))
     #:declare macro-id (static S-expander? "S expander")
     ((attribute macro-id.value) #'macro-use)]
    ;; XXX Maybe not require this and just silently drop out? Seems
    ;; confusing since we take over names (above)
    [(_ (unsyntax e)) #'e]))

;; XXX Make a macro/protocol for calling a function where you inline
;; and communicate the return value back in a sensible way.
;;
#;(Begin A (set! X (call F args)) B)
;; :=
#;(Begin A (F X args) B)

(define-S-free-syntax cond
  (λ (stx)
    (syntax-parse stx
      #:literals (else)
      [(_) (syntax/loc stx (S (void)))]
      [(_ [else . b]) (syntax/loc stx (S (begin . b)))]
      [(_ [q . a] . more)
       (syntax/loc stx (S (if q (begin . a) (cond . more))))])))
(define-S-free-syntax when
  (λ (stx)
    (syntax-parse stx
      [(_ p . t) (syntax/loc stx (S (if p (begin . t) (void))))])))
(define-S-free-syntax unless
  (λ (stx)
    (syntax-parse stx
      [(_ p . f) (syntax/loc stx (S (if p (void) (begin . f))))])))

(define-S-expander assert!
  (λ (stx)
    (syntax-parse stx
      [(_
        (~optional (~and #:dyn (~bind [must-be-static? #f]))
                   #:defaults ([must-be-static? #t]))
        (~optional (~seq #:msg p-msg-expr)
                   #:defaults ([p-msg-expr #'#f]))
        p)
       #:with p-e (if (attribute must-be-static?)
                    (syntax/loc #'p (Static p))
                    #'p)
       (syntax/loc stx
         (let ([p-msg (or p-msg-expr (format "Static Assertion: ~a" 'p))])
           (S (if p-e
                (void p-msg)
                (error p-msg)))))])))

(provide E P
         while assert! S
         define-S-free-syntax define-S-expander)
