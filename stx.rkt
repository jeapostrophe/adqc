#lang racket/base
(require racket/contract/base
         racket/list
         racket/match
         syntax/parse/define
         racket/stxparam
         "ast.rkt")

;; XXX This module should use plus not ast (i.e. the thing that does
;; type checking, termination checking, and resource analysis)

;; XXX Should these macros record the src location in the data
;; structure some how?

;; Float syntax
(define-simple-macro (define-flo-stx [tyname:id name:id bits arg-ctc] ...)
  (begin
    (begin
      (define tyname (FloT bits))
      (define (name v) (Flo bits v))
      (provide
       (contract-out
        [tyname Type?]
        [name (-> arg-ctc Expr?)])))
    ...))

(define-flo-stx
  [F32T F32 32 single-flonum?]
  [F64T F64 64 double-flonum?])

;; Integer syntax
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

;; Binary Op syntax
(define-simple-macro (define-bin-ops [name:id op] ...)
  (begin
    (begin
      (define (name L R)
        (BinOp op L R)) ...)
    (provide
     (contract-out [name (Expr? Expr? . -> . Expr?)] ...))))

(define-bin-ops
  [IAdd   'iadd  ]
  [ISub   'isub  ]
  [IMul   'imul  ]
  [ISDiv  'isdiv ]
  [IUDiv  'iudiv ]
  [ISRem  'isrem ]
  [IURem  'iurem ]
  [IShl   'ishl  ]
  [ILShr  'ilshr ]
  [IAShr  'iashr ]
  [IOr    'ior   ]
  [IAnd   'iand  ]
  [IXor   'ixor  ]
  [IEq    'ieq   ]
  [INe    'ine   ]
  [IUGt   'iugt  ]
  [ISGt   'isgt  ]
  [IUGe   'iuge  ]
  [ISGe   'isge  ]
  [IULt   'iult  ]
  [ISLt   'islt  ]
  [IULe   'iule  ]
  [ISLe   'isle  ]
  [FAdd   'fadd  ]
  [FSub   'fsub  ]
  [FMul   'fmul  ]
  [FDiv   'fdiv  ]
  [FRem   'frem  ]
  [FFalse 'ffalse]
  [FTrue  'ftrue ]
  [FOEq   'foeq  ]
  [FOGt   'fogt  ]
  [FOGe   'foge  ]
  [FOLt   'folt  ]
  [FOLe   'fole  ]
  [FONe   'fone  ]
  [FOrd   'ford  ]
  [FUEq   'fueq  ]
  [FUGt   'fugt  ]
  [FUGe   'fuge  ]
  [FULt   'fult  ]
  [FULe   'fule  ]
  [FUNe   'fune  ]
  [FUno   'funo  ])

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

(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     racket/dict
                     syntax/id-table))

(define-syntax-rule (define-expanders&macros
                      S-free-macros define-S-free-syntax
                      S-expander define-S-expander)
  (begin
    (begin-for-syntax
      (define S-free-macros (make-free-id-table))
      (struct S-expander (impl)
        #:property prop:procedure (struct-field-index impl)))
    (define-simple-macro (define-S-free-syntax id impl)
      (begin-for-syntax (dict-set! S-free-macros #'id impl)))
    (define-simple-macro (define-S-expander id impl)
      (define-syntax id (S-expander impl)))))

;; XXX implement T
(define-syntax (T stx)
  (syntax-parse stx
    [(_ x) #'x]))

;; XXX implement E
(define-syntax (E stx)
  (syntax-parse stx
    [(_ x) #'x]))

;; XXX implement P
(define-expanders&macros
  P-free-macros define-P-free-syntax
  P-expander define-P-expander)
(define-syntax (P stx)
  (syntax-parse stx
    [(_ x) #'x]))

;; XXX implement I
(define-syntax (I stx)
  (syntax-parse stx
    [(_ x) #'x]))

(define-expanders&macros
  S-free-macros define-S-free-syntax
  S-expander define-S-expander)

(define-syntax-parameter current-return #f)
(define-syntax-parameter current-return-var #f)

(define-syntax (while stx) (raise-syntax-error 'while "Illegal outside S" stx))
(define-syntax (S stx)
  (syntax-parse stx
    #:literals (void error begin define set! if let/ec while let unsyntax)
    [(_ (void)) (syntax/loc stx (Skip #f))]
    [(_ (void m)) (syntax/loc stx (Skip m))]
    [(_ (error m)) (syntax/loc stx (Fail m))]
    [(_ (begin)) (syntax/loc stx (S (void)))]
    [(_ (begin (define . d) . b)) (syntax/loc stx (S (let (d) . b)))]
    [(_ (begin s)) (syntax/loc stx (S s))]
    [(_ (begin a . d)) (syntax/loc stx (Begin (S a) (S (begin . d))))]
    [(_ (set! p e)) (syntax/loc stx (Assign (P p) (E e)))]
    [(_ (if p t f)) (syntax/loc stx (If (E p) (S t) (S f)))]
    [(_ (let/ec k:id . b))
     (syntax/loc stx
       (let ([k-id (gensym 'k)])
         (Let/ec k-id
                 (let ([the-ret (Jump k-id)])
                   (let-syntax ([k (S-expander (syntax-parser [(_) #'the-ret]))])
                     (S (begin . b)))))))]
    [(_ (while (~optional (~seq #:I I)
                          #:defaults ([I #'(U32 1)]))
          p . b))
     (syntax/loc stx (While (E p) (E I) (S (begin . b))))]
    [(_ (let ([x:id (~datum :) ty (~datum :=) xi]) . b))
     (syntax/loc stx
       (let ([x-id (gensym 'x)]
             [the-ty (T ty)])
         (Let x-id the-ty (I xi)
              (let ([the-x-ref (Var x-id the-ty)])
                (let-syntax ([x (P-expander
                                 (syntax-parser [_:id #'the-x-ref]))])
                  (S (begin . b)))))))]
    ;; XXX Call
    [(_ (~and macro-use (macro-id . _)))
     #:when (dict-has-key? S-free-macros #'macro-id)
     ((dict-ref S-free-macros #'macro-id) #'macro-use)]
    [(_ (~and macro-use (macro-id . _)))
     #:declare macro-id (static S-expander? "S expander")
     ((attribute macro-id.value) #'macro-use)]
    ;; XXX Maybe not require this and just silently drop out? Seems
    ;; confusing since we take over names (above)
    [(_ (unsyntax e)) #'e]))

(define-S-free-syntax cond
  (syntax-parser
    #:literals (else)
    [(_) (syntax/loc this-syntax (S (void)))]
    [(_ [else . b]) (syntax/loc this-syntax (S (begin . b)))]
    [(_ [q . a] . more)
     (syntax/loc this-syntax (S (if q (begin . a) (cond . more))))]))
(define-S-free-syntax when
  (syntax-parser
    [(_ p . t)
     (syntax/loc this-syntax
       (S (if p (begin . t) (void))))]))
(define-S-free-syntax unless
  (syntax-parser
    [(_ p . f)
     (syntax/loc this-syntax
       (S (if p (void) (begin . f))))]))
(define-S-free-syntax let*
  (syntax-parser
    #:literals ()
    [(_ () . b) (syntax/loc this-syntax (S (begin . b)))]
    [(_ (a . d) . b) (syntax/loc this-syntax (S (let (a) (let* d . b))))]))

(define-S-expander assert!
  (syntax-parser
    [(_
      (~optional (~and #:dyn (~bind [must-be-static? #f]))
                 #:defaults ([must-be-static? #t]))
      (~optional (~seq #:msg p-msg-expr)
                 #:defaults ([p-msg-expr #'#f]))
      p)
     #:with p-e (if (attribute must-be-static?)
                  (syntax/loc #'p (MetaE 'XXX-must-be-static p))
                  #'p)
     (syntax/loc this-syntax
       (let ([p-msg (or p-msg-expr (format "~a" 'p))])
         (S (if p-e
              (void (format "Checked: ~a" p-msg))
              (error (format "Failed! ~a" p-msg))))))]))

(begin-for-syntax
  (define-syntax-class Farg
    #:attributes (x ref var arg)
    #:description "function argument"
    [pattern
     ((~optional (~and #:const (~bind [mode #''copy]))
                 #:defaults ([mode #''read-only]))
      x:id (~datum :) ty)
     #:attr ref (generate-temporary #'x)
     #:attr var (syntax/loc this-syntax (Var (gensym 'x) ty))
     #:attr arg (syntax/loc this-syntax (Arg (Var-x ref) (Var-ty ref) mode))])
  (define-syntax-class Fret
    #:attributes (x ref var)
    #:description "function return"
    [pattern
     (x:id (~datum :) ty)
     #:attr ref (generate-temporary #'x)
     #:attr var (syntax/loc this-syntax (Var (gensym 'x) ty))]))
(define-syntax (F stx)
  (syntax-parse stx
    [(_ (a:Farg ...) (~datum :) r:Fret
        (~optional (~seq #:pre pre)
                   #:defaults ([pre #'(S64 1)]))
        (~optional (~seq #:post post)
                   #:defaults ([post #'(S64 1)]))
        (~optional (~seq #:return ret-lab:id)
                   #:defaults ([ret-lab (generate-temporary)]))
        . bs)
     ;; XXX check a ... unique
     (syntax/loc stx
       (let* ([ret-lab-id (gensym 'ret-lab)]
              [a.ref a.var] ...
              [r.ref r.var])
         (let-syntax ([a.x (P-expander (syntax-parser [_:id #'a.ref]))] ...)
           (let-values
               ([(the-post the-body)
                 (let-syntax ([r.x (P-expander (syntax-parser [_:id #'r.ref]))])
                   (values post
                           (let ([the-ret (Jump ret-lab-id)])
                             (let-syntax
                                 ([ret-lab (S-expander (syntax-parser [(_) #'the-ret]))])
                               (syntax-parameterize
                                   ([current-return
                                     (make-rename-transformer #'ret-lab)]
                                    [current-return-var
                                     (make-rename-transformer #'r.x)])
                                 (S (begin . bs)))))))])
             (IntFun (list a.arg ...) pre
                     (Var-x r.ref) (Var-ty r.ref) the-post
                     ret-lab-id the-body)))))]))

(define-syntax (define-fun stx)
  (syntax-parse stx
    [(_ x:id . more)
     #:do [(define inside-Prog? (syntax-parameter-value #'current-Prog))]
     #:with install-in-Prog
     (if inside-Prog?
       (syntax/loc stx
         (hash-set! (Program-name->fun current-Prog) (symbol->string 'x) x))
       #'(void))
     (quasisyntax/loc stx
       (begin (define x #,(syntax/loc #'more (F . more)))
              install-in-Prog))]
    [(_ (x:id . args) . more)
     (quasisyntax/loc stx
       (define-fun x . #,(syntax/loc #'args (args . more))))]))

;; XXX define-extern-fun

(define-syntax (define-global stx)
  (syntax-parse stx
    [(_ (~optional (~and #:public (~bind [public? #t]))
                   #:defaults ([public? #f]))
        x:id (~datum :) ty (~datum :=) xi)
     #:do [(define inside-Prog? (syntax-parameter-value #'current-Prog))]
     #:fail-when (and (attribute public?) (not inside-Prog?))
     "Cannot define public global outside of Prog"
     #:with make-public
     (and inside-Prog?
          (syntax/loc stx
            (hash-set! (Program-private->public current-Prog)
                       'x (symbol->string 'x))))
     #:with install-in-Prog
     (if inside-Prog?
       (syntax/loc stx
         (begin (hash-set! (Program-globals current-Prog) 'x x)
                make-public))
       #'(void))
     (syntax/loc stx
       (begin (define x (Global (T ty) (I xi)))
              install-in-Prog))]))

(define-syntax-parameter current-Prog #f)
(define-syntax (Prog stx)
  (syntax-parse stx
    [(_ pf ...)
     (syntax/loc stx
       (let ([the-prog (Program (make-hasheq) (make-hasheq) (make-hash))])
         (syntax-parameterize ([current-Prog (make-rename-transformer #'the-prog)])
           pf ...)
         the-prog))]))

(provide T P E I
         while assert! S
         define-S-free-syntax define-S-expander
         F
         define-fun define-global
         Prog)

;; XXX Array Slice
