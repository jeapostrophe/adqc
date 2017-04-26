#lang racket/base
(require racket/contract/base
         racket/match
         racket/list
         racket/set
         plai/datatype)

(define-syntax-rule
  (match-type ty e [(Variant f ...) body ...] ...)
  (type-case
   ty e
   [Variant (f ...) (let () body ...)]
   ...))

;; AST
(define bitwidths '(1 8 16 32 64))
(define IntegerBitWidth?
  (apply or/c bitwidths))
(define FloatBitWidth?
  (or/c 32 64))
(define Dimension?
  exact-nonnegative-integer?)
(define Field? symbol?)
(define-type Type
  ;; This is for external data (i.e. pointers)
  (OpaqT)
  (IntT [signed? boolean?] [w IntegerBitWidth?])
  (FloT [w FloatBitWidth?])
  ;; xxx Is this too restrictive, because we can't take arbitrary
  ;; sized arrays?
  (ArrT [dim Dimension?] [elem Type?])
  (RecT [list-of-field*ty (listof (cons/c Field? Type?))]))
(define AtomicT?
  (or/c OpaqT? IntT? FloT?))
(define NumT?
  (or/c IntT? FloT?))

(define-syntax-rule (define-Integers [id signed? w] ...)
  (begin (define id (IntT signed? w)) ...))

(define-Integers
  [ U1 #f  1] [ S1 #t  1]
  [ U8 #f  8] [ S8 #t  8]
  [U16 #f 16] [S16 #t 16]
  [U32 #f 16] [S32 #t 16]
  [U64 #f 16] [S64 #t 16])
(define Bool U1)
(define F32 (FloT 32))
(define F64 (FloT 64))

(define Variable? symbol?)
(define Label? symbol?)

(define BinaryOperator?
  (or/c 'add 'fadd 'sub 'fsub 'mul 'fmul 'udiv 'sdiv 'fdiv 'urem 'srem 'frem
        'shl 'lshr 'ashr 'and 'or 'xor))
(define ICmpOperator?
  (or/c 'eq 'ne 'ugt 'uge 'ult 'ule 'sgt 'sge 'slt 'sle))
(define FCmpOperator?
  (or/c 'false 'oeq 'ogt 'oge 'olt 'ole 'one 'ord
        'true 'ueq 'uge 'uge 'ult 'ule 'une 'uno))
(define-type Expr
  (VarR [x Variable?])
  (IntegerV [ty IntT?] [i exact-integer?])
  (FloatV [ty FloT?] [f flonum?])
  (RecordV [list-of-field*exp (listof (cons/c Field? Expr?))])
  (RecordR [r Expr?] [f Field?])
  (ArrayV [vs (listof Expr?)])
  (ArrayR [a Expr?] [i Expr?])
  ;; NOTE In compiler/verifier, assert that i is correct dimension
  (Call [p Procedure?] [ins (listof Expr?)] [refs (listof LHS?)] [outs (listof LHS?)])
  (NumCast [ty NumT?] [e Expr?])
  (BinOp [op BinaryOperator?] [l Expr?] [r Expr?])
  (ICmpOp [op ICmpOperator?] [l Expr?] [r Expr?])
  (FCmpOp [op FCmpOperator?] [l Expr?] [r Expr?]))

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
  (If [c Expr?] [t Statement?] [e Statement?])
  (Loop [lab Label?] [ty IntT?] [idx Variable?]
        [start exact-integer?] [end exact-integer?]
        ;; idx is read-only - if you need to skip around an array like
        ;; in binary search, then you need to use another variable and
        ;; this idx becomes a time bound.

        ;; start must be less than end
        ;; idx is bound in body
        ;; lab is valid in body
        [body Statement?])
  (Break [label Label?])
  (Continue [label Label?]))

(define-type ProcType
  (ProcArr [ret AtomicT?]
           [ins (listof Type?)]
           [refs (listof Type?)]
           [outs (listof Type?)]))
(define-type Procedure
  ;; No name, because no recursion
  (Proc [ret AtomicT?]
        [ins (listof (cons/c Variable? Type?))]
        ;; in is copied?
        [refs (listof (cons/c Variable? Type?))]
        ;; refs is not copied, but not write-able
        [outs (listof (cons/c Variable? Type?))]
        ;; out is not copied? but write-able
        ;; ins, refs, and outs are bound in body
        ;; body must return ret (it is guaranteed to be register sized)
        [body Statement?]))

;; Type Checker

;; xxx type-checker should return a maybe type, a writeability (to
;; know if an array/record reference is writeable), list of errors,
;; and set of read variables, and really a whole derivation so we can
;; make a type-directed compiler (useful for figuring out the right
;; assertions to spit out, for example)

(define (type= where x y)
  (unless (equal? x y)
    (error 'type= "~a: ~v and ~v" where x y)))

(define (typec-int ty n)
  (match-define (IntT signed? bw) ty)
  (when (and (negative? n) (not signed?))
    (error 'typec-int "Negative constant and unsigned number"))
  (unless (<= (integer-length n) bw)
    (error 'typec-int "Constant too large for bitwidth")))
(define (typec-float ty n)
  (match-define (FloT bw) ty)
  (unless ((match bw [32 single-flonum?] [64 double-flonum?]) n)
    (error 'typec-float "Constant float wrong size for bitwidth ~v" n)))

(define (typec-expr e
                    #:p->t p->t
                    #:var-env var-env)
  (define (rec e)
    (typec-expr e
                #:p->t p->t
                #:var-env var-env))
  (match-type
   Expr e
   [(VarR x)
    (match (hash-ref var-env x #f)
      [(cons xt _) xt]
      [#f (error 'typec "Unbound variable: ~v" x)])]
   [(IntegerV ty i)
    (typec-int ty i)
    ty]
   [(FloatV ty f)
    (typec-float ty f)
    ty]
   [(RecordV lofe)
    ;; FIXME Should find all the duplicates
    (match (check-duplicates (map car lofe))
      [#f (void)]
      [dupe (error 'typec "Duplicated field in record: ~v" dupe)])
    (RecT
     (for/list ([fe (in-list lofe)])
       (match-define (cons f e) fe)
       (cons f (rec e))))]
   [(RecordR r rf)
    (match-define (RecT loft) (rec r))
    (for/or ([ft (in-list loft)])
      (match-define (cons f t) ft)
      (and (eq? rf f)
           t))]
   [(ArrayV vs)
    (define len (length vs))
    (when (zero? len)
      (error 'typec "Array must have non-zero length"))
    (match-define (cons v0 vn) vs)
    (define v0t (rec v0))
    (for/and ([vi (in-list vn)])
      (type= "homogeneous array elements" (rec vi) v0t))
    (ArrT len v0t)]
   [(ArrayR a i)
    (match-define (ArrT len vt) (rec a))
    (match-define (IntT #f bw) (rec i))
    (define (closest-bitwidth some-bw)
      (for/or ([bw (in-list bitwidths)])
        (and (<= some-bw bw) bw)))
    (unless (= (closest-bitwidth (integer-length len)) bw)
      (error 'typec "Bitwidth of index is too large for array dimension"))
    vt]
   [(Call p ins refs outs)
    (match-define (ProcArr ret_p ins_p refs_p outs_p) (typec-proc p->t p))
    (define (compare kind rec tys es)
      (unless (= (length tys) (length es))
        (error 'typec "Argument count mismatch: ~a" kind))
      (for/list ([t (in-list tys)]
                 [e (in-list es)])
        (type= (format "call ~a arguments" kind) (rec e) t)))
    (compare 'ins rec ins_p ins)
    (define ((rec-lhs check-read-only?) l)
      (typec-lhs l
                 #:p->t p->t
                 #:var-env var-env
                 #:check-read-only? check-read-only?))
    (compare 'refs (rec-lhs #f) refs_p refs)
    (compare 'outs (rec-lhs #t) outs_p outs)
    ret_p]
   [(NumCast t e)
    (define et (rec e))
    (unless (NumT? et)
      (error 'typec "May only cast numbers: ~v" et))
    t]
   [(BinOp op l r)
    (define lt (rec l))
    (define rt (rec r))
    (type= "BinOp" lt rt)
    (unless (NumT? lt)
      (error 'typec "Not integer in BinOp"))
    lt]
   [(ICmpOp op l r)
    (define lt (rec l))
    (define rt (rec r))
    (type= "ICmpOp" lt rt)
    (unless (IntT? lt)
      (error 'typec "Not integer in ICmpOp"))
    Bool]
   [(FCmpOp op l r)
    (define lt (rec l))
    (define rt (rec r))
    (type= "FCmpOp" lt rt)
    (unless (FloT? lt)
      (error 'typec "Not float in FCmpOp"))
    Bool]))

(define (typec-lhs l
                   #:p->t p->t
                   #:var-env var-env
                   #:check-read-only? check-read-only?)
  (define (rec-e e)
    (typec-expr e
                #:p->t p->t
                #:var-env var-env))
  (match-type
   LHS l
   [(VarLHS x)
    (match (hash-ref var-env x #f)
      [(cons xt writeable?)
       (unless writeable?
         (when check-read-only?
           (error 'typec-lhs "~e is read-only" x)))
       xt]
      [_ (error 'typec-lhs "Unbound variable: ~v" x)])]
   [(ArrayLHS a i)
    (error 'typec-lhs "XXX ArrayLHS: Code is basically same as typec-expr for ArrayR, except that we need to check that the array is writeable, which is not currently available.")]
   [(RecordLHS r i)
    (error 'typec-lhs "XXX RecordLHS: Code is basically same as typec-expr for RecordR, except that we need to check that the array is writeable, which is not currently available.")]))

(define (typec-stmt s
                    #:p->t p->t
                    #:must-return? must-return?
                    #:return-ty return-ty
                    #:var-env var-env
                    #:label-set label-set)
  (define (doesnt-return!)
    (when must-return?
      (error 'stmt-typec "must return")))
  (define (rec s
               #:must-return? [must-return? must-return?]
               #:return-ty [return-ty return-ty]
               #:var-env [var-env var-env]
               #:label-set [label-set label-set])
    (typec-stmt s
                #:p->t p->t
                #:must-return? must-return?
                #:return-ty return-ty
                #:var-env var-env
                #:label-set label-set))
  (define (rec-e e)
    (typec-expr e
                #:p->t p->t
                #:var-env var-env))
  (match-type
   Statement s
   [(Assert e)
    (type= "Assert" (rec-e e) Bool)
    (doesnt-return!)]
   [(Assign l e)
    (define lt
      (typec-lhs l
                 #:p->t p->t
                 #:var-env var-env
                 #:check-read-only? #t))
    (define et (rec-e e))
    (type= "Assign" lt et)
    (doesnt-return!)]
   [(Return e)
    (type= "Return" (rec-e e) return-ty)]
   [(Let read-only? x e b)
    (define et (rec-e e))
    ;; xxx x MUST be read in b
    (rec b #:var-env (hash-set var-env x (cons et (not read-only?))))]
   [(Seq f r)
    (and
     (rec f #:must-return? #f)
     (rec r))]
   [(If c t e)
    (type= "If Condition" (rec-e c) Bool)
    (and (rec t) (rec e))]
   [(Loop lab ty idx start end b)
    (typec-int ty start)
    (typec-int ty end)
    (unless (< start end)
      (error 'typec-stmt "loop start not before loop end"))
    (rec b
         #:var-env (hash-set var-env idx (cons ty #f))
         #:label-set (set-add label-set lab))]
   [(Break lab)
    (unless (set-member? label-set lab)
      (error 'typec-stmt "Unknown label: ~v" lab))
    (doesnt-return!)]
   [(Continue lab)
    (unless (set-member? label-set lab)
      (error 'typec-stmt "Unknown label: ~v" lab))
    (doesnt-return!)]))
(define (typec-proc p->t p)
  (hash-ref!
   p->t p
   (λ ()
     (match-define (Proc ret ins refs outs body) p)
     (define (add-vars to l writeable?)
       (for/fold ([σ to]) ([i (in-list l)])
         (match-define (cons ii it) i)
         (when (hash-has-key? σ ii)
           (error 'typec-proc "Duplicate argument: ~e" ii))
         (hash-set σ ii (cons it writeable?))))
     (define var-env0 (add-vars (hasheq) ins #t))
     (define var-env1 (add-vars var-env0 refs #f))
     (define var-env2 (add-vars var-env1 outs #t))
     (and
      (typec-stmt body
                  #:p->t p->t
                  #:must-return? #t
                  #:return-ty ret
                  #:var-env var-env2
                  #:label-set (seteq))
      (ProcArr ret (map cdr ins) (map cdr refs) (map cdr outs))))))
(define (typec-main m)
  (define p->t (make-hasheq))
  (define mt (typec-proc p->t m))
  (match mt
    [(ProcArr (== U8) (list) (list) (list))
     #t]
    [_
     (error 'main-type "Invalid main function(~e) returns (~e)" m mt)]))

;; Interpreter
(struct lhs-value (reader writer))
(define (lhs-read lv)
  ((lhs-value-reader lv)))
(define (lhs-write! lv nv)
  ((lhs-value-writer lv) nv))

(define (eval-primitive which op lv rv)
  ((match which
     ['ICmp
      (match op
        ['eq =]
        ['slt <])])
   lv rv))

(define (eval-expr σ e)
  (define (rec e) (eval-expr σ e))
  (define (rec-lhs l) (lhs-read (eval-lhs σ l)))
  (match-type
   Expr e
   [(VarR x) (unbox (hash-ref σ x))]
   [(IntegerV _ i) i]
   [(FloatV _ f) f]
   [(RecordV lofe)
    (define r (make-hasheq))
    (for ([fe (in-list lofe)])
      (match-define (cons f e) fe)
      (hash-set! r f (rec e)))
    r]
   [(RecordR r f)
    (hash-ref (rec r) f)]
   [(ArrayV vs)
    (apply vector (map rec vs))]
   [(ArrayR a i)
    (vector-ref (rec a) (rec i))]
   [(Call p ins refs outs)
    (eval-proc p (map rec ins) (map rec-lhs refs) (map rec-lhs outs))]
   [(NumCast t e)
    (define conv
      (match t
        [(IntT sign? bw)
         (λ (v)
           (modulo (inexact->exact v)
                   (expt 2 (if sign? (sub1 bw) bw))))]
        [(FloT 32) real->single-flonum]
        [(FloT 64) real->double-flonum]))
    (conv (rec e))]
   [(BinOp op l r)
    (eval-primitive 'Bin op (rec l) (rec r))]
   [(ICmpOp op l r)
    (eval-primitive 'ICmp op (rec l) (rec r))]
   [(FCmpOp op l r)
    (eval-primitive 'FCmp op (rec l) (rec r))]))

(define (eval-lhs σ l)
  (match-type
   LHS l
   [(VarLHS x)
    (define xb (hash-ref σ x))
    (lhs-value (λ () (unbox xb))
               (λ (nv) (set-box! xb nv)))]
   [(ArrayLHS a i)
    (define av (eval-expr σ a))
    (define iv (eval-expr σ i))
    (lhs-value (λ () (vector-ref av iv))
               (λ (nv) (vector-set! av iv nv)))]
   [(RecordLHS r f)
    (define rv (eval-expr σ r))
    (lhs-value (λ () (hash-ref rv f))
               (λ (nv) (hash-set! rv f nv)))]))

(define (eval-stmt var-env label-env return s)
  (define (rec s
               #:var-env [var-env var-env]
               #:label-env [label-env label-env])
    (eval-stmt var-env label-env return s))
  (match-type
   Statement s
   [(Assert e)
    (unless (eval-expr var-env e)
      (error 'eval "Assert failed: ~v" e))]
   [(Assign l e)
    (define lv (eval-lhs var-env l))
    (define nv (eval-expr var-env e))
    (lhs-write! lv nv)]
   [(Return e)
    (return (eval-expr var-env e))]
   [(Let _ x e b)
    (rec #:var-env (hash-set var-env x (box (eval-expr var-env e)))
         b)]
   [(Seq f r)
    (rec f) ;; <--- Any 0-valued return is here
    (rec r)]
   [(If c t f)
    (if (eval-expr var-env c)
      (rec t)
      (rec f))]
   [(Loop lab _ i s e b)
    (let/ec break
      (for ([ci (in-range s e)])
        (let/ec continue
          (rec #:var-env (hash-set var-env i (box ci))
               #:label-env (hash-set label-env lab (cons break continue))
               b))))]
   ;; The type-checker has ensured that these 0-valued returns are
   ;; okay
   [(Break lab)
    (match-define (cons b c) (hash-ref label-env lab))
    (b)]
   [(Continue lab)
    (match-define (cons b c) (hash-ref label-env lab))
    (c)]))

(define (eval-proc p invs refvs outvs)
  (match-define (Proc _ ins refs outs b) p)
  (define (add-vars to ids vs)
    (for/fold ([σ to]) ([i (in-list ids)] [iv (in-list vs)])
      (match-define (cons ii it) i)
      (hash-set σ ii (box iv))))
  (define var-env0 (add-vars (hasheq) ins invs))
  (define var-env1 (add-vars var-env0 refs refvs))
  (define var-env2 (add-vars var-env1 outs outvs))
  (let/ec return
    (eval-stmt var-env2 (hasheq) return b)))

(define (eval-main m)
  (eval-proc m (list) (list) (list)))

(define (adqc-eval m)
  (typec-main m)
  (eval-main m))

;; Examples
(module+ test
  (define N 12)
  (define LinearSearch
    (Proc S8
          (list (cons 'x U8))
          (list (cons 'array (ArrT N U8)))
          (list)
          (Seq
           (Loop 'main U8 'i 0 N
                 (If (ICmpOp 'eq (VarR 'i) (VarR 'x))
                     (Return (NumCast S8 (VarR 'i)))
                     (Continue 'main)))
           (Return (IntegerV S8 -1)))))
  (define Main
    (Proc U8 (list) (list) (list)
          (Let #t 'a (ArrayV (for/list ([i (in-range N)])
                               (IntegerV U8 i)))
               (Let #t 'res (Call LinearSearch
                                  (list (IntegerV U8 5))
                                  (list (VarLHS 'a))
                                  (list))
                    (If (ICmpOp 'slt (VarR 'res) (IntegerV S8 0))
                        (Return (IntegerV U8 1))
                        (Return (IntegerV U8 0)))))))
  (adqc-eval Main))
