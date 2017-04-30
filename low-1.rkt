#lang racket/base
(require racket/contract/base
         racket/match
         racket/list
         racket/set
         plai/datatype
         "ast.rkt")

(define-syntax-rule
  (match-type ty e [(Variant f ...) body ...] ...)
  (type-case
   ty e
   [Variant (f ...) (let () body ...)]
   ...))

;; xxx
(define-type TypeError
  (TyErrEq [lhs Type?] [rhs Type?])
  (TyErrPred [where string?] [pred string?] [ty Type?])
  (TyErrReturn [where string?] ))

;; xxx
(define-type TypesExprR
  ;; Γ |- e => ListOfErrors x (T|#f) x CanWrite? x {VarsRead}
  ;; xxx add derivation
  (ATypesExprR [errs (listof TypeError?)]
               [ty (or/c #f Type?)]
               ;; xxx would it be better to explicitly evaluate to a pointer?
               [can-write? boolean?]
               [pure? boolean?]
               ;; xxx things other than vars are written, but they are
               ;; always discovered through variables.
               [vars-written (set/c Variable?)]
               [vars-read (set/c Variable?)]))

;; xxx would it be better to use a global (parameter) database of all
;; this stuff mapped per expr/stmt/etc? (Not great because code is in
;; different environments.)

;; Type Checker
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
   [(IntV ty i)
    (typec-int ty i)
    ty]
   [(FloV ty f)
    (typec-float ty f)
    ty]
   [(RecV lofe)
    ;; FIXME Should find all the duplicates
    (match (check-duplicates (map car lofe))
      [#f (void)]
      [dupe (error 'typec "Duplicated field in record: ~v" dupe)])
    (RecT
     (for/list ([fe (in-list lofe)])
       (match-define (cons f e) fe)
       (cons f (rec e))))]
   [(RecR r rf)
    (match-define (RecT loft) (rec r))
    (for/or ([ft (in-list loft)])
      (match-define (cons f t) ft)
      (and (eq? rf f)
           t))]
   [(ArrV vs)
    (define len (length vs))
    (when (zero? len)
      (error 'typec "Array must have non-zero length"))
    (match-define (cons v0 vn) vs)
    (define v0t (rec v0))
    (for/and ([vi (in-list vn)])
      (type= "homogeneous array elements" (rec vi) v0t))
    (ArrT len v0t)]
   [(ArrR a i)
    (match-define (ArrT len vt) (rec a))
    (match-define (IntT #f bw) (rec i))
    (define (closest-bitwidth some-bw)
      (for/or ([bw (in-list bitwidths)])
        (and (<= some-bw bw) bw)))
    (unless (= (closest-bitwidth (integer-length len)) bw)
      (error 'typec "Bitwidth of index is too large for array dimension"))
    vt]
   [(ArrS a s e)
    (match-define (ArrT len vt) (rec a))
    (unless (<= s e)
      (error 'typec "ArrS - Start must be before end"))
    (unless (<= s len)
      (error 'typec "ArrS - Start must be within range"))
    (unless (<= e len)
      (error 'typec "ArrS - End must be within range"))
    (ArrT (- e s) vt)]
   [(Call p ro-cpy ro-ref rw-ref)
    (match-define (ProcArr ret_p ro-cpy_p ro-ref_p rw-ref_p) (typec-proc p->t p))
    (define (compare kind rec tys es)
      (unless (= (length tys) (length es))
        (error 'typec "Argument count mismatch: ~a" kind))
      (for/list ([t (in-list tys)]
                 [e (in-list es)])
        (type= (format "call ~a arguments" kind) (rec e) t)))
    (compare 'ins rec ro-cpy_p ro-cpy)
    (define ((rec-lhs check-read-only?) l)
      (typec-lhs l
                 #:p->t p->t
                 #:var-env var-env
                 #:check-read-only? check-read-only?))
    (compare 'refs (rec-lhs #f) ro-ref_p ro-ref)
    (compare 'outs (rec-lhs #t) rw-ref_p rw-ref)
    ret_p]
   [(Cast t e)
    (define et (rec e))
    (unless (NumT? et)
      (error 'typec "May only cast numbers: ~v" et))
    t]
   [(Bin op l r)
    (define lt (rec l))
    (define rt (rec r))
    (type= "Bin" lt rt)
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
    (unless (Which? lt)
      (error 'typec "Wrong argument in Bin"))
    Result]))

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
    (error 'typec-lhs "XXX ArrayLHS: Code is basically same as typec-expr for ArrR, except that we need to check that the array is writeable, which is not currently available.")]
   [(RecordLHS r i)
    (error 'typec-lhs "XXX RecordLHS: Code is basically same as typec-expr for RecR, except that we need to check that the array is writeable, which is not currently available.")]))

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
    ;; xxx e should be pure?
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
   [(Loop lab ty idx end end-e b)
    (typec-int ty end)
    (type= "Loop dynamic end" (rec-e end-e) ty)
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
     (match-define (Proc ret ro-cpy ro-ref rw-ref body) p)
     (define (add-vars to l writeable?)
       (for/fold ([σ to]) ([i (in-list l)])
         (match-define (cons ii it) i)
         (when (hash-has-key? σ ii)
           (error 'typec-proc "Duplicate argument: ~e" ii))
         (hash-set σ ii (cons it writeable?))))
     (define var-env0 (add-vars (hasheq) ro-cpy #t))
     (define var-env1 (add-vars var-env0 ro-ref #f))
     (define var-env2 (add-vars var-env1 rw-ref #t))
     (and
      (typec-stmt body
                  #:p->t p->t
                  #:must-return? #t
                  #:return-ty ret
                  #:var-env var-env2
                  #:label-set (seteq))
      (ProcArr ret (map cdr ro-cpy) (map cdr ro-ref) (map cdr rw-ref))))))
(define (typec-main m)
  (define p->t (make-hasheq))
  (define mt (typec-proc p->t m))
  (match mt
    [(ProcArr (== U8) (list) (list) (list))
     #t]
    [_
     (error 'main-type "Invalid main function(~e) returns (~e)" m mt)]))

;; Interpreter
(struct *svector (vec s e) #:transparent)
(define (svector l)
  (define vec (list->vector l))
  (*svector vec 0 (vector-length vec)))
(define (svector-dec sv i)
  (match-define (*svector vec s e) sv)
  (unless (< i e)
    (error 'svector-ref "Out of bounds access"))
  (values vec (+ s i)))
(define (svector-ref sv i)
  (define-values (vec idx) (svector-dec sv i))
  (vector-ref vec idx))
(define (svector-set! sv i nv)
  (define-values (vec idx) (svector-dec sv i))
  (vector-ref vec idx nv))
(define (svector-slice sv ns ne)
  (match-define (*svector vec s e) sv)
  (*svector vec (+ s ns) ne))

(struct lhs-value (reader writer))
(define (lhs-read lv)
  ((lhs-value-reader lv)))
(define (lhs-write! lv nv)
  ((lhs-value-writer lv) nv))

(define (eval-primitive op lv rv)
  ((match op
     ['ieq =]
     ['islt <])
   lv rv))

(define (eval-expr σ e)
  (define (rec e) (eval-expr σ e))
  (define (rec-lhs l) (lhs-read (eval-lhs σ l)))
  (match-type
   Expr e
   [(VarR x) (unbox (hash-ref σ x))]
   [(IntV _ i) i]
   [(FloV _ f) f]
   [(RecV lofe)
    (define r (make-hasheq))
    (for ([fe (in-list lofe)])
      (match-define (cons f e) fe)
      (hash-set! r f (rec e)))
    r]
   [(RecR r f)
    (hash-ref (rec r) f)]
   [(ArrV vs)
    (svector (map rec vs))]
   [(ArrR a i)
    (svector-ref (rec a) (rec i))]
   [(ArrS a s e)
    (svector-slice (rec a) s e)]
   [(Call p ro-cpy ro-ref rw-ref)
    (eval-proc p (map rec ro-cpy) (map rec-lhs ro-ref) (map rec-lhs rw-ref))]
   [(Cast t e)
    (define conv
      (match t
        [(IntT sign? bw)
         (λ (v)
           (modulo (inexact->exact v)
                   (expt 2 (if sign? (sub1 bw) bw))))]
        [(FloT 32) real->single-flonum]
        [(FloT 64) real->double-flonum]))
    (conv (rec e))]
   [(Bin op l r)
    (eval-primitive op (rec l) (rec r))]))

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
    (lhs-value (λ () (svector-ref av iv))
               (λ (nv) (svector-set! av iv nv)))]
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
    (rec f) ;; <--- Any 0-valued return is to here here
    (rec r)]
   [(If c t f)
    (if (eval-expr var-env c)
      (rec t)
      (rec f))]
   [(Loop lab _ i e ee b)
    (let/ec break
      (for ([ci (in-range (min (eval-expr var-env ee) e))])
        (let/ec continue
          (rec #:var-env (hash-set var-env i (box ci))
               #:label-env (hash-set label-env lab (cons break continue))
               b))))]
   ;; The type-checker has ensured that these 0-valued returns are
   ;; okay, because of the must-return? part
   [(Break lab)
    (match-define (cons b c) (hash-ref label-env lab))
    (b)]
   [(Continue lab)
    (match-define (cons b c) (hash-ref label-env lab))
    (c)]))

(define (eval-proc p ro-cpyv ro-refv rw-refv)
  (match-define (Proc _ ro-cpy ro-ref rw-ref b) p)
  (define (add-vars to ids vs)
    (for/fold ([σ to]) ([i (in-list ids)] [iv (in-list vs)])
      (match-define (cons ii it) i)
      (hash-set σ ii (box iv))))
  (define var-env0 (add-vars (hasheq) ro-cpy ro-cpyv))
  (define var-env1 (add-vars var-env0 ro-ref ro-refv))
  (define var-env2 (add-vars var-env1 rw-ref rw-refv))
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
           (Loop 'main U8 'i N (IntV U8 N)
                 (If (Bin 'ieq (VarR 'i) (VarR 'x))
                     (Return (Cast S8 (VarR 'i)))
                     (Continue 'main)))
           (Return (IntV S8 -1)))))
  (define Main
    (Proc U8 (list) (list) (list)
          (Let #t 'a (ArrV (for/list ([i (in-range N)])
                             (IntV U8 i)))
               (Let #t 'res (Call LinearSearch
                                  (list (IntV U8 5))
                                  (list (VarLHS 'a))
                                  (list))
                    (If (Bin 'islt (VarR 'res) (IntV S8 0))
                        (Return (IntV U8 1))
                        (Return (IntV U8 0)))))))
  (adqc-eval Main))

;; xxx make NES synth example

;; xxx compute space and time

;; xxx liveness analysis of variables to compute how to re-use space

;; xxx MIC-1 simulator/assembler/microcompiler
