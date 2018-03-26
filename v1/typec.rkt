#lang racket/base
(require plai/datatype
         racket/match
         racket/set
         racket/contract/base
         racket/list
         "ast.rkt")

;; xxx
(provide typec-main)

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
   [(VarR _ x)
    (match (hash-ref var-env x #f)
      [(cons xt _) xt]
      [#f (error 'typec "Unbound variable: ~v" x)])]
   [(IntV _ ty i)
    (typec-int ty i)
    ty]
   [(FloV _ ty f)
    (typec-float ty f)
    ty]
   [(RecV _ lofe)
    ;; FIXME Should find all the duplicates
    (match (check-duplicates (map car lofe))
      [#f (void)]
      [dupe (error 'typec "Duplicated field in record: ~v" dupe)])
    (RecT
     (for/list ([fe (in-list lofe)])
       (match-define (cons f e) fe)
       (cons f (rec e))))]
   [(RecR _ r rf)
    (match-define (RecT loft) (rec r))
    (for/or ([ft (in-list loft)])
      (match-define (cons f t) ft)
      (and (eq? rf f)
           t))]
   [(ArrV _ vs)
    (define len (length vs))
    (when (zero? len)
      (error 'typec "Array must have non-zero length"))
    (match-define (cons v0 vn) vs)
    (define v0t (rec v0))
    (for/and ([vi (in-list vn)])
      (type= "homogeneous array elements" (rec vi) v0t))
    (ArrT len v0t)]
   [(ArrR _ a i)
    (match-define (ArrT len vt) (rec a))
    (match-define (IntT #f bw) (rec i))
    (define (closest-bitwidth some-bw)
      (for/or ([bw (in-list bitwidths)])
        (and (<= some-bw bw) bw)))
    (unless (= (closest-bitwidth (integer-length len)) bw)
      (error 'typec "Bitwidth of index is too large for array dimension"))
    vt]
   [(ArrS _ a s e)
    (match-define (ArrT len vt) (rec a))
    (unless (<= s e)
      (error 'typec "ArrS - Start must be before end"))
    (unless (<= s len)
      (error 'typec "ArrS - Start must be within range"))
    (unless (<= e len)
      (error 'typec "ArrS - End must be within range"))
    (ArrT (- e s) vt)]
   [(Call _ p ro-cpy ro-ref rw-ref)
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
   [(Cast _ t e)
    (define et (rec e))
    (unless (NumT? et)
      (error 'typec "May only cast numbers: ~v" et))
    t]
   [(Bin _ op l r)
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
   [(VarLHS _ x)
    (match (hash-ref var-env x #f)
      [(cons xt writeable?)
       (unless writeable?
         (when check-read-only?
           (error 'typec-lhs "~e is read-only" x)))
       xt]
      [_ (error 'typec-lhs "Unbound variable: ~v" x)])]
   [(ArrayLHS _ a i)
    (error 'typec-lhs "XXX ArrayLHS: Code is basically same as typec-expr for ArrR, except that we need to check that the array is writeable, which is not currently available.")]
   [(RecordLHS _ r i)
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
   [(Assert _ e)
    ;; xxx e should be pure?
    (type= "Assert" (rec-e e) Bool)
    (doesnt-return!)]
   [(Assign _ l e)
    (define lt
      (typec-lhs l
                 #:p->t p->t
                 #:var-env var-env
                 #:check-read-only? #t))
    (define et (rec-e e))
    (type= "Assign" lt et)
    (doesnt-return!)]
   [(Return _ e)
    (type= "Return" (rec-e e) return-ty)]
   [(Let _ read-only? x e b)
    (define et (rec-e e))
    ;; xxx x MUST be read in b
    (rec b #:var-env (hash-set var-env x (cons et (not read-only?))))]
   [(Seq _ f r)
    (and
     (rec f #:must-return? #f)
     (rec r))]
   [(If _ c t e)
    (type= "If Condition" (rec-e c) Bool)
    (and (rec t) (rec e))]
   [(Loop _ lab ty idx end end-e b)
    (typec-int ty end)
    (type= "Loop dynamic end" (rec-e end-e) ty)
    (rec b
         #:var-env (hash-set var-env idx (cons ty #f))
         #:label-set (set-add label-set lab))]
   [(Break _ lab)
    (unless (set-member? label-set lab)
      (error 'typec-stmt "Unknown label: ~v" lab))
    (doesnt-return!)]
   [(Continue _ lab)
    (unless (set-member? label-set lab)
      (error 'typec-stmt "Unknown label: ~v" lab))
    (doesnt-return!)]))
(define (typec-proc p->t p)
  (hash-ref!
   p->t p
   (λ ()
     (match-define (Proc _ ret ro-cpy ro-ref rw-ref body) p)
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

