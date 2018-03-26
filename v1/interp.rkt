#lang racket/base
(require racket/match
         "typec.rkt"
         "ast.rkt")

;; xxx
(provide adqc-eval)

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
   [(VarR _ x) (unbox (hash-ref σ x))]
   [(IntV _l _ i) i]
   [(FloV _l _ f) f]
   [(RecV _ lofe)
    (define r (make-hasheq))
    (for ([fe (in-list lofe)])
      (match-define (cons f e) fe)
      (hash-set! r f (rec e)))
    r]
   [(RecR _ r f)
    (hash-ref (rec r) f)]
   [(ArrV _ vs)
    (svector (map rec vs))]
   [(ArrR _ a i)
    (svector-ref (rec a) (rec i))]
   [(ArrS _ a s e)
    (svector-slice (rec a) s e)]
   [(Call _ p ro-cpy ro-ref rw-ref)
    (eval-proc p (map rec ro-cpy) (map rec-lhs ro-ref) (map rec-lhs rw-ref))]
   [(Cast _ t e)
    (define conv
      (match t
        [(IntT sign? bw)
         (λ (v)
           (modulo (inexact->exact v)
                   (expt 2 (if sign? (sub1 bw) bw))))]
        [(FloT 32) real->single-flonum]
        [(FloT 64) real->double-flonum]))
    (conv (rec e))]
   [(Bin _ op l r)
    (eval-primitive op (rec l) (rec r))]))

(define (eval-lhs σ l)
  (match-type
   LHS l
   [(VarLHS _ x)
    (define xb (hash-ref σ x))
    (lhs-value (λ () (unbox xb))
               (λ (nv) (set-box! xb nv)))]
   [(ArrayLHS _ a i)
    (define av (eval-expr σ a))
    (define iv (eval-expr σ i))
    (lhs-value (λ () (svector-ref av iv))
               (λ (nv) (svector-set! av iv nv)))]
   [(RecordLHS _ r f)
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
   [(Assert _ e)
    (unless (eval-expr var-env e)
      (error 'eval "Assert failed: ~v" e))]
   [(Assign _ l e)
    (define lv (eval-lhs var-env l))
    (define nv (eval-expr var-env e))
    (lhs-write! lv nv)]
   [(Return _ e)
    (return (eval-expr var-env e))]
   [(Let _l _ x e b)
    (rec #:var-env (hash-set var-env x (box (eval-expr var-env e)))
         b)]
   [(Seq _ f r)
    (rec f) ;; <--- Any 0-valued return is to here here
    (rec r)]
   [(If _ c t f)
    (if (eval-expr var-env c)
      (rec t)
      (rec f))]
   [(Loop _l lab _ i e ee b)
    (let/ec break
      (for ([ci (in-range (min (eval-expr var-env ee) e))])
        (let/ec continue
          (rec #:var-env (hash-set var-env i (box ci))
               #:label-env (hash-set label-env lab (cons break continue))
               b))))]
   ;; The type-checker has ensured that these 0-valued returns are
   ;; okay, because of the must-return? part
   [(Break _ lab)
    (match-define (cons b c) (hash-ref label-env lab))
    (b)]
   [(Continue _ lab)
    (match-define (cons b c) (hash-ref label-env lab))
    (c)]))

(define (eval-proc p ro-cpyv ro-refv rw-refv)
  (match-define (Proc _ _ ro-cpy ro-ref rw-ref b) p)
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
