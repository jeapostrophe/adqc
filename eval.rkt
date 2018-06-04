#lang racket/base
(require racket/contract/base
         racket/contract/region
         racket/match
         syntax/parse/define
         "ast.rkt")

(define/contract (arithmetic-shift-left n m)
  (exact-integer? exact-nonnegative-integer? . -> . exact-integer?)
  (arithmetic-shift n m))

(define/contract (arithmetic-shift-right n m)
  (exact-integer? exact-nonnegative-integer? . -> . exact-integer?)
  (arithmetic-shift n (- m)))

;; For now, assume 64 bits of storage for unsigned values.
;; TODO: Should signed values also be restricted to 64-bits?
;; Right now, they are basically BigInts and can grow very
;; large or small.
(define 2^64 (expt 2 64))

(define (unsigned-quotient a b)
  (modulo (quotient a b) 2^64))

(define (unsigned-remainder a b)
  (modulo (remainder a b) 2^64))

(define ((bin-op op) a b)
  (match-define (Integer a-signed? a-bits a-val) a)
  (match-define (Integer b-signed? b-bits b-val) b)
  (unless (equal? a-signed? b-signed?)
    (error "Mismatched signs" a b))
  (unless (= a-bits b-bits)
    (error "Mismatched bit widths" a b))
  (Integer a-signed? a-bits (op a-val b-val)))

(define (bool->c b)
  (if b 1 0))

(define-simple-macro (make-bin-op-table ([arith-name arith-op] ...)
                                        ([cmp-name cmp-op] ...))
  ;; TODO: better way of forwarding macro args to hasheq?
  (make-immutable-hasheq
   (append
    (list (cons arith-name (bin-op arith-op)) ...)
    (list (cons cmp-name (bin-op (λ (a b) (bool->c (cmp-op a b))))) ...))))

(define bin-op-table
  (make-bin-op-table
   ;; Binary arithmetic
   (['iadd +]
    ['isub -]
    ['imul *]
    ['iudiv unsigned-quotient]
    ['isdiv quotient]
    ['iurem unsigned-remainder]
    ['isrem remainder]
    ['ishl arithmetic-shift-left]
    ; 'ilshr - logical rshift
    ['iashr arithmetic-shift-right]
    ['ior bitwise-ior]
    ['iand bitwise-and]
    ['ixor bitwise-xor])
   ;; Binary comparisons
   (['ieq =]
    ['ine (λ (a b) (not (= a b)))]
    ;; TODO: Should we keep separate signed and unsigned ops in the table if
    ;; signed-ness is part of the integer type? Right now unsigned ops are
    ;; pretty much broken since they just modulo their arguments by 2^64.
    ;; Going forward ops should probably infer sign from arguments, then modulo
    ;; the result depending on signed-ness of arguments to simulate
    ;; overflow/underflow.
    ['iugt (unsigned-cmp >)]
    ['iuge (unsigned-cmp >=)]
    ['iult (unsigned-cmp <)]
    ['iule (unsigned-cmp <=)]
    ['isgt >]
    ['isge >=]
    ['islt <]
    ['isle <=])))

(define ((unsigned-cmp op) a b)
  (op (modulo a 2^64)
      (modulo b 2^64)))

(define (eval-expr σ e)
  (define (rec e) (eval-expr σ e))
  (match e
    [(Var x)
     (hash-ref σ x)]
    [(Integer signed? bits val)
     e]
    [(IBinOp op L R)
     ((hash-ref bin-op-table op)
      (rec L) (rec R))]))

(define (eval-expr-pred σ pred)
  (not (zero? (Integer-val (eval-expr σ pred)))))

(define (eval-stmt γ σ s)
  (match s
    [(Skip) σ]
    [(Assign x e)
     (hash-set σ x (eval-expr σ e))]
    [(Begin f s)
     (eval-stmt γ (eval-stmt γ σ f) s)]
    [(If p t f)
     (eval-stmt γ σ (if (eval-expr-pred σ p) t f))]
    [(While p _ b)
     (cond [(eval-expr-pred σ p)
            (eval-stmt γ (eval-stmt γ σ b) s)]
           [else σ])]
    [(Return l)
     ((hash-ref γ l) σ)]
    [(Let/ec l b)
     (let/ec this-return
       (eval-stmt (hash-set γ l this-return) σ b))]
    [(Assert _ p msg)
     (or (and (eval-expr-pred σ p) σ)
         (error 'Assert "Failed assertion: ~e" msg))]))

(define (eval-stmt* s)
  (eval-stmt (hasheq) (hasheq) s))

(define (weakest-precond stmt post-cond)
  (match stmt
    [(Skip) post-cond]
    [(Assign (Var x) e)
     (subst x e post-cond)]
    [(Begin L-stmt R-stmt)
     (define post-cond* (weakest-precond R-stmt post-cond))
     (weakest-precond L-stmt post-cond*)]
    [(If pred then else)
     (And (Implies pred
                   (weakest-precond else post-cond))
          (Implies (Not pred)
                   (weakest-precond then post-cond)))]
    [(While pred invar do-stmt)
     (And invar
          (And (Implies (And pred invar)
                        (weakest-precond do-stmt invar))
               (Implies (And (Not pred) invar)
                        post-cond)))]
    [(Return label)
     (Var label)]
    [(Let/ec label stmt)
     (subst label post-cond (weakest-precond stmt post-cond))]
    [(Assert _ p _)
     (And p post-cond)]))

(define (subst x v e)
  (define (rec e) (subst x v e))
  (match e
    [(Var (== x)) v]
    [(? (or/c Var? Integer?)) e]
    [(IBinOp op L R)
     (IBinOp op (rec L) (rec R))]))

(module+ test
  (require chk)
  ;; eval-expr
  (chk (eval-expr (hash) (S64 5)) (S64 5))
  (chk (eval-expr (hash 'x (S64 5)) (Var 'x)) (S64 5))
  (chk (eval-expr (hash) (IAdd (S64 5) (S64 6))) (S64 11))
  (chk (eval-expr (hash 'x (S64 5) 'y (S64 6)) (IAdd (Var 'x) (Var 'y))) (S64 11))
  (chk (eval-expr (hash) (ISub (S64 6) (S64 5))) (S64 1))
  (chk (eval-expr (hash) (IMul (S64 3) (S64 4))) (S64 12))
  (chk #:t (> (Integer-val (eval-expr (hash) (IUDiv (S64 10) (S64 -2)))) 0))
  (chk (eval-expr (hash) (ISDiv (S64 12) (S64 4))) (S64 3))
  (chk (eval-expr (hash) (ISDiv (S64 13) (S64 4))) (S64 3))
  ;; TODO: Unsigned remainder? What's the difference between signed/unsigned?
  (chk (eval-expr (hash) (ISRem (S64 12) (S64 5))) (S64 2))
  (chk (eval-expr (hash) (IShl (S64 2) (S64 1))) (S64 4))
  (chk #:x (eval-expr (hash) (IShl (S64 2) (S64 -1))) exn:fail:contract?)
  (chk (eval-expr (hash) (IAShr (S64 4) (S64 1))) (S64 2))
  (chk #:x (eval-expr (hash) IAShr (S64 4) (S64 -1)) exn:fail:contract?)
  (chk (eval-expr (hash) (IOr (S64 1) (S64 2))) (S64 3))
  (chk (eval-expr (hash) (IAnd (S64 3) (S64 1))) (S64 1))
  (chk (eval-expr (hash) (IXor (S64 3) (S64 2))) (S64 1))
  (chk (eval-expr (hash) (IEq (S64 1) (S64 1))) (S64 1))
  (chk (eval-expr (hash) (INe (S64 1) (S64 2))) (S64 1))
  ;; TODO: retyping unit tests is expensive...
  )

#;
(module+ test
  (require chk)
  ;; eval-expr
  (chk (eval-expr (hash) 5)
       5)
  (chk (eval-expr (hash 'x 5) 'x)
       5)
  (chk (eval-expr (hash) (IBinOp 'iadd 5 6))
       11)
  (chk (eval-expr (hash 'x 5 'y 6) (IBinOp 'iadd 'x 'y))
       11)
  (chk (eval-expr (hash) (IBinOp 'isub 6 5))
       1)
  (chk (eval-expr (hash) (IBinOp 'imul 3 4))
       12)
  (chk #:t (> (eval-expr (hash) (IBinOp 'iudiv 10 -2)) 0))
  (chk (eval-expr (hash) (IBinOp 'isdiv 12 4))
       3)
  (chk (eval-expr (hash) (IBinOp 'isdiv 13 4))
       3)
  ;; TODO: What's the actual difference between signed
  ;; and unsigned remainder?
  (chk (eval-expr (hash) (IBinOp 'isrem 12 5))
       2)
  (chk (eval-expr (hash) (IBinOp 'ishl 2 1))
       4)
  (chk #:x (eval-expr (hash) (IBinOp 'ishl 2 -1))
       exn:fail:contract?)
  (chk (eval-expr (hash) (IBinOp 'iashr 4 1))
       2)
  (chk #:x (eval-expr (hash) (IBinOp 'iashr 2 -1))
       exn:fail:contract?)
  (chk (eval-expr (hash) (IBinOp 'ior 2 1))
       3)
  (chk (eval-expr (hash) (IBinOp 'iand 3 1))
       1)
  (chk (eval-expr (hash) (IBinOp 'ixor 3 2))
       1)
  (chk (eval-expr (hash) (ICmp 'ieq 1 1))
       1)
  (chk (eval-expr (hash) (ICmp 'ieq 1 0))
       0)
  (chk (eval-expr (hash) (ICmp 'ine 1 1))
       0)
  (chk (eval-expr (hash) (ICmp 'ine 1 0))
       1)
  (chk (eval-expr (hash) (ICmp 'iugt -1 5))
       1)
  (chk (eval-expr (hash) (ICmp 'iuge -1 5))
       1)
  (chk (eval-expr (hash) (ICmp 'iult 5 -1))
       1)
  (chk (eval-expr (hash) (ICmp 'iule 5 -1))
       1)
  (chk (eval-expr (hash) (ICmp 'isgt 1 0))
       1)
  (chk (eval-expr (hash) (ICmp 'isgt 0 1))
       0)
  (chk (eval-expr (hash) (ICmp 'isge 1 0))
       1)
  (chk (eval-expr (hash) (ICmp 'isge 1 1))
       1)
  (chk (eval-expr (hash) (ICmp 'isge 0 1))
       0)
  (chk (eval-expr (hash) (ICmp 'islt 0 1))
       1)
  (chk (eval-expr (hash) (ICmp 'islt 1 0))
       0)
  (chk (eval-expr (hash) (ICmp 'isle 0 1))
       1)
  (chk (eval-expr (hash) (ICmp 'isle 1 1))
       1)
  (chk (eval-expr (hash) (ICmp 'isle 1 0))
       0)
  (chk (eval-expr (hash) (And (ICmp 'islt 2 3)
                              (ICmp 'isge 3 3)))
       1)
  (chk (eval-expr (hash) (And (ICmp 'ieq 1 0)
                              (ICmp 'ine 1 0)))
       0)
  (chk (eval-expr (hash) (Or (ICmp 'islt 1 0)
                             (ICmp 'ieq 1 1)))
       1)
  (chk (eval-expr (hash) (Or (ICmp 'ieq 1 0)
                             (ICmp 'islt 1 0)))
       0)

  ;; eval-stmt
  (chk (eval-stmt (hash 'x 1) (Skip))
       (hash 'x 1))
  (chk (eval-stmt (hash) (Assign 'x 5))
       (hash 'x 5))
  (chk (eval-stmt (hash) (Assign 'x (IBinOp 'iadd 5 6)))
       (hash 'x 11))
  (chk (eval-stmt (hash) (Begin (Assign 'x 1) (Assign 'y 2)))
       (hash 'x 1 'y 2))
  (chk (eval-stmt (hash) (If (ICmp 'ieq 0 0) (Assign 'x 1) (Assign 'y 2)))
       (hash 'x 1))
  (chk (eval-stmt (hash) (If (ICmp 'ieq 0 1) (Assign 'x 1) (Assign 'y 2)))
       (hash 'y 2))
  (chk (eval-stmt (hash 'x 0) (While (ICmp 'islt 'x 5)
                                     ;(ICmp 'isle 'x 5)
                                     (Assign 'x (IBinOp 'iadd 'x 1))))
       (hash 'x 5))
  (chk (eval-stmt (hash 'x 0) (While (ICmp 'islt 'x 5)
                                     ;(ICmp 'isle 'x 5)
                                     (Begin (Assign 'x (IBinOp 'iadd 'x 1))
                                            (Assign 'y (IBinOp 'iadd 'x 'x)))))
       (hash 'x 5 'y 10))

  ;;TODO: Tests for check-pred.
  )
