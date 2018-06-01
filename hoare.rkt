#lang racket/base
(require racket/contract/base
         racket/contract/region
         racket/match
         "grammar.rkt")


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

(define bin-op-table
  (hasheq 'iadd +
          'isub -
          'imul *
          'iudiv unsigned-quotient
          'isdiv quotient
          'iurem unsigned-remainder
          'isrem remainder
          'ishl arithmetic-shift-left
          ; 'ilshr - logical rshift
          'iashr arithmetic-shift-right
          'ior bitwise-ior
          'iand bitwise-and
          'ixor bitwise-xor
          ))          

(define ((unsigned-cmp op) a b)
  (op (modulo a 2^64)
      (modulo b 2^64)))

(define (c-and a b)
  (if (not (zero? a))
      (not (zero? b))
      #f))

(define (c-or a b)
  (let ([tmp (not (zero? a))])
    (if tmp tmp (not (zero? b)))))

(define cmp-table
  (hasheq 'ieq =
          'ine (λ (a b) (not (= a b)))
          'iugt (unsigned-cmp >)
          'iuge (unsigned-cmp >=)
          'iult (unsigned-cmp <)
          'iule (unsigned-cmp <=)
          'isgt >
          'isge >=
          'islt <
          'isle <=
          'iand c-and
          'ior c-or
          ))

(define (bool->c b)
  (if b 1 0))

;; V = nat | bool
;; A = [X -> V] ...

;; A x E -> V
(define (eval-expr env exp)
  (define (recur exp*)
    (eval-expr env exp*))
  (match exp
    ;; TODO: Use struct instead of raw symbol for variable names?
    ;;       Definitely, we're going to need type info
    [(? symbol?)
     (hash-ref env exp)]
    ;; TODO: Should eval-expr return Racket numbers or Integer structs?
    [(Integer signed? bits val)
     val]
    [(IBinOp op L R)
     (define op-fn (hash-ref bin-op-table op))
     (op-fn (recur L) (recur R))]
    [(ICmp op L R)
     (define op-fn (hash-ref cmp-table op))
     (bool->c (op-fn (recur L) (recur R)))]))


;; A x S -> A
(define (eval-stmt env stmt)
  (define (recur stmt*)
    (eval-stmt env stmt*))
  (match stmt
    ;; Skip
    [(Skip) env]
    ;; Assign
    [(Assign dest exp)
     (define new-val (eval-expr env exp))
     (hash-set env dest new-val)]
    ;; Begin
    [(Begin L-stmt R-stmt)
     (define env* (recur L-stmt))
     (eval-stmt env* R-stmt)]
    ;; If
    [(If pred then else)
     (if (check-pred env pred)
         (recur then)
         (recur else))]
    ;; While
    [(While pred _ do-stmt)
     (cond [(check-pred env pred) 
            (define new-env (recur do-stmt))
            (eval-stmt new-env stmt)]
           [else env])]))
     

;; A x P -> ? (T/F)
(define (check-pred env pred)
  (not (zero? (eval-expr env pred))))


;; S x P -> P (weakest precondition)
(define (weakest-precond stmt post-cond)
  (match stmt
    ;; Skip
    [(Skip) post-cond]
    ;; Assign
    [(Assign dest exp)
     (subst post-cond dest exp)]
    ;; Begin
    [(Begin L-stmt R-stmt)
     (define post-cond* (weakest-precond R-stmt post-cond))
     (weakest-precond L-stmt post-cond*)]
    ;; If
    [(If pred then else)
     (And (Implies pred
                   (weakest-precond else post-cond))
          (Implies (Not pred)
                   (weakest-precond then post-cond)))]
    ;; While (weakest *liberal* pre-condition)
    [(While pred invar do-stmt)
     (And invar
          (And (Implies (And pred invar)
                        (weakest-precond do-stmt invar))
               (Implies (And (Not pred) invar)
                        post-cond)))]))

(define (subst start-exp remv-exp subst-exp)
  (define (recur start-exp*)
    (subst start-exp* remv-exp subst-exp))
  (define (eq-remv-exp? e)
    (equal? e remv-exp))
  (match start-exp
    ;; Will substitute any exp that is equal? to remv-exp with subst-exp
    [(? eq-remv-exp?) subst-exp]
    [(? (or/c number? symbol?)) start-exp]
    [(IBinOp op L R)
     (IBinOp op (recur L) (recur R))]
    [(ICmp op L R)
     (ICmp op (recur L) (recur R))]))

(module+ test
  (require chk)
  ;; eval-expr
  (chk (eval-expr (hash) (i64 5)) 5)
  (chk (eval-expr (hash 'x 5) 'x) 5)
  (chk (eval-expr (hash) (IAdd (i64 5) (i64 6))) 11)
  (chk (eval-expr (hash 'x 5 'y 6) (IAdd 'x 'y)) 11)
  (chk (eval-expr (hash) (ISub (i64 6) (i64 5))) 1)
  (chk (eval-expr (hash) (IMul (i64 3) (i64 4))) 12)
  (chk #:t (> (eval-expr (hash) (IUDiv (i64 10) (i64 -2))) 0))
  (chk (eval-expr (hash) (ISDiv (i64 12) (i64 4))) 3)
  (chk (eval-expr (hash) (ISDiv (i64 13) (i64 4))) 3)
  ;; TODO: Unsigned remainder? What's the difference between signed/unsigned?
  (chk (eval-expr (hash) (ISRem (i64 12) (i64 5))) 2)
  (chk (eval-expr (hash) (IShl (i64 2) (i64 1))) 4)
  (chk #:x (eval-expr (hash) (IShl (i64 2) (i64 -1))) exn:fail:contract?)
  (chk (eval-expr (hash) (IAShr (i64 4) (i64 1))) 2)
  (chk #:x (eval-expr (hash) IAShr (i64 4) (i64 -1)) exn:fail:contract?)
  (chk (eval-expr (hash) (IOr (i64 1) (i64 2))) 3)
  (chk (eval-expr (hash) (IAnd (i64 3) (i64 1))) 1)
  (chk (eval-expr (hash) (IXor (i64 3) (i64 2))) 1)
  (chk (eval-expr (hash) (IEq (i64 1) (i64 1))) 1)
  (chk (eval-expr (hash) (INe (i64 1) (i64 1))) 1)
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
