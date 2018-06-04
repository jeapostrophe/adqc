#lang racket/base
(require racket/contract/base
         racket/match
         "ast.rkt"
         "stx.rkt")

(define (weakest-precond stmt post-cond)
  (match stmt
    [(Skip) post-cond]
    [(Fail _) (U32 0)]
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
    [(or (? Var?) (? Integer?)) e]
    [(IBinOp op L R)
     (IBinOp op (rec L) (rec R))]))

