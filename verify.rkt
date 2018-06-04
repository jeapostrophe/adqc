#lang racket/base
(require racket/contract/base
         racket/match
         "ast.rkt"
         "stx.rkt")

(define (weakest-precond stmt post-cond)
  (match stmt
    [(Skip) post-cond]
    [(Fail _) (U32 0)]
    [(Assign (Var x _) e)
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
    [(Var (== x) _) v]
    [(or (? Var?) (? Int?) (? Flo?)) e]
    [(BinOp op L R)
     (BinOp op (rec L) (rec R))]))

;; XXX strongest postcondition
;;
;; How to deal with Return? I think you compute the post-condition as
;; you go and the recursive call gives you the main post-cond, plus a
;; list of post-conds for each label and you OR them together.

;; XXX verify! function that compiler needs
