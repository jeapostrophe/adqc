#lang racket/base
(require racket/contract/base
         racket/match
         "ast.rkt")

(struct type-info (ty) #:transparent)

(define (expr-ty e)
  (match e
    [(MetaE (type-info ty) _) ty]
    [(MetaE _ e) (expr-ty e)]
    [(Int signed? bits _) (IntT signed? bits)]
    [(Flo bits _) (FloT bits)]
    [(Cast ty _) ty]
    [(Read p) (path-ty p)]
    [(BinOp _ L R)
     (define L-ty (expr-ty L))
     (define R-ty (expr-ty R))
     (unless (equal? L-ty R-ty)
       (error 'expr-ty "BinOp: LHS and RHS types not equal"))
     L-ty]
    [(LetE _ ty xe be)
     (unless (equal? ty (expr-ty xe))
       (error 'expr-ty "LetE: x and xe types not equal"))
     (expr-ty be)]
    [(IfE ce te fe)
     (unless (IntT? (expr-ty ce))
       (error 'expr-ty "IfE: predicate type not integral"))
     (define te-ty (expr-ty te))
     (define fe-ty (expr-ty fe))
     (unless (equal? te-ty fe-ty)
       (error 'expr-ty "IfE: true and false types not equal"))
     te-ty]))

(define (path-ty p)
  (match p
    [(Var _ ty) ty]
    [(ExtVar _ _ ty) ty]
    [(Select p ie)
     (unless (IntT? (expr-ty ie))
       (error 'path-ty "Select: index type not integral"))
     (match-define (ArrT _ ety) (path-ty p))
     ety]
    [(Field p f)
     (match-define (RecT f->ty _ _) (path-ty p))
     (hash-ref f->ty f)]
    [(Mode p m)
     (match-define (UniT m->ty _) (path-ty p))
     (hash-ref m->ty m)]))

;; Typed expressions constructors
(define (Int^ signed? bits val)
  (MetaE (type-info (IntT signed? bits))
         (Int signed? bits val)))
(define (Flo^ bits val)
  (MetaE (type-info (FloT bits))
         (Flo bits val)))
(define (Cast^ ty e)
  (MetaE (type-info ty)
         (Cast ty e)))
(define (Read^ p)
  (MetaE (type-info (path-ty p))
         (Read p)))
(define (BinOp^ op L R)
  (define bin-op (BinOp op L R))
  (MetaE (type-info (expr-ty bin-op))
         bin-op))
(define (LetE^ x ty xe be)
  (define let-e (LetE x ty xe be))
  (MetaE (type-info (expr-ty let-e))
         let-e))
(define (IfE^ ce te fe)
  (define if-e (IfE ce te fe))
  (MetaE (type-info (expr-ty if-e))
         if-e))

;; XXX Should Stmts have types? It's hard to imagine what the type of certain
;; kinds of statements would be. E.g., what's the type of a Skip? Should we
;; still make smart-constructors that check for internal coherency?

;; Typed function constructors
(define (IntFun^ args ret-x ret-ty ret-lab body)
  (MetaFun (type-info ret-ty)
           (IntFun args ret-x ret-ty ret-lab body)))
(define (ExtFun^ src args ret-ty name)
  (MetaFun (type-info ret-ty)
           (ExtFun src args ret-ty name)))

;; XXX How to provide? Do we want to expose the the typed-constructors using
;; their internal names (with the hat symbols), or is it better to shadow the
;; untyped constructors?