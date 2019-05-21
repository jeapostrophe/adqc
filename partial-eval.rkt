#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/contract/base
         racket/match
         racket/require
         (subtract-in "ast.rkt" "type.rkt")
         "type.rkt"
         "eval.rkt")

(define const? (or/c Int? Flo?))

(define current-partial-eval? (make-parameter #t))

(define-match-expander BinOp^
  (λ (stx)
    (syntax-parse stx
      [(_ args:expr ...)
       (syntax/loc stx (BinOp args ...))]))
  (λ (stx)
    (syntax-parse stx
      [me:id
       (quasisyntax/loc stx
         (contract
          (-> symbol? Expr? Expr? (or/c BinOp? MetaE? const?))
          (λ (op the-lhs the-rhs)
            (define the-binop (BinOp op the-lhs the-rhs))
            (define eval? (current-partial-eval?))
            (cond [(and eval? (const? the-lhs) (const? the-rhs))
                   (eval-expr #hasheq() the-binop)]
                  [else the-binop]))
          (syntax-source #'BinOp) #'#,stx 'me #'BinOp))]
      [(me:id . args)
       (syntax/loc stx (#%app me . args))])))

#|(define-match-expander Cast^
  (λ (stx)
    (syntax-parse stx
      [(_ args:expr ...)
       (syntax/loc stx (Cast args ...))]))
  (λ (stx)
    (syntax-parse stx
      [me:id
       (quasisyntax/loc stx
         (contract
          (value-contract Cast)
          (λ (ty the-e)
            (define the-cast (Cast ty the-e))
            (|#

(provide
 current-partial-eval?
 (rename-out [BinOp^ BinOp]))
