#lang racket/base
(require racket/contract/base)

;; Expressions
(struct Expr () #:transparent)

(define Integer-bit-width? (or/c 8 16 32 64))
(struct Integer Expr (signed? bits val) #:transparent)
;; TODO needs type info
(struct Var Expr (name) #:transparent)
(struct IBinOp Expr (op L R) #:transparent)

;; Statements
(struct Stmt () #:transparent)
(struct Skip Stmt () #:transparent)
(struct Fail Stmt (msg) #:transparent)
(struct Begin Stmt (f s) #:transparent)
(struct Assign Stmt (x e) #:transparent)
(struct If Stmt (p t f) #:transparent)
(struct While Stmt (p I body) #:transparent)
(struct Return Stmt (label) #:transparent)
(struct Let/ec Stmt (label body) #:transparent)
(struct Assert Stmt (must-be-static? p msg) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [struct Expr ()]
  [struct IBinOp ([op symbol?] [L Expr?] [R Expr?])]
  [struct Integer ([signed? boolean?]
                   [bits Integer-bit-width?]
                   [val exact-integer?])]
  [struct Var ([name symbol?])]

  [struct Stmt ()]
  [struct Skip ()]
  [struct Fail ([msg string?])]
  [struct Begin ([f Stmt?] [s Stmt?])]
  [struct Assign ([x Var?] [e Expr?])]
  [struct If ([p Expr?] [t Stmt?] [f Stmt?])]
  [struct While ([p Expr?] [I Expr?] [body Stmt?])]
  [struct Return ([label symbol?])]
  [struct Let/ec ([label symbol?] [body Stmt?])]
  [struct Assert ([must-be-static? boolean?] [p Expr?] [msg string?])]))
