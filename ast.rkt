#lang racket/base
(require racket/contract/base)

(define float-bit-widths '(32 64))
(define integer-bit-widths '(8 16 32 64))

;; Types
(struct Type () #:transparent)
(struct IntT Type (signed? bits) #:transparent)
(struct FloT Type (bits) #:transparent)

;; Expressions
(struct Expr () #:transparent)
(struct Var Expr (name ty) #:transparent)
(struct Int Expr (signed? bits val) #:transparent)
(struct Flo Expr (bits val) #:transparent)
(struct BinOp Expr (op L R) #:transparent)

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
  [struct Type ()]
  [struct IntT ([signed? boolean?]
                [bits (apply or/c integer-bit-widths)])]
  [struct FloT ([bits (apply or/c float-bit-widths)])]

  [struct Expr ()]
  [struct Var ([name symbol?] [ty Type?])]
  [struct BinOp ([op symbol?] [L Expr?] [R Expr?])]
  [struct Int ([signed? boolean?]
               [bits (apply or/c integer-bit-widths)]
               [val exact-integer?])]
  [struct Flo ([bits (apply or/c float-bit-widths)]
               ;; XXX should for Flo32 to be single-flonum?
               [val flonum?])]

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
