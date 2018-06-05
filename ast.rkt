#lang racket/base
(require racket/contract/base)

(define float-bit-widths '(32 64))
(define integer-bit-widths '(8 16 32 64))

;; Types
(struct Type () #:transparent)
(struct IntT Type (signed? bits) #:transparent)
(struct FloT Type (bits) #:transparent)
;; XXX Array
;; XXX Struct
;; XXX Union
;; XXX Opaque (from C)

;; Path
;; XXX Var
;; XXX Array
;; XXX Struct
;; XXX Union
;; XXX CVar

;; Expressions
(struct Expr () #:transparent)
;; XXX Change Var Expr to Read Expr and take a Path
(struct Var Expr (name ty) #:transparent)
(struct Int Expr (signed? bits val) #:transparent)
(struct Flo Expr (bits val) #:transparent)
(struct BinOp Expr (op L R) #:transparent)
;; XXX Let (pure substitution)
;; XXX If (ternary operator)

;; Statements
(struct Stmt () #:transparent)
(struct Skip Stmt () #:transparent)
(struct Fail Stmt (msg) #:transparent)
(struct Begin Stmt (f s) #:transparent)
;; XXX Change x to a path
(struct Assign Stmt (x e) #:transparent)
(struct If Stmt (p t f) #:transparent)
(struct While Stmt (p I body) #:transparent)
;; XXX Is Throw or Goto a better name?
(struct Return Stmt (label) #:transparent)
(struct Let/ec Stmt (label body) #:transparent)
;; XXX Delete Assert and add a (Static E) to Expr that says the
;; expression must be provably constant, like lancet's frozen, then
;; this can be made unto something entirely in a library
(struct Assert Stmt (must-be-static? p msg) #:transparent)
;; XXX Let
;; XXX CCall
;; XXX Specif (insert a particular pre/post condition pair that obscures the body?)

;; Program
;; XXX May add a constructor for a program that records what the open
;; variable's types are, how to get arguments (copy vs reference), and
;; how to return the result? Perhaps it can set up an initial Return
;; label

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
               ;; XXX Should be constrained to correct range here?
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
