#lang racket/base
(require racket/contract/base)

(define float-bit-widths '(32 64))
(define integer-bit-widths '(8 16 32 64))

;; Extern Source
(struct ExternSrc (ls hs) #:transparent)

(provide
 (contract-out
  [struct ExternSrc ([ls (listof string?)] [hs (listof string?)])]))

;; Types
(struct Type () #:transparent)
(struct IntT Type (signed? bits) #:transparent)
(struct FloT Type (bits) #:transparent)
(struct ArrT Type (dim ety) #:transparent)
(struct RecT Type (field->ty) #:transparent)
(struct UniT Type (mode->ty) #:transparent)
(struct ExtT Type (src name) #:transparent)

(provide
 (contract-out
  [struct Type ()]
  [struct IntT ([signed? boolean?]
                [bits (apply or/c integer-bit-widths)])]
  [struct FloT ([bits (apply or/c float-bit-widths)])]
  [struct ArrT ([dim exact-nonnegative-integer?] [ety Type?])]
  [struct RecT ([field->ty (hash/c symbol? Type?)])]
  [struct UniT ([mode->ty (hash/c symbol? Type?)])]
  [struct ExtT ([src ExternSrc?] [name string?])]))

;; Path
(struct Path () #:transparent)
(struct Var Path (x ty) #:transparent)
(struct Select Path (p ie) #:transparent)
(struct Field Path (p f) #:transparent)
(struct Mode Path (p m) #:transparent)
(struct ExtVar Path (src name ty) #:transparent)

(provide
 (contract-out
  [struct Path ()]
  [struct Var ([x symbol?] [ty Type?])]
  [struct Select ([p Path?] [ie Expr?])]
  [struct Field ([p Path?] [f symbol?])]
  [struct Mode ([p Path?] [m symbol?])]
  [struct ExtVar ([src ExternSrc?] [name string?] [ty Type?])]))

;; Expressions
(struct Expr () #:transparent)
(struct Int Expr (signed? bits val) #:transparent)
(struct Flo Expr (bits val) #:transparent)
(struct Cast Expr (ty e) #:transparent)
(struct Read Expr (p) #:transparent)
(struct BinOp Expr (op L R) #:transparent)
(struct LetE Expr (x xe be) #:transparent)
(struct IfE Expr (ce te fe) #:transparent)
(struct MetaE Expr (m e) #:transparent)

(provide
 (contract-out
  [struct Expr ()]
  [struct Int ([signed? boolean?]
               [bits (apply or/c integer-bit-widths)]
               [val exact-integer?])]
  [struct Flo ([bits (apply or/c float-bit-widths)]
               [val (or/c single-flonum? double-flonum?)])]
  [struct Cast ([ty Type?] [e Expr?])]
  [struct Read ([p Path?])]
  [struct BinOp ([op symbol?] [L Expr?] [R Expr?])]
  [struct LetE ([x symbol?] [xe Expr?] [be Expr?])]
  [struct IfE ([ce Expr?] [te Expr?] [fe Expr?])]
  [struct MetaE ([m any/c] [e Expr?])]))

;; Initializer
(struct Init () #:transparent)
(struct Undef Init () #:transparent)
(struct ConI Init (e) #:transparent)
(struct ZedI Init (ty) #:transparent)
(struct ArrI Init (is) #:transparent)
(struct RecI Init (field->i) #:transparent)
(struct UniI Init (mode i) #:transparent)

(provide
 (contract-out
  [struct Init ()]
  [struct Undef ()]
  ;; DESIGN NOTE: It is unsafe for `e` to vary at runtime. We do not
  ;; protect against that possibility here, though, because the core
  ;; language is unsafe.
  [struct ConI ([e Expr?])]
  [struct ZedI ([ty Type?])]
  [struct ArrI ([is (listof Init?)])]
  [struct RecI ([field->i (hash/c symbol? Init?)])]
  [struct UniI ([mode symbol?] [i Init?])]))

;; Statements
(struct Stmt () #:transparent)
(struct Skip Stmt (comment) #:transparent)
(struct Fail Stmt (msg) #:transparent)
(struct Begin Stmt (f s) #:transparent)
(struct Assign Stmt (p e) #:transparent)
(struct If Stmt (p t f) #:transparent)
(struct While Stmt (p I body) #:transparent)
(struct Jump Stmt (label) #:transparent)
(struct Let/ec Stmt (label body) #:transparent)
(struct Let Stmt (x ty xi bs) #:transparent)
(struct MetaS Stmt (m bs) #:transparent)
;; DESIGN NOTE: `f` could be an `IntFun`, which includes `Stmt`, so
;; this is a mutually recursive definition. Alternatively, we could
;; treat functions like variables and have a name plus an environment
;; binding later in `Program`.
(struct Call (x ty f as bs) #:transparent)

(provide
 (contract-out
  [struct Stmt ()]
  [struct Skip ([comment (or/c #f string?)])]
  [struct Fail ([msg string?])]
  [struct Begin ([f Stmt?] [s Stmt?])]
  [struct Assign ([p Path?] [e Expr?])]
  [struct If ([p Expr?] [t Stmt?] [f Stmt?])]
  [struct While ([p Expr?] [I Expr?] [body Stmt?])]
  [struct Jump ([label symbol?])]
  [struct Let/ec ([label symbol?] [body Stmt?])]
  [struct Let ([x symbol?] [ty Type?] [xi Init?] [bs Stmt?])]
  [struct MetaS ([m any/c] [bs Stmt?])]
  [struct Call ([x symbol?] [ty Type?] [f Fun?] [as (listof Expr?)] [bs Stmt?])]))

;; Functions
(struct Arg (x ty mode) #:transparent)
(define mode/c (or/c 'read-only 'ref))
;; read-only := it and no piece of it can be modified

;; ref := the function receives a pointer and all changes are
;; reflected back to caller, as if the function were inlined

;; XXX copy := the function receives a copy that may be modified, but
;; changes are not visible. (This is not possible be inlined given
;; what is supported above, because there is no Copy operation.)

(struct Fun () #:transparent)
(struct IntFun Fun (args Pre ret-x ret-ty Post ret-lab body) #:transparent)
;; This definition is carefully chosen to be trivially inline-able.
#;(Call x0 ty0 (IntFun (list (Arg x1 ty1 mode1) ... (Arg xN tyN modeN))
                       Pre xR tyR Post ret-lab fun-body)
        (list xe1 ... xeN) res-body)
;; =
#;(Let x0 ty0 (Undef)
       (Begin
         (Let* ([mode1 x1 : ty1 := xe1] ... [modeN xN : tyN := xeN])
               (Begin (Assert Pre)
                      (Let xR tyR (Undef)
                           (Begin
                             (ReadOnly x0 ty0
                                       (Begin
                                         (Let/ec ret-lab
                                                 fun-body)
                                         (Assert Post)))
                             (Assign x0 xR)))))
         res-body))
;; But the compiler MAY turn it into an actual function call (perhaps
;; if it is used many times)

(struct ExtFun Fun (src args ret-ty name) #:transparent)

(provide
 (contract-out
  [struct Arg ([x symbol?] [ty Type?] [mode mode/c])]
  [struct Fun ()]
  [struct IntFun ([args (listof Arg?)]
                  [Pre Expr?]
                  [ret-x symbol?] [ret-ty Type?]
                  [Post Expr?]
                  [ret-lab symbol?] [body Stmt?])]
  [struct ExtFun ([src ExternSrc?]
                  [args (listof Arg?)]
                  [ret-ty Type?]
                  [name string?])]))

;; Program
(struct Global (ty xi) #:transparent)
(struct Program (globals private->public name->fun) #:transparent)

(provide
 (contract-out
  [struct Global ([ty Type?] [xi Init?])]
  [struct Program ([globals (hash/c symbol? Global?)]
                   [private->public (hash/c symbol? (or/c #f string?))]
                   [name->fun (hash/c string? IntFun?)])]))
