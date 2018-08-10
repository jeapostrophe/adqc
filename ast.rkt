#lang racket/base
(require racket/contract/base
         racket/match)

(define float-bit-widths '(32 64))
(define integer-bit-widths '(8 16 32 64))

;; This is a partial test, see
;; http://en.cppreference.com/w/c/language/identifier for the complete
;; rules.
(define (c-identifier-string? x)
  (regexp-match? #rx"^[_a-zA-Z][a-zA-Z0-9_]*$" x))

(define current-cify-counter (make-parameter (box 0)))
(define-syntax-rule (with-cify-counter . b)
  (parameterize ([current-cify-counter (box 0)]) . b))
(define (cify s)
  (define cify-counter (current-cify-counter))
  (define which (unbox cify-counter))
  (set-box! cify-counter (add1 which))
  (format "_~a_~a"
          (regexp-replace* #rx"[^a-zA-Z0-9_]" (symbol->string s) "_")
          which))

;; Legal things the right of -l in cc
(define c-library-string? string?)
;; Legal things inside the <>s of an #include
(define c-header-string? string?)

(provide
 with-cify-counter
 (contract-out
  [c-identifier-string? (-> any/c boolean?)]
  [c-library-string? (-> any/c boolean?)]
  [c-header-string? (-> any/c boolean?)]
  [cify (-> symbol? c-identifier-string?)]))

;; Extern Source
(struct ExternSrc (ls hs) #:transparent)

(provide
 (contract-out
  [struct ExternSrc ([ls (listof c-library-string?)]
                     [hs (listof c-header-string?)])]))

;; Types
(struct Type () #:transparent)
(struct IntT Type (signed? bits) #:transparent)
(struct FloT Type (bits) #:transparent)
(struct ArrT Type (dim ety) #:transparent)
(struct RecT Type (field->ty field->c c-order) #:transparent)
(struct UniT Type (mode->ty mode->c) #:transparent)
(struct ExtT Type (src name) #:transparent)

(provide
 (contract-out
  [struct Type ()]
  [struct IntT ([signed? boolean?]
                [bits (apply or/c integer-bit-widths)])]
  [struct FloT ([bits (apply or/c float-bit-widths)])]
  [struct ArrT ([dim exact-nonnegative-integer?] [ety Type?])]
  [struct RecT ([field->ty (hash/c symbol? Type?)]
                [field->c (hash/c symbol? c-identifier-string?)]
                [c-order (listof symbol?)])]
  [struct UniT ([mode->ty (hash/c symbol? Type?)]
                [mode->c (hash/c symbol? c-identifier-string?)])]
  [struct ExtT ([src ExternSrc?] [name c-identifier-string?])]))

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
  [struct ExtVar ([src ExternSrc?] [name c-identifier-string?] [ty Type?])]))

;; Expressions
(struct Expr () #:transparent)
(struct Int Expr (signed? bits val) #:transparent)
(struct Flo Expr (bits val) #:transparent)
(struct Cast Expr (ty e) #:transparent)
(struct Read Expr (p) #:transparent)
(struct BinOp Expr (op L R) #:transparent)
;; DESIGN: We could instead make LamE and AppE then make expressions a
;; static simply-typed version of the lambda-calculus. I think this
;; would be overkill. We can do most of what we want with Racket
;; macros though.
(struct LetE Expr (x ty xe be) #:transparent)
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
  [struct LetE ([x symbol?] [ty Type?] [xe Expr?] [be Expr?])]
  [struct IfE ([ce Expr?] [te Expr?] [fe Expr?])]
  [struct MetaE ([m any/c] [e Expr?])]))

;; Initializer
(struct Init () #:transparent)
(struct UndI Init (ty) #:transparent)
(struct ConI Init (e) #:transparent)
(struct ZedI Init (ty) #:transparent)
(struct ArrI Init (is) #:transparent)
(struct RecI Init (field->i) #:transparent)
(struct UniI Init (mode i) #:transparent)

(provide
 (contract-out
  [struct Init ()]
  [struct UndI ([ty Type?])]
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
(struct While Stmt (p body) #:transparent)
(struct Jump Stmt (label) #:transparent)
(struct Let/ec Stmt (label body) #:transparent)
(struct Let Stmt (x ty xi bs) #:transparent)
(struct MetaS Stmt (m bs) #:transparent)
;; DESIGN NOTE: `f` could be an `IntFun`, which includes `Stmt`, so
;; this is a mutually recursive definition. Alternatively, we could
;; treat functions like variables and have a name plus an environment
;; binding later in `Program`.
(struct Call Stmt (x ty f as bs) #:transparent)

(provide
 (contract-out
  [struct Stmt ()]
  [struct Skip ([comment (or/c #f string?)])]
  [struct Fail ([msg string?])]
  [struct Begin ([f Stmt?] [s Stmt?])]
  [struct Assign ([p Path?] [e Expr?])]
  [struct If ([p Expr?] [t Stmt?] [f Stmt?])]
  [struct While ([p Expr?] [body Stmt?])]
  [struct Jump ([label symbol?])]
  [struct Let/ec ([label symbol?] [body Stmt?])]
  [struct Let ([x symbol?] [ty Type?] [xi Init?] [bs Stmt?])]
  [struct MetaS ([m any/c] [bs Stmt?])]
  [struct Call ([x symbol?] [ty Type?] [f Fun?] [as (listof (or/c Expr? Path?))] [bs Stmt?])]))

;; Functions
(struct Arg (x ty mode) #:transparent)
(define mode/c (or/c 'read-only 'copy 'ref))
;; read-only := it and no piece of it can be modified (could be
;; implemented as read-only or copy)

;; ref := the function receives a pointer and all changes are
;; reflected back to caller, as if the function were inlined. This
;; should only work if the argument is a path.

;; copy := the function receives a shallow copy that may be modified,
;; but changes are not visible.

(struct Fun () #:transparent)
(struct IntFun Fun (args ret-x ret-ty ret-lab body) #:transparent)
;; This definition is carefully chosen to be trivially inline-able.
;; but the compiler MAY turn it into an actual function call (perhaps
;; if it is used many times)

(struct MetaFun Fun (m f) #:transparent)
(struct ExtFun Fun (src args ret-ty name) #:transparent)

(define (unpack-MetaFun f)
  (match f
    [(MetaFun _ f)
     (unpack-MetaFun f)]
    [(? Fun?) f]))

(provide
 (contract-out
  [struct Arg ([x symbol?] [ty Type?] [mode mode/c])]
  [struct Fun ()]
  [struct IntFun ([args (listof Arg?)]
                  [ret-x symbol?] [ret-ty Type?]
                  [ret-lab symbol?] [body Stmt?])]
  [struct MetaFun ([m any/c] [f Fun?])]
  [struct ExtFun ([src ExternSrc?]
                  [args (listof Arg?)]
                  [ret-ty Type?]
                  [name c-identifier-string?])]
  [unpack-MetaFun (-> Fun? Fun?)]))

(define (IntFun*? x)
  (or (IntFun? x)
      (and (MetaFun? x) (IntFun*? (MetaFun-f x)))))

;; Program
(struct Global (ty xi) #:transparent)
(struct Program (globals private->public name->fun) #:transparent)

(provide
 (contract-out
  [struct Global ([ty Type?] [xi Init?])]
  [struct Program ([globals (hash/c symbol? Global?)]
                   [private->public (hash/c symbol? (or/c #f c-identifier-string?))]
                   [name->fun (hash/c c-identifier-string? IntFun*?)])]))
