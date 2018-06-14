#lang racket/base
(require racket/contract/base
         racket/format
         racket/function
         racket/list
         racket/match
         racket/string
         "ast.rkt")

;; XXX fill this in
(define bin-op-table
  (hasheq 'iadd "+" 'isub "-" 'imul "*" 'iudiv "/" 'isdiv "/" 'iurem "%" 'isrem "%"
          'ishl "<<" 'ilshr ">>" 'iashr ">>" 'iand "&" 'ior "|" 'ixor "^"
          'fadd "+" 'fsub "-" 'fmul "*" 'fdiv "/" 'frem "%"
          'ieq "==" 'ine "!="
          'iugt ">" 'iuge ">=" 'iult "<" 'iule "<="
          'isgt ">" 'isge ">=" 'islt "<" 'isle "<="
          'foeq "==" 'fogt ">" 'foge ">=" 'folt "<" 'fole "<="))

(define (compile-type ty)
  (match ty
    [(IntT signed? bits)
     (list* (if signed? "" "u") "int" (~a bits) "_t")]
    [(FloT bits)
     (match bits
       [32 "float"]
       [64 "double"])]))

(define (compile-path ρ path)
  (define (rec path) (compile-path ρ path))
  (match path
    [(Var x _)
     (hash-ref ρ x)]
    [(Select path ie)
     (list* "(" (rec path) "[" (compile-expr ρ ie) "])")]
    [(Field path f)
     (list* "(" (rec path) "." f ")")]
    [(Mode path m)
     (list "(" (rec path) "." m ")")]
    ;; XXX: Are f and m literal, or should we rename map them? If we don't,
    ;; then we have to restrict names of fields/modes to valid C IDs, even
    ;; in the CF+ layer. If we do, then we either need separate rename maps
    ;; for vars, fields, and modes, or we need to assume that a given name,
    ;; when mapped to its 'valid C equivalent', will always produce the same
    ;; output. In that case, maybe rho isn't a hash table but actually a pure
    ;; function?

    ;; XXX: ExtVar
    ))

(define (compile-expr ρ e)
  (define (rec e) (compile-expr ρ e))
  (match e
    [(Int signed? bits val)
     (list* "((" (compile-type (IntT signed? bits)) ")" (~a val) ")")]
    [(Flo bits val)
     (list* "((" (compile-type (FloT bits)) ")" (~a val) ")")]
    [(Cast ty e)
     (list* "((" (compile-type ty) ")" (rec e) ")")]
    [(Read (Var x _))
     (hash-ref ρ x)]
    [(BinOp op L R)
     (define op-str (hash-ref bin-op-table op))
     (list* "(" (rec L) " " op-str " " (rec R) ")")]
    [(LetE x _ xe be)
     ;; DESIGN: We ignore xt because x does not become a real thing in
     ;; C (because we can't make it.) If/when we compile to LLVM, we
     ;; will be able to make it something and it will be useful.
     (compile-expr (hash-set ρ x (compile-expr ρ xe)) be)]
    [(IfE ce te fe)
     (list* "(" (rec ce) " ? " (rec te) " : " (rec fe) ")")]
    [(MetaE _ e)
     (rec e)]))

(define (compile-decl ty name [val #f])
  (define assign (and val (list* " = " val)))
  (match ty
    [(or (? IntT?) (? FloT?))
     (list* (compile-type ty) #\space name assign ";")]
    [(ArrT dim ety)
     (list* (compile-type ety) #\space name "[" (~a dim) "]" assign ";")]
    ))

(define (compile-init ρ i)
  (define (rec i) (compile-init ρ i))
  (match i
    [(UndI ty) #f]
    [(ConI e) (compile-expr ρ e)]
    [(ZedI ty) (type-zero ty)]
    [(ArrI is)
     (add-between (map rec is) '(", ") #:splice? #t
                  #:before-first '("{ ") #:after-last '(" }"))]
    ;; XXX: RecT, UniT
    ))

(define (type-zero ty)
  (match ty
    [(IntT signed? bits)
     (compile-expr (hasheq) (Int signed? bits 0))]
    [(FloT bits)
     (compile-expr (hasheq) (Flo bits 0.0))]
    [(ArrT dim ety) "{ 0 }"]
    ;; XXX: RecT, UniT, ExtT
    ))

(define (compile-stmt γ ρ s)
  (define (rec s) (compile-stmt γ ρ s))
  (match s
    [(Skip c)
     (and c (list* "/* " c " */"))]
    [(Fail m)
     (list* "fprintf(stderr, " (~v m) ");" ind-nl
            "exit(1);")]
    [(Assign (Var x _) e)
     (list* (hash-ref ρ x) " = " (compile-expr ρ e) ";")]
    [(Begin f s)
     (list* (rec f) ind-nl (rec s))]
    [(If p t f)
     (list* "if " (compile-expr ρ p) " {" ind++ ind-nl
            (rec t)
            ind-- ind-nl "} else {" ind++ ind-nl
            (rec f)
            ind-- ind-nl "}")]
    [(While p b)
     (list* "while " (compile-expr ρ p) " {" ind++ ind-nl
            (rec b)
            ind-- ind-nl "}")]
    [(Jump l)
     (list* "goto " (hash-ref γ l) ";")]
    [(Let/ec l b)
     (define cl (~a (gensym 'label)))
     (list* (compile-stmt (hash-set γ l cl) ρ b) ind-nl
            cl ":")]
    ;; XXX: What data is ρ supposed to carry? Right now it just
    ;; maps from name -> name, which seems obviously wrong, but
    ;; in 'compile-expr', the `(Read (Var x _))` case seems to
    ;; expect this. Should we change it so that ρ maps from
    ;; name to type?
    [(Let x ty xi bs)
     (define decl (compile-decl ty x (compile-init ρ xi)))
     (list* decl ind-nl
            (compile-stmt γ (hash-set ρ x x) bs))]
    [(MetaS _ s)
     (compile-stmt γ ρ s)]))

;; Σ is a renaming environment for public functions
;; ρ is a renaming environment for global variables
(define (compile-fun Σ ρ f)
  ;; XXX
  (match f))

(define (compile-program p)
  ;; XXX
  (match p))

;; Display code

(define ind-nl (gensym))
(define ind++ (gensym))
(define ind-- (gensym))
(define ind-lvl (box 0))

(define (idisplay v)
  (match v
    [(== ind-nl)
     (newline)
     (for ([i (in-range (unbox ind-lvl))])
       (display #\space))]
    [(== ind++)
     (set-box! ind-lvl (+ (unbox ind-lvl) 2))]
    [(== ind--)
     (set-box! ind-lvl (- (unbox ind-lvl) 2))]
    [_ (display v)]))

(define (tree-for f t)
  (match t
    [(or (? void?) #f '()) (void)]
    [(cons a d) (tree-for f a) (tree-for f d)]
    [x (f x)]))

(module+ test
  ;; XXX Drop this function and just accept a Program from ast.rkt
  (define (compile&emit ρ s)
    (tree-for idisplay (compile-stmt (hasheq) ρ s))))

;; XXX A function that actually really calls the compiler with the
;; appropriate -l lines, etc.

;; XXX A function that calls the above one and links it in and sends
;; it some input

(module+ test
  ;; XXX: Actually test things instead of just printing them to console
  ;; JM: Actually, don't and hook the test suite into the compiler
  (define (dnewline)
    (printf "~n~n"))
  (compile&emit (hasheq 'x 'x) (Assign (Var 'x (IntT #f 32)) (Int #f 32 100)))
  (dnewline)
  (tree-for idisplay
            (compile-expr (hasheq)
                          (IfE (BinOp 'islt (Int #f 32 5) (Int #f 32 6))
                               (BinOp 'iadd (Int #t 64 2) (Int #t 64 3))
                               (BinOp 'isub (Int #t 64 5) (Int #t 64 6)))))
  (dnewline)
  (tree-for idisplay
            (compile-expr (hasheq)
                          (LetE 'x (IntT #f 32) (Int #f 32 5)
                                (BinOp 'iadd
                                       (Read (Var 'x (IntT #f 32)))
                                       (Int #f 32 1)))))
  (dnewline)
  (tree-for idisplay
            (compile-decl
             (ArrT 3 (IntT #f 32)) "my_arr"
             (compile-init (hasheq)
                           (ArrI (list
                                  (ConI (Int #f 32 0))
                                  (ConI (Int #f 32 1))
                                  (ConI (Int #f 32 2)))))))
  (dnewline)
  (tree-for idisplay
            (compile-stmt
             (hasheq) (hasheq 'y 'y)
             (Let 'x (ArrT 3 (IntT #t 32)) (ZedI (ArrT 3 (IntT #t 32)))
                  (Assign (Var 'y (IntT #t 32)) (Int #t 32 5))))))
  
