#lang racket/base
(require racket/contract/base
         racket/format
         racket/list
         racket/match
         "ast.rkt")

(define (int-cast signed? bw)
  (list* "(" (if signed? "" "u") "int" (~a bw) "_t)"))
(define (flo-cast bw)
  (match bw
    [32 "(float)"]
    [64 "(double)"]))

;; XXX fill this in
(define bin-op-table
  (hasheq 'iadd "+" 'isub "-" 'imul "*" 'iudiv "/" 'isdiv "/" 'iurem "%" 'isrem "%"
          'ishl "<<" 'ilshr ">>" 'iashr ">>" 'iand "&" 'ior "|" 'ixor "^"
          'fadd "+" 'fsub "-" 'fmul "*" 'fdiv "/" 'frem "%"
          'ieq "==" 'ine "!="
          'iugt ">" 'iuge ">=" 'iult "<" 'iule "<="
          'isgt ">" 'isge ">=" 'islt "<" 'isle "<="
          'foeq "==" 'fogt ">" 'foge ">=" 'folt "<" 'fole "<="))

(define (compile-expr ρ e)
  (define (rec e) (compile-expr ρ e))
  (match e
    [(Int signed? bits val)
     (list* "(" (int-cast signed? bits) (~a val) ")")]
    [(Flo bits val)
     ;; XXX perhaps use the fast way to read floats in C as the raw bits
     (list* "(" (flo-cast bits) (~a val) ")")]
    [(Cast ty e)
     (match ty
       [(IntT signed? bits)
        (list* "(" (int-cast signed? bits) (rec e) ")")]
       [(FloT bits)
        (list* "(" (flo-cast bits) (rec e) ")")])]
    [(Read (Var x _))
     (hash-ref ρ x)]
    [(BinOp op L R)
     (define op-str (hash-ref bin-op-table op))
     (list* "(" (rec L) " " op-str " " (rec R) ")")]))

(define (compile-stmt γ ρ s)
  (define (verify! p)
    #f)
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
    [(While p _ b)
     (list* "while " (compile-expr ρ p) " {" ind++ ind-nl
            (rec b)
            ind-- ind-nl "}")]
    [(Jump l)
     (list* "goto " (hash-ref γ l) ";")]
    [(Let/ec l b)
     (define cl (~a (gensym 'label)))
     (list* (compile-stmt (hash-set γ l cl) ρ b) ind-nl
            cl ":")]))

(define (compile-stmt* ρ s)
  (compile-stmt (hasheq) ρ s))

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
  ;; XXX better interface
  (define (compile&emit ρ s)
    (tree-for idisplay (compile-stmt* ρ s)))
  (provide compile&emit))
