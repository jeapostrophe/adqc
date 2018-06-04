#lang racket/base
(require racket/format
         racket/list
         racket/match
         "ast.rkt")

(define (int-cast signed? bw)
  (list* "(" (if signed? "" "u") "int" (~a bw) "_t)"))
(define (flo-cast bw)
  (match bw
    [32 "float"]
    [64 "double"]))

(define bin-op-table
  (hasheq 'iadd "+"
          'isub "-"
          'imul "*"
          'ieq "=="
          'iult "<"))

(define (compile-expr ρ e)
  (define (rec e) (compile-expr ρ e))
  (match e
    [(Int signed? bits val)
     (list* "(" (int-cast signed? bits) (~a val) ")")]
    [(Flo bits val)
     (list* "(" (flo-cast bits) (~a val) ")")]
    [(Var x _)
     (hash-ref ρ x)]
    [(BinOp op L R)
     (define op-str (hash-ref bin-op-table op))
     (list* "(" (rec L) " " op-str " " (rec R) ")")]))

(define (compile-stmt γ ρ s)
  ;; XXX This should consult the verifier. But, how?
  ;;
  ;; As we are compiling, we could compute the strongest
  ;; post-condition (SP) of the code that came before this point and
  ;; then check the theorem (not (SP => P)) for UNSAT. If it is SAT,
  ;; then the condition is not verified (#f), if it is UNSAT, then the
  ;; condition is checked.
  ;;
  ;; It is awkard to have the compiler interact with the theorem
  ;; prover this way though, particularly having to compute the SP. So
  ;; another idea is to have the verifier run first and return a weak
  ;; hash-table mapping each precondition to whether it can be SAT or
  ;; NOT in this way, then compiler can consult the table.
  (define (verify! p)
    #f)
  (define (rec s) (compile-stmt γ ρ s))
  (match s
    [(Skip) '()]
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
    [(Return l)
     (list* "goto " (hash-ref γ l) ";")]
    [(Let/ec l b)
     (define cl (~a (gensym 'label)))
     (list* (compile-stmt (hash-set γ l cl) ρ b) ind-nl
            cl ":")]
    [(Assert must-be-static? p msg)
     (list* "/* ASSERT " msg ": " (compile-expr ρ p) " */" ind-nl
            (cond
              [(verify! p)
               "/* Statically verified! */"]
              [(not must-be-static?)
               (compile-stmt γ ρ (If p (Skip) (Fail (~a "Assertion failed: " msg))))]
              [else
               (error 'compile "Assertion not verifiable statically: ~a" msg)]))]))

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
  (define (compile&emit ρ s)
    (tree-for idisplay (compile-stmt* ρ s)))
  (provide compile&emit))
