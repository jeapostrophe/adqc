#lang racket/base
(require (except-in pprint empty)
         racket/contract/base
         racket/list
         racket/match
         "ast.rkt")

(define (sym s) (text (symbol->string s)))
(define (num n) (text (number->string n)))

(define (type-doc ty)
  (match ty
    [(IntT signed? bits)
     (define ch (if signed? #\S #\U))
     (h-append (char ch) (num bits))]
    [(FloT bits)
     (h-append (char #\F) (num bits))]
    [(ArrT dim ety)
     (h-append (text "(array ") (num dim) space (type-doc ety) rparen)]
    [(RecT f->ty _ c-order)
     (define f-docs
       (for/list ([f (in-list c-order)])
         (hs-append (sym f) (type-doc (hash-ref f->ty f)))))
     (h-append (text "(record ") (apply hs-append f-docs) rparen)]
    [(UniT m->ty _)
     (define m-docs
       (for/list ([(m ty) (in-hash m->ty)])
         (hs-append (sym m) (type-doc ty))))
     (h-append (text "(union ") (apply hs-append m-docs) rparen)]
    [(ExtT _ name) (text name)]
    [(? VoiT?) (text "void")]
    [(? AnyT?) (text "any")]))

(define (path-doc p)
  (match (unpack-MetaP p)
    [(Var x _) (sym x)]
    [(Global ty xi)
     (define g-name (given-name p))
     (if g-name
         (sym g-name)
         (group
          (h-append
           lparen
           (nest 2 (v-append (text "Global") (type-doc ty) (init-doc xi)))
           rparen)))]
    [(Select p ie)
     (h-append lparen (path-doc p) (text " @ ") (expr-doc ie) rparen)]
    [(Field p f)
     (h-append lparen (path-doc p) (text " -> ") (sym f) rparen)]
    [(Mode p m)
     (h-append lparen (path-doc p) (text " as ") (sym m) rparen)]
    [(ExtVar _ name _) (text name)]))

(define (expr-doc e)
  (match (unpack-MetaE e)
    [(Int signed? bits val)
     (define ty (IntT signed? bits))
     (h-append lparen (type-doc ty) space (num val) rparen)]
    [(Flo bits val)
     (define ty (FloT bits))
     (h-append lparen (type-doc ty) space (num val) rparen)]
    [(Cast ty e)
     (h-append lparen (expr-doc e) space colon space (type-doc ty) rparen)]
    [(Read p) (path-doc p)]
    [(BinOp op L R)
     (group
      (h-append
       lparen
       (nest 2 (v-append (sym op) (expr-doc L) (expr-doc R)))
       rparen))]
    [(LetE x _ xe be)
     (define decl
       (h-append (text "LetE ([") (sym x) space (expr-doc xe) (text "])")))
     (group (h-append lparen (nest 2 (v-append decl (expr-doc be))) rparen))]
    [(IfE ce te fe)
     (group
      (h-append
       lparen
       (nest 2 (v-append (hs-append (text "IfE") (expr-doc ce))
                         (expr-doc te) (expr-doc fe)))
       rparen))]))

(define (init-doc i)
  (match i
    [(UndI _) (text "{}")]
    [(ConI e) (expr-doc e)]
    [(ZedI _) (text "{ 0 }")]
    [(ArrI is)
     (define is-doc
       (apply h-append (add-between (map init-doc is) (h-append comma space))))
     (hs-append lbrace is-doc rbrace)]
    [(RecI f->i)
     (define i-docs
       (add-between
        (for/list ([(f i) (in-hash f->i)])
          (hs-append (sym f) (text ":=") (init-doc i)))
        (h-append comma space)))
     (hs-append lbrace (apply h-append i-docs) rbrace)]
    [(UniI m i)
     (hs-append lbrace (h-append (text "#:") (sym m)) (init-doc i) rbrace)]))

(define (stmt-doc s)
  (match (unpack-MetaS s)
    [(Skip #f) (text "(void)")]
    [(Skip msg)
     (h-append (text "(void \"") (text msg) dquote rparen)]
    [(Fail msg)
     (h-append (text "(error \"") (text msg) dquote rparen)]
    [(Begin f s)
     (group
      (h-append
       lparen
       (nest 2 (v-append (text "begin") (stmt-doc f) (stmt-doc s)))
       rparen))]
    [(Assign p e)
     (h-append (text "(set! ") (path-doc p) space (expr-doc e) rparen)]
    [(If p t f)
     (group
      (h-append
       lparen
       (nest 2 (v-append (hs-append (text "If") (expr-doc p))
                         (stmt-doc t) (stmt-doc f)))
       rparen))]
    [(While p b)
     (group
      (h-append
       lparen
       (nest 2 (v-append (h-append (text "while") (expr-doc p))
                         (stmt-doc b)))
       rparen))]
    [(Jump l)
     (h-append (text "(Jump ") (sym l) rparen)]
    [(Let/ec l b)
     (nest 2 (v-append (h-append (text "(Let/ec ") (sym l))
                       (h-append (stmt-doc b) rparen)))]
    [(Let x ty xi bs)
     (define decl
       (h-append (text "Let ([") (sym x) (text " : ") (type-doc ty)
                       (text " := ") (init-doc xi) (text "])")))
     (group (h-append lparen (nest 2 (v-append decl (stmt-doc bs))) rparen))]
    [(Call x ty f as bs)
     (define f-name (given-name f))
     (define f-doc (if f-name (sym f-name) (fun-doc f)))
     (define decl
       (h-append (text "Call ([") (sym x) (text " := ") f-doc (text "])")))
     (group (h-append lparen (v-append decl (stmt-doc bs)) rparen))]))

(define (fun-doc f)
  (match (unpack-MetaFun f)
    [(IntFun args ret-x ret-ty ret-lab body)
     (define arg-docs
       (for/list ([a (in-list args)])
         (match-define (Arg x ty mode) a)
         (define mode-doc (h-append (text "#:") (sym mode)))
         (h-append lbracket mode-doc space (type-doc ty) space (sym x) rbracket)))
     (define head
       (h-append (text "(F ") (type-doc ret-ty) space
                 lparen (apply hs-append arg-docs) rparen))
     ;; XXX Emit ret-x and ret-lab
     (nest 2 (v-append head (h-append (stmt-doc body) rparen)))]
    [(ExtFun _ _ _ name) (text name)]))

(define (ast-doc ast)
  (match ast
    [(? Type?) (type-doc ast)]
    [(? Path?) (path-doc ast)]
    [(? Expr?) (expr-doc ast)]
    [(? Init?) (init-doc ast)]
    [(? Stmt?) (stmt-doc ast)]
    [(? Fun?)  (fun-doc ast)]))

(define (print-ast ast [out (current-output-port)] [width (current-page-width)])
  (pretty-print (ast-doc ast) out width)
  (void))

(provide
 (contract-out
  [print-ast (->* ((or/c Type? Path? Expr? Init? Stmt? Fun?))
                  (output-port? (or/c #f natural-number/c)) void?)]))
