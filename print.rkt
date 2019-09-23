#lang racket/base
(require (except-in pprint empty)
         racket/contract/base
         racket/list
         racket/match
         "ast.rkt")

;; XXX remove this when we have better printing for types and inits
(require racket/format)

;; XXX sym should print vertical bars around symbols with whitespace
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
    [(? VoiT?) (text "void")]))

(define (path-doc p)
  (match (unpack-MetaP p)
    [(Var x _) (sym x)]
    ;; XXX Better printing for globals, probably by modifying define-global
    ;; to store name info as metadata
    [(Global ty _)
     (h-append lparen (text "SomeGlobal") space (type-doc ty) rparen)]
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
     (h-append
      lparen
      (hs-append (sym op) (expr-doc L) (expr-doc R))
      rparen)]
    [(LetE x _ xe be)
     (nest 2 (v-append
              (h-append (text "(LetE ([") (sym x) space (expr-doc xe) (text "])"))
              (expr-doc be)))]
    [(IfE ce te fe)
     (h-append
      lparen
      (hs-append (text "IfE") (expr-doc ce) (expr-doc te) (expr-doc fe))
      rparen)]))

(define (init-doc i)
  (match i
    [(UndI _) (text "{}")]
    [(ConI e) (expr-doc e)]
    [(ZedI _) (text "{ 0 }")]
    [(ArrI is)
     (define is-doc
       (apply h-append (add-between (map init-doc is) (h-append comma space))))
     (hs-append lbrace is-doc rbrace)]
    ;; XXX rec, union
    [_ (text (~a i))]))

(define (stmt-doc s)
  (match (unpack-MetaS s)
    [(Skip #f) (text "(void)")]
    [(Skip msg)
     (h-append (text "(void \"") (text msg) dquote rparen)]
    [(Fail msg)
     (h-append (text "(error \"") (text msg) dquote rparen)]
    [(Begin f s)
     (h-append (text "(begin ") (stmt-doc f) space (stmt-doc s) rparen)]
    [(Assign p e)
     (h-append (text "(set! ") (path-doc p) space (expr-doc e) rparen)]
    [(If p t f)
     (h-append
      lparen
      (hs-append (text "If") (expr-doc p) (stmt-doc t) (stmt-doc f))
      rparen)]
    [(While p b)
     (nest 2 (v-append (h-append (text "(while ") (expr-doc p))
                       (h-append (stmt-doc b) rparen)))]
    [(Jump l)
     (h-append (text "(Jump ") (sym l) rparen)]
    [(Let/ec l b)
     (nest 2 (v-append (h-append (text "(Let/ec ") (sym l))
                       (h-append (stmt-doc b) rparen)))]
    [(Let x ty xi bs)
     (define decl
       (h-append (sym x) (text " : ") (type-doc ty) (text " := ") (init-doc xi)))
     (nest 2 (v-append (h-append (text "(Let ([") decl (text "])"))
                       (h-append (stmt-doc bs) rparen)))]
    ;; XXX implement this fully with meta data for functions
    [(Call x ty f as bs)
     (text "(some function call)")]))

(define (fun-doc f)
  (error 'fun-doc "XXX: pretty printing for functions"))

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
  (newline))

(provide
 (contract-out
  [print-ast (->* ((or/c Type? Path? Expr? Init? Stmt? Fun?))
                  (output-port? (or/c #f natural-number/c)) void?)]))
