#lang racket/base
(require data/queue
         graph
         racket/contract/base
         racket/file
         racket/format
         racket/function
         racket/list
         racket/match
         racket/set
         racket/string
         racket/system
         "ast.rkt")

;; XXX Read through https://queue.acm.org/detail.cfm?id=3212479 and
;; see if there are any changes we should have to our language
;; model. I think a lot of our choices are good because we don't make
;; promises about memory.


;; modes are 'copy, 'ref, or 'const-ref
(struct var-info (exp mode) #:transparent)

(define current-headers (make-parameter (mutable-set)))
(define current-libs (make-parameter (mutable-set)))
(define current-fun-queue (make-parameter (make-queue)))
(define current-fun (make-parameter #f))
(define current-fun-graph (make-parameter (unweighted-graph/directed empty)))
(define current-Σ (make-parameter (make-hash)))

(define (include-src! src)
  (match-define (ExternSrc ls hs) src)
  (for ([l (in-list ls)])
    (set-add! (current-libs) l))
  (for ([h (in-list hs)])
    (set-add! (current-headers) h)))

(define math-h (ExternSrc '("m") '("math.h")))
(define stdio-h (ExternSrc '() '("stdio.h")))
(define stdlib-h (ExternSrc '() '("stdlib.h")))
(define stdint-h (ExternSrc '() '("stdint.h")))

(define ((c-op op) ρ a b)
  (define a* (compile-expr ρ a))
  (define b* (compile-expr ρ b))
  (list* "(" a* " " op " " b* ")"))

(define ((c-unord-op op) ρ a b)
  (include-src! math-h)
  (define a* (compile-expr ρ a))
  (define b* (compile-expr ρ b))
  (list* "(isnan(" a* ") || isnan(" b* ") || (" a* " " op " " b* "))"))

(define ((c-fun bits->name src) ρ a b)
  (include-src! src)
  (define a* (compile-expr ρ a))
  (define b* (compile-expr ρ b))
  ;; XXX dispatch on type
  (define name* (hash-ref bits->name 64))
  (list* "(" name* "(" a* ", " b* "))"))

(define (compile-fone ρ a b)
  (include-src! math-h)
  (define a* (compile-expr ρ a))
  (define b* (compile-expr ρ b))
  (list* "(!isnan(" a* ") && !isnan(" b* ") && (" a* " != " b* "))"))

(define (compile-ford ρ a b)
  (include-src! math-h)
  (define a* (compile-expr ρ a))
  (define b* (compile-expr ρ b))
  (list* "(!isnan(" a* ") && !isnan(" b* "))"))

(define (compile-funo ρ a b)
  (include-src! math-h)
  (define a* (compile-expr ρ a))
  (define b* (compile-expr ρ b))
  (list* "(isnan(" a* ") || isnan(" b* "))"))

(define bin-op-table
  (hasheq 'iadd (c-op "+")
          'isub (c-op "-")
          'imul (c-op "*")
          'iudiv (c-op "/")
          'isdiv (c-op "/")
          'iurem (c-op "%")
          'isrem (c-op "%")
          'ishl (c-op "<<")
          'ilshr (c-op ">>")
          'iashr (c-op ">>")
          'iand (c-op "&")
          'ior (c-op "|")
          'ixor (c-op "^")
          'fadd (c-op "+")
          'fsub (c-op "-")
          'fmul (c-op "*")
          'fdiv (c-op "/")
          'frem (c-fun (hasheq 32 "fmodf" 64 "fmod") math-h)
          'ieq (c-op "==")
          'ine (c-op "!=")
          'iugt (c-op ">")
          'iuge (c-op ">=")
          'iult (c-op "<")
          'iule (c-op "<=")
          'isgt (c-op ">")
          'isge (c-op ">=")
          'islt (c-op "<")
          'isle (c-op "<=")
          'foeq (c-op "==")
          'fone compile-fone
          'fogt (c-op ">")
          'foge (c-op ">=")
          'folt (c-op "<")
          'fole (c-op "<=")
          'fueq (c-unord-op "==")
          ;; Note: behavior of C's != operator is unordered.
          'fune (c-op "!=")
          'fugt (c-unord-op ">")
          'fuge (c-unord-op ">=")
          'fult (c-unord-op "<")
          'fule (c-unord-op "<=")
          'ffalse (const "(0)")
          'ftrue (const "(1)")
          'ford compile-ford
          'funo compile-funo
          ))

(define (compile-type ty [mode 'copy])
  (define ty-ast (match ty
                   [(IntT signed? bits)
                    (list* (if signed? "" "u") "int" (~a bits) "_t")]
                   [(FloT bits)
                    (match bits
                      [32 "float"]
                      [64 "double"])]))
  (match mode
    ['copy ty-ast]
    ['ref (list* ty-ast "*")]
    ['const-ref (list* "const " ty-ast "*")]))

(define (path-x path)
  (match path
    [(Var x _) x]
    [(or (Select path _)
         (Field path _)
         (Mode path _))
     (path-x path)]
    ;; XXX ExtVar
    ))

(define (path-deref? ρ path)
  (match (var-info-mode (hash-ref ρ (path-x path)))
    ['copy #f]
    [(or 'ref 'const-ref) #t]))

(define (compile-path ρ path deref-root?)
  (define (rec path) (compile-path ρ path deref-root?))
  (match path
    [(Var x ty)
     (define exp (var-info-exp (hash-ref ρ x)))
     (define ast (if deref-root? (list* "(*" exp ")") exp))
     (values ast ty)]
    [(Select path ie)
     (define-values (pc pty) (rec path))
     (match-define (ArrT _ ety) pty)
     (values (list* "(" pc "[" (compile-expr ρ ie) "])")
             ety)]
    [(Field path f)
     (define-values (pc pty) (rec path))
     (match-define (RecT f->ty f->c _) pty)
     (values (list* "(" pc "." (hash-ref f->c f) ")")
             (hash-ref f->ty f))]
    [(Mode path m)
     (define-values (pc pty) (rec path))
     (match-define (UniT m->ty m->c) pty)
     (values (list "(" pc "." (hash-ref m->c m) ")")
             (hash-ref m->ty m))]
    [(ExtVar src n ty)
     ;; XXX register src
     (values n ty)]))

(define (compile-expr ρ e)
  (define (rec e) (compile-expr ρ e))
  (match e
    [(Int signed? bits val)
     (list* "((" (compile-type (IntT signed? bits)) ")" (~a val) ")")]
    [(Flo bits val)
     (define val* (cond [(equal? val +nan.0)
                         (include-src! math-h)
                         "(NAN)"]
                        [else (~a val)]))
     (list* "((" (compile-type (FloT bits)) ")" val* ")")]
    [(Cast ty e)
     (list* "((" (compile-type ty) ")" (rec e) ")")]
    [(Read path)
     (match-define-values (name _)
       (compile-path ρ path (path-deref? ρ path)))
     name]
    [(BinOp op L R)
     (define op-fn (hash-ref bin-op-table op))
     (op-fn ρ L  R)]
    [(LetE x _ xe be)
     ;; DESIGN: We ignore xt because x does not become a real thing in
     ;; C (because we can't make it.) If/when we compile to LLVM, we
     ;; will be able to make it something and it will be useful.
     (define xe-info (var-info (compile-expr ρ xe) 'copy))
     (compile-expr (hash-set ρ x xe-info) be)]
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
    ;; XXX Lift unique RecT to top-level and give a name?
    [(RecT f->ty f->c c-order)
     (list* "struct {" ind++ ind-nl
            (add-between
             (for/list ([f (in-list c-order)])
               (compile-decl (hash-ref f->ty f) (hash-ref f->c f)))
             ind-nl)
            ind-- ind-nl "} " name assign ";")]
    ;; XXX Lift unique UniT to top-level and give a name?
    [(UniT m->ty m->c)
     (list* "union {" ind++ ind-nl
            (add-between
             (for/list ([(m ty) (in-hash m->ty)])
               (compile-decl ty (hash-ref m->c m)))
             ind-nl)
            ind-- ind-nl "} " name assign ";")]
    [(ExtT src name)
     (include-src! src)
     (list* "extern void* " name ";")]))

(define (compile-init ρ ty i)
  (define (rec i) (compile-init ρ ty i))
  (match i
    [(UndI ty) #f]
    [(ConI e) (compile-expr ρ e)]
    [(ZedI ty) (type-zero ty)]
    [(ArrI is)
     (match-define (ArrT _ ety) ty)
     (list* "{ "
            (add-between
             (for/list ([i (in-list is)])
               (compile-init ρ ety i))
             ", ")
            " }")]
    [(RecI f->i)
     (match-define (RecT f->ty _ c-order) ty)
     (list* "{ "
            (add-between
             (for/list ([f (in-list c-order)])
               (compile-init ρ (hash-ref f->ty f) (hash-ref f->i f)))
             ", ")
            " }")]
    [(UniI m i)
     (match-define (UniT m->ty m->c) ty)
     (define ie (compile-init ρ (hash-ref m->ty m) i))
     (list* "{ ." (hash-ref m->c m) " = " ie " }")]))

(define (type-zero ty)
  (match ty
    [(IntT signed? bits)
     (compile-expr (hasheq) (Int signed? bits 0))]
    [(FloT bits)
     (compile-expr (hasheq) (Flo bits 0.0))]
    [(or (? ArrT?) (? RecT?) (? UniT?)) "{ 0 }"]
    ;; XXX: ExtT
    ))

;; XXX make sure that comment lines don't end in backslash
;; so that real code isn't accidentally commented out.
(define (compile-comment cstr)
  (add-between
   (for/list ([c (in-list (string-split cstr "\n"))])
     (list* "// " c))
   ind-nl))

(define (compile-stmt γ ρ s)
  (define (rec s) (compile-stmt γ ρ s))
  (match s
    [(Skip c)
     (and c (compile-comment c))]
    [(Fail m)
     ;; XXX Ensure (~v m) is valid C string (maybe turn into a literal
     ;; array at top-level?)
     (include-src! stdio-h)
     (include-src! stdlib-h)
     (list* "fprintf(stderr, " (~v m) ");" ind-nl
            "exit(1);")]
    [(Assign path e)
     (match-define-values (name _)
       (compile-path ρ path (path-deref? ρ path)))
     (list* name " = " (compile-expr ρ e) ";")]
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
     (define cl (cify l))
     (list* (compile-stmt (hash-set γ l cl) ρ b) ind-nl
            cl ":")]
    [(Let x ty xi bs)
     (define x* (cify x))
     (list* (compile-decl ty x* (compile-init ρ ty xi)) ind-nl
            (compile-stmt γ (hash-set ρ x (var-info x* 'copy)) bs))]
    [(MetaS _ s)
     (compile-stmt γ ρ s)]
    [(Call x ty f as bs)
     (define Σ (current-Σ))
     (define f* (unpack-MetaFun f))
     (add-directed-edge! (current-fun-graph) f* (current-fun))
     (define fun-name (cond [(hash-has-key? Σ f*)
                             (hash-ref Σ f*)]
                            [else
                             (define fun-name (cify 'fun))
                             (hash-set! Σ f* fun-name)
                             (enqueue! (current-fun-queue) f*)
                             fun-name]))
     (define args-ast
       (add-between
        ;; var-* is argument being passed
        ;; arg-* relates to what the function expects of its arguments
        (for/list ([var (in-list as)]
                   [arg (in-list (Fun-args f*))])
          (match var
            [(or (Read path) (? Path? path))
             (define var-mode (var-info-mode (hash-ref ρ (path-x path))))
             (define arg-mode (Arg->var-mode arg))
             (define var-is-ref? (or (eq? var-mode 'ref)
                                     (eq? var-mode 'const-ref)))
             (define arg-is-ref? (or (eq? arg-mode 'ref)
                                     (eq? arg-mode 'const-ref)))
             (define-values (deref-var? take-var-addr?)
               (cond [(eq? var-is-ref? arg-is-ref?)
                      (values #f #f)]
                     [(and var-is-ref? (not (arg-is-ref?)))
                      (values #t #f)]
                     [(and (not var-is-ref?) arg-is-ref?)
                      (values #f #t)]))
             (match-define-values (var-ast _)
               (compile-path ρ path deref-var?))
             (if take-var-addr?
                 (list* "(&" var-ast ")")
                 var-ast)]
            [(? Expr?) (compile-expr ρ var)]))
        ", "))
     (define x* (cify x))
     (define call-ast (compile-decl ty x* (list* fun-name "(" args-ast ")")))
     (define body-ast (compile-stmt γ (hash-set ρ x (var-info x* 'copy)) bs))
     (list* call-ast ind-nl
            body-ast)]))

(define (Arg->var-mode arg)
  (match-define (Arg _ ty mode) arg)
  (cond [(eq? mode 'ref) 'ref]
        [(or (eq? mode 'copy)
             (and (eq? mode 'read-only)
                  (or (IntT? ty) (FloT? ty))))
         'copy]
        [(eq? mode 'read-only) 'const-ref]))

;; Σ is a renaming environment for public functions
;; ρ is a renaming environment for global variables
(define (compile-fun ρ f)
  (match f
    [(MetaFun _ f) (compile-fun ρ f)]
    [(IntFun as ret-x ret-ty ret-lab body)
     (parameterize ([current-fun f])
       (define fun-name (hash-ref (current-Σ) f))
       (define ρ* (for/fold ([out ρ])
                            ([arg (in-list as)])
                    (match-define (Arg x ty mode) arg)
                    (define var-mode (Arg->var-mode arg))
                    (hash-set out x (var-info (cify x) var-mode))))
       (define args-ast (add-between
                         (for/list ([arg (in-list as)])
                           (match-define (Arg x ty _) arg)
                           (match-define (var-info x* mode) (hash-ref ρ* x))
                           (list* (compile-type ty mode) #\space x*))
                         ", "))
       (define ret-x* (cify ret-x))
       (define ret-x-info (var-info ret-x* 'copy))
       (define ret-lab* (cify ret-lab))
       (define γ (hasheq ret-lab ret-lab*))
       (list* (compile-type ret-ty) #\space fun-name "(" args-ast "){" ind++ ind-nl
              (compile-decl ret-ty ret-x*) ind-nl
              (compile-stmt γ (hash-set ρ* ret-x ret-x-info) body) ind-nl
              ret-lab* ":" ind-nl
              "return " ret-x* ";"
              ind-- ind-nl "}"))]))

(define (compile-program prog)
  (match-define (Program gs private->public n->f) prog)
  ;; XXX Need to construct immutable ρ from given value?
  (define ρ (make-immutable-hash
             (for/list ([(priv pub) (in-hash private->public)])
               (cons priv (var-info pub 'copy)))))
  (define Σ (make-hash (for/list ([(x f) (in-hash n->f)])
                         (cons (unpack-MetaFun f) x))))
  (define pub-funs (list->set (hash-keys Σ)))
  (define fun-queue (make-queue))
  (for ([f (in-hash-keys Σ)])
    (enqueue! fun-queue f))
  (with-cify-counter
    (parameterize ([current-fun-queue fun-queue]
                   [current-fun-graph (unweighted-graph/directed empty)]
                   [current-Σ Σ])
      (define globals-ast (for/list ([(x g) (in-hash gs)])
                            (match-define (Global ty xi) g)
                            (compile-decl ty (hash-ref ρ x) xi)))
      (define fun-table
        (let loop ([table (hash)])
          (cond [(queue-empty? fun-queue) table]
                [else
                 (define f (dequeue! fun-queue))
                 (define static? (not (set-member? pub-funs f)))
                 (define ast (list* (and static? "static ") (compile-fun ρ f) ind-nl))
                 (loop (hash-set table f ast))])))
      (define fun-graph (current-fun-graph))
      (define funs-ast (list*
                        (for/list ([f (in-list (tsort fun-graph))])
                          (hash-ref fun-table f))
                        (for/list ([f (in-set pub-funs)]
                                   #:when (not (has-vertex? fun-graph f)))
                          (hash-ref fun-table f))))
      (define headers-ast (for/list ([h (in-set (current-headers))])
                            (list* "#include <" h ">" ind-nl)))
      (list* headers-ast
             globals-ast ind-nl
             funs-ast ind-nl))))
    
;; Display code

(struct ind-token ())
(define ind-nl (ind-token))
(define ind++ (ind-token))
(define ind-- (ind-token))
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

(define (compile-binary prog c-path out-path #:shared? [shared? #f])
  (parameterize ([current-libs (mutable-set)]
                 [current-headers (mutable-set)])
    (include-src! stdint-h)
    (with-output-to-file c-path #:mode 'text #:exists 'replace
      (λ () (tree-for idisplay (compile-program prog))))
    (define libs (for/list ([l (in-set (current-libs))])
                   (format "-l~a" l)))
    (define args (list* "-o" out-path "-xc" c-path libs))
    (define args* (if shared? (list* "-shared" "-fPIC" args) args))
    (apply system* (find-executable-path "cc") args*)))

(define (compile-library prog c-path out-path)
  (compile-binary prog c-path out-path #:shared? #t))

(define (compile-exe prog c-path out-path)
  (compile-binary prog c-path out-path))

(provide
 (contract-out
  [compile-library (-> Program? path? path? boolean?)]
  [compile-exe (-> Program? path? path? boolean?)]))
