#lang racket/base
(require data/queue
         graph
         racket/contract/base
         racket/format
         racket/function
         racket/list
         racket/match
         racket/set
         racket/string
         racket/system
         "ast.rkt"
         (only-in "type.rkt" expr-type))

;; XXX Read through https://queue.acm.org/detail.cfm?id=3212479 and
;; see if there are any changes we should have to our language
;; model. I think a lot of our choices are good because we don't make
;; promises about memory.


(struct ret-info (x lab) #:transparent)

(define current-ref-vars (make-parameter #f))
(define current-ret-info (make-parameter #f))
(define current-headers (make-parameter #f))
(define current-libs (make-parameter #f))
(define current-fun (make-parameter #f))
(define current-fun-queue (make-parameter #f))
(define current-fun-graph (make-parameter #f))
;; Σ is a renaming environment for public functions
(define current-Σ (make-parameter #f))
(define current-type-queue (make-parameter #f))
(define current-type-graph (make-parameter #f))
(define current-type-table (make-parameter #f))
(define current-globals (make-parameter #f))

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

(define ((c-fun ty->name src) ρ a b)
  (include-src! src)
  (define a* (compile-expr ρ a))
  (define b* (compile-expr ρ b))
  (define ty (expr-type a))
  (define name* (hash-ref ty->name ty))
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
          'frem (c-fun (hash (FloT 32) "fmodf" (FloT 64) "fmod") math-h)
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

(define (compile-type ty)
  (define (rec ty) (compile-type ty))
  (match ty
    [(IntT signed? bits)
     (list* (if signed? "" "u") "int" (~a bits) "_t")]
    [(FloT bits)
     (match bits
       [32 "float"]
       [64 "double"])]
    [(ArrT dim ety)
     (list* (rec ety) "*")]
    [(or (? RecT?) (? UniT?))
     (define type-table (current-type-table))
     (define (new-type!)
       (match-define (or (RecT ?->ty _ _) (UniT ?->ty _)) ty)
       (for ([ty* (in-hash-values ?->ty)])
         (when (or (RecT? ty*) (UniT? ty*))
           (add-directed-edge! (current-type-graph) ty* ty)))
       (define x (cify (match ty
                         [(? RecT?) 'rec]
                         [(? UniT?) 'uni])))
       (enqueue! (current-type-queue) ty)
       (hash-set! type-table ty x)
       x)
     (hash-ref type-table ty new-type!)]
    [(ExtT src name)
     (include-src! src)
     name]))

(define (compile-type/ref ty x)
  (match ty
    [(or (? IntT?) (? FloT?))
     (define ref-vars (current-ref-vars))
     ;; current-ref-vars will be #f if we are compiling a global
     ;; variable (this function will be called from outside compile-fun).
     (list* (compile-type ty)
            (and ref-vars (set-member? ref-vars x) "*"))]
    [(ArrT dim ety)
     (compile-type ty)]
    [(or (? RecT?) (? UniT?) (? ArrT?))
     (list* (compile-type ty) "*")]))

(define (compile-path ρ path)
  (define (rec path) (compile-path ρ path))
  (match (unpack-MetaP path)
    [(Var x ty)
     (values (hash-ref ρ x) ty)]
    [(and the-glob (Global ty _))
     (define globals (current-globals))
     (define (new-global!)
       (define x (cify 'glob))
       (hash-set! globals the-glob x)
       x)
     (values (hash-ref globals the-glob new-global!) ty)]
    [(Select path ie)
     (define-values (p-ast pty) (rec path))
     (match-define (ArrT _ ety) pty)
     (values (list* "(" p-ast "[" (compile-expr ρ ie) "])")
             ety)]
    [(Field path f)
     (define-values (p-ast pty) (rec path))
     (match-define (RecT f->ty f->c _) pty)
     ;; XXX This is kind of hacky, treating arrays as a special
     ;; case where we use . instead of ->   ...or maybe C just
     ;; has weird syntax and is this complexity isn't incidental.
     ;; We may have to revist this if more complex programs force
     ;; us to change the way value vs. reference types work.
     (define op (if (Select? (unpack-MetaP path)) "." "->"))
     (values (list* "(" p-ast op (hash-ref f->c f) ")")
             (hash-ref f->ty f))]
    [(Mode path m)
     (define-values (p-ast pty) (rec path))
     (match-define (UniT m->ty m->c) pty)
     (define op (if (Select? (unpack-MetaP path)) "." "->"))
     (values (list* "(" p-ast op (hash-ref m->c m) ")")
             (hash-ref m->ty m))]
    [(ExtVar src n ty)
     (include-src! src)
     (values n ty)]))

(define (compile-path/deref ρ path)
  (define-values (ast ty) (compile-path ρ path))
  ;; ast will be x* if path is an int or float ref.
  (define ref? (set-member? (current-ref-vars) ast))
  (values (if ref? (list* "(*" ast ")") ast)
          ty))

(define (compile-expr ρ e)
  (define (rec e) (compile-expr ρ e))
  (match e
    [(Int signed? bits val)
     (list* "((" (compile-type (IntT signed? bits)) ")" (~a val) ")")]
    [(Flo bits val)
     (define val* (cond [(equal? val +nan.0)
                         (include-src! math-h)
                         "NAN"]
                        [(single-flonum? val)
                         (~a (real->double-flonum val))]
                        [else (~a val)]))
     (list* "((" (compile-type (FloT bits)) ")" val* ")")]
    [(Cast ty e)
     ;; XXX Only for int/float types?
     (list* "((" (compile-type ty) ")" (rec e) ")")]
    [(Read path)
     (match-define-values (ast _)
       (compile-path/deref ρ path))
     ast]
    [(BinOp op L R)
     (define op-fn (hash-ref bin-op-table op))
     (op-fn ρ L  R)]
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
    [(or (? IntT?) (? FloT?) (? ArrT?) (? RecT?) (? UniT?))
     (list* (compile-type/ref ty name) " " name assign ";")]
    [(ExtT src ext)
     (include-src! src)
     (list* ext " " name assign ";")]))

(define (compile-storage ty name [val #f])
  (define assign (and val (list* " = " val)))
  (match ty
    [(or (? IntT?) (? FloT?) (? RecT?) (? UniT?))
     (list* (compile-type ty) " " name assign ";")]
    [(ArrT dim ety)
     (list* (compile-type ety) " " name "[" (~a dim) "]" assign ";")]
    [(ExtT src ext)
     (include-src! src)
     (list* ext " " name assign ";")]
    ))

;; returns (values storage-ast x-init-ast)
;; storage-ast is #f if no storage is required
(define (compile-storage/init ty xi [val #f])
  (cond [(and (or (ArrT? ty) (RecT? ty) (UniT? ty)) (not (ConI? xi)))
         ;; XXX Better name?
         (define storage-x (cify 'mem))
         (values (list* (compile-storage ty storage-x val))
                 (cond [(ArrT? ty) storage-x]
                       [else (list* "(&" storage-x ")")]))]
        [else (values #f val)]))

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
     (match-define-values (path-ast _) (compile-path/deref ρ path))
     (list* path-ast " = " (compile-expr ρ e) ";")]
    [(Begin f s)
     (list* (rec f) ind-nl (rec s))]
    [(If p t f)
     (define tail
       (if (If? (unpack-MetaS f))
           (rec f)
           (list* "{" ind++ ind-nl
                  (rec f)
                  ind-- ind-nl "}")))
     (list* "if " (compile-expr ρ p) " {" ind++ ind-nl
            (rec t)
            ind-- ind-nl "} else " tail)]
    [(While p b)
     (list* "while " (compile-expr ρ p) " {" ind++ ind-nl
            (rec b)
            ind-- ind-nl "}")]
    [(Jump l)
     (define ret-info (current-ret-info))
     (define l* (hash-ref γ l))
     (if (and ret-info (equal? l* (ret-info-lab ret-info)))
         (list* "return " (ret-info-x ret-info) ";")
         (list* "goto " l* ";"))]
    [(Let/ec l b)
     (define cl (cify l))
     (list* (compile-stmt (hash-set γ l cl) ρ b) ind-nl
            cl ":")]
    [(Let x ty xi bs)
     (define-values (storage-ast x-init-ast)
       (compile-storage/init ty xi (compile-init ρ ty xi)))
     (define x* (cify x))
     (list* (and storage-ast (list* storage-ast ind-nl))
            (compile-decl ty x* x-init-ast) ind-nl
            (compile-stmt γ (hash-set ρ x x*) bs))]
    [(MetaS _ s)
     (compile-stmt γ ρ s)]
    [(Call x ty f as bs)
     (define Σ (current-Σ))
     (define f* (unpack-MetaFun f))
     (define (new-fun!)
       (define fun-name (cify 'fun))
       (hash-set! Σ f* fun-name)
       (enqueue! (current-fun-queue) f*)
       fun-name)
     (define fun-name
       (match f*
         [(ExtFun src _ _ name)
          (include-src! src)
          name]
         [(? IntFun?)
          (add-directed-edge! (current-fun-graph) f* (current-fun))
          (hash-ref Σ f* new-fun!)]))
     ;; XXX Awkward to calculate these for each function call as well as
     ;;     inside of compile-fun.
     (define ref-args
       (for/fold ([out (set)]) ([a (in-list (Fun-args f*))])
         (match-define (Arg x ty mode) a)
         (if (and (or (IntT? ty) (FloT? ty)) (eq? mode 'ref))
             (set-add out x)
             out)))
     (define args-ast
       (add-between
        ;; var-* is argument being passed
        ;; arg-* relates to what the function expects of its arguments
        (for/list ([var (in-list as)]
                   [arg (in-list (Fun-args f*))])
          (match (unpack-any var)
            [(or (Read path) (? Path? path))
             (match-define-values (var-ast _)
               (compile-path ρ path))
             (define var-is-ref? (set-member? (current-ref-vars) var-ast))
             (define arg-is-ref? (set-member? ref-args (Arg-x arg)))
             (define-values (deref-var? take-var-addr?)
               (cond [(eq? var-is-ref? arg-is-ref?)
                      (values #f #f)]
                     [(and var-is-ref? (not (arg-is-ref?)))
                      (values #t #f)]
                     [(and (not var-is-ref?) arg-is-ref?)
                      (values #f #t)]))
             (cond [deref-var?
                    (list* "(*" var-ast ")")]
                   [take-var-addr?
                    (list* "(&" var-ast ")")]
                   [else var-ast])]
            [(? Expr?) (compile-expr ρ var)]))
        ", "))
     (define x* (cify x))
     (define call-ast (compile-decl ty x* (list* fun-name "(" args-ast ")")))
     (define body-ast (compile-stmt γ (hash-set ρ x x*) bs))
     (list* call-ast ind-nl
            body-ast)]))

(define (compile-fun mf)
  (define f (unpack-MetaFun mf))
  (match-define (IntFun as ret-x ret-ty ret-lab body) f)
  (parameterize ([current-fun f])
    (define fun-name (hash-ref (current-Σ) f))
    (define-values (ρ ref-args)
      (for/fold ([ρ (hasheq)] [ref-args (set)])
                ([a (in-list as)])
        (match-define (Arg x ty mode) a)
        (define x* (cify x))
        (values (hash-set ρ x x*)
                (cond [(and (or (IntT? ty) (FloT? ty)) (eq? mode 'ref))
                       (set-add ref-args x*)]
                      [else ref-args]))))
    (define ret-x* (cify ret-x))
    (define ret-lab* (cify ret-lab))
    (define γ (hasheq ret-lab ret-lab*))
    (parameterize ([current-ret-info (ret-info ret-x* ret-lab*)]
                   [current-ref-vars ref-args])
      (define args-ast
        (add-between
         (for/list ([a (in-list as)])
           (match-define (Arg x ty _) a)
           (define x* (hash-ref ρ x))
           (list* (compile-type/ref ty x*) " " (hash-ref ρ x)))
         ", "))
      (define decl-part
        (list* (compile-type/ref ret-ty ret-x*) " " fun-name "(" args-ast ")"))
      (define defn-part
        (list* decl-part "{" ind++ ind-nl
               (compile-decl ret-ty ret-x*) ind-nl
               (compile-stmt γ (hash-set ρ ret-x ret-x*) body) ind-nl
               ind-- ind-nl "}"))
      (values decl-part defn-part))))

(define (compile-program prog)
  (match-define (Program n->g n->ty n->f) prog)
  ;; Setup Σ and fun-queue
  (define Σ (make-hash (for/list ([(x f) (in-hash n->f)])
                         (cons (unpack-MetaFun f) x))))
  (define pub-funs (list->set (hash-keys Σ)))
  (define fun-queue (make-queue))
  (for ([f (in-hash-keys Σ)])
    (enqueue! fun-queue f))
  ;; Setup type-table and type-queue
  (define type-table (make-hash (for/list ([(n ty) (in-hash n->ty)])
                                  (cons ty n))))
  (define type-queue (make-queue))
  (for ([ty (in-hash-keys type-table)])
    (enqueue! type-queue ty))
  ;; Setup globals
  (define globals (make-hasheq))
  (for ([(n g) (in-hash n->g)])
    (hash-set! globals g n))
  (with-cify-counter
    (parameterize ([current-fun-queue fun-queue]
                   [current-fun-graph (unweighted-graph/directed empty)]
                   [current-Σ Σ]
                   [current-type-queue type-queue]
                   [current-type-graph (unweighted-graph/directed empty)]
                   [current-type-table type-table]
                   [current-globals globals])
      ;; Functions
      (define pub-fun-decls (make-queue))
      (define f->ast
        (for/hash ([f (in-queue fun-queue)])
          (define static? (not (set-member? pub-funs f)))
          (define-values (decl-ast defn-ast) (compile-fun f))
          (unless static?
            (enqueue! pub-fun-decls (list* decl-ast ";")))
          (values f (list* (and static? "static ") defn-ast ind-nl))))
      (define fun-graph (current-fun-graph))
      (define funs-ast
        (list*
         (for/list ([f (in-list (tsort fun-graph))])
           (hash-ref f->ast f))
         (for/list ([f (in-set pub-funs)]
                    #:when (not (has-vertex? fun-graph f)))
           (hash-ref f->ast f))))
      (define pub-funs-ast (add-between (queue->list pub-fun-decls) ind-nl))
      ;; Globals
      (define pub-globals (hash-values n->g))
      (define pub-global-decls (make-queue))
      (define globals-ast
        (for/list ([(g x) (in-hash globals)])
          (match-define (Global ty xi) g)
          (define-values (storage-ast x-init-ast)
            (compile-storage/init ty xi (compile-init (hasheq) ty xi)))
          (when (set-member? pub-globals g)
            (enqueue! pub-global-decls (list* "extern " (compile-decl ty x))))
          (list* (and storage-ast (list* storage-ast ind-nl))
                 (compile-decl ty x x-init-ast) ind-nl)))
      (define pub-globals-ast (add-between (queue->list pub-global-decls) ind-nl))
      ;; Types
      (define root-types (queue->list (current-type-queue)))
      (define ty->ast
        (for/hash ([ty (in-queue (current-type-queue))])
          (define x (hash-ref (current-type-table) ty))
          (define ast
            (match ty
              [(? ArrT?)
               (list* "typedef " (compile-type ty) " " x ";")]
              [(RecT f->ty f->c c-order)
               (list* "typedef struct {" ind++ ind-nl
                      (add-between
                       (for/list ([f (in-list c-order)])
                         (compile-decl (hash-ref f->ty f) (hash-ref f->c f)))
                       ind-nl)
                      ind-- ind-nl "} " x ";" ind-nl)]
              [(UniT m->ty m->c)
               (list* "typedef union {" ind++ ind-nl
                      (add-between
                       (for/list ([(m ty) (in-hash m->ty)])
                         (compile-storage ty (hash-ref m->c m)))
                       ind-nl)
                      ind-- ind-nl "} " x ";" ind-nl)]))
          (values ty ast)))
      (define types-ast
        (list* (for/list ([ty (in-list (tsort (current-type-graph)))])
                 (hash-ref ty->ast ty))
               (for/list ([ty (in-list root-types)]
                          #:when (not (has-vertex? (current-type-graph) ty)))
                 (hash-ref ty->ast ty))
               (for/list ([(n ty) (in-hash n->ty)])
                 (define def-n (hash-ref type-table ty))
                 (and (not (string=? def-n n))
                      (list* "typedef " def-n " " n ";" ind-nl)))))
      ;; Headers
      (define headers-ast (for/list ([h (in-set (current-headers))])
                            (list* "#include <" h ">" ind-nl)))
      (define c-part
        (list* headers-ast
               types-ast ind-nl
               globals-ast ind-nl
               funs-ast ind-nl))
      (define h-part
        (list* pub-globals-ast ind-nl ind-nl
               pub-funs-ast ind-nl))
      (values h-part c-part))))
    
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

(define (compile-binary shared? prog c-path out-path [h-path #f])
  (parameterize ([current-libs (mutable-set)]
                 [current-headers (mutable-set)])
    (include-src! stdint-h)
    (define-values (h-part c-part) (compile-program prog))
    (with-output-to-file c-path #:mode 'text #:exists 'replace
      (λ () (tree-for idisplay c-part)))
    (when h-path
      (with-output-to-file h-path #:mode 'text #:exists 'replace
        (λ () (tree-for idisplay h-part))))
    (define libs (for/list ([l (in-set (current-libs))])
                   (format "-l~a" l)))
    (define args
      (list*
       ;; error on any warning, except for...
       "-Wall" "-Werror"
       ;; Too pedantic, maybe we should have our own warning for this
       "-Wno-unused-variable"
       ;; We don't emit unused functions so all this flag does is
       ;; complain about unused functions in util.h.
       "-Wno-unused-function"
       "-o" out-path "-xc" c-path libs))
    (define args* (if shared? (list* "-shared" "-fPIC" args) args))
    (apply system* (find-executable-path "cc") args*)))

(define (compile-library prog c-path out-path [h-path #f])
  (compile-binary #t prog c-path out-path h-path))

(define (compile-exe prog c-path out-path [h-path #f])
  (compile-binary #f prog c-path out-path h-path))

(provide
 (contract-out
  [compile-library (->* (Program? path? path?) (path?) boolean?)]
  [compile-exe (->* (Program? path? path?) (path?) boolean?)]))
