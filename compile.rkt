#lang racket/base
(require data/queue
         racket/contract/base
         racket/file
         racket/format
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


(define headers-default (mutable-set "stdint.h"))

(define current-headers (make-parameter headers-default))
(define current-libs (make-parameter (mutable-set)))
(define current-fun-queue (make-parameter (make-queue)))
(define current-Σ (make-parameter (make-hash)))

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

(define (unpack-ExternSrc src)
  (match-define (ExternSrc ls hs) src)
  (for ([l (in-list ls)])
    (set-add! (current-libs) l))
  (for ([h (in-list hs)])
    (set-add! (current-headers) h)))

(define (compile-path ρ path)
  (define (rec path) (compile-path ρ path))
  (match path
    [(Var x ty)
     (values (hash-ref ρ x) ty)]
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
     (list* "((" (compile-type (FloT bits)) ")" (~a val) ")")]
    [(Cast ty e)
     (list* "((" (compile-type ty) ")" (rec e) ")")]
    [(Read path)
     (match-define-values (name _)
       (compile-path ρ path))
     name]
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
     (unpack-ExternSrc src)
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
     (list* "fprintf(stderr, " (~v m) ");" ind-nl
            "exit(1);")]
    [(Assign path e)
     (match-define-values (name _)
       (compile-path ρ path))
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
            (compile-stmt γ (hash-set ρ x x*) bs))]
    [(MetaS _ s)
     (compile-stmt γ ρ s)]
    [(Call x ty f as bs)
     (define Σ (current-Σ))
     (define fun-name
       (cond [(hash-has-key? Σ f)
              (hash-ref Σ f)]
             [else
              (define fun-name (cify 'fun))
              (hash-set! Σ f fun-name)
              (enqueue! (current-fun-queue) f)
              fun-name]))
     (define args-ast
       (add-between
        (for/list ([arg (in-list as)])
          (match arg
            [(? Expr?) (compile-expr ρ arg)]
            [(? Path?) (compile-path ρ arg)]))
        ", "))
     (list* (hash-ref ρ x) " = " fun-name "(" args-ast ");")]))

;; Σ is a renaming environment for public functions
;; ρ is a renaming environment for global variables
(define (compile-fun ρ f)
  (match f
    [(MetaFun _ f) (compile-fun ρ f)]
    [(IntFun as ret-x ret-ty ret-lab body)
     (define fun-name (hash-ref (current-Σ) f))
     (define ρ* (for/fold ([out ρ])
                          ([arg (in-list as)])
                  (define x (Arg-x arg))
                  (hash-set out x (cify x))))
     (define args-ast (add-between
                       (for/list ([arg (in-list as)])
                         (match-define (Arg x ty mode) arg)
                         (list* (compile-type ty) #\space (hash-ref ρ* x)))
                       ", "))
     (define ret-x-name (cify ret-x))
     (define ret-lab-name (cify ret-lab))
     (define γ (hasheq ret-lab ret-lab-name))
     (list* (compile-type ret-ty) #\space fun-name "(" args-ast "){" ind++ ind-nl
            (compile-decl ret-ty ret-x-name) ind-nl
            (compile-stmt γ (hash-set ρ* ret-x ret-x-name) body) ind-nl
            ret-lab-name ":" ind-nl
            "return " ret-x-name ";"
            ind-- ind-nl "}")]))

(define (compile-program prog)
  (match-define (Program gs private->public n->f) prog)
  ;; XXX Need to construct immutable ρ from given value?
  (define ρ (make-immutable-hash (hash->list private->public)))
  (define Σ (make-hash (for/list ([(x f) (in-hash n->f)])
                         (cons (unpack-MetaFun f) x))))
  (define pub-funs (list->set (hash-keys Σ)))
  (define fun-queue (make-queue))
  (for ([f (in-hash-keys Σ)])
    (enqueue! fun-queue f))
  (parameterize ([current-fun-queue fun-queue]
                 [current-Σ Σ]
                 [current-headers headers-default])
    (define globals-ast (for/list ([(x g) (in-hash gs)])
                          (match-define (Global ty xi) g)
                          (compile-decl ty (hash-ref ρ x) xi)))
    (define funs-ast (let loop ([ast empty])
                       (cond
                         [(queue-empty? fun-queue) ast]
                         [else
                          (define next (dequeue! fun-queue))
                          (define static? (not (set-member? pub-funs next)))
                          (loop
                           (list* (and static? "static ") (compile-fun ρ next) ind-nl
                                  ast))])))
    (define headers-ast (for/list ([h (in-set (current-headers))])
                          (list* "#include<" h ">" ind-nl)))
    (list* headers-ast ind-nl
           globals-ast ind-nl
           funs-ast ind-nl)))

(define (compile-program* prog out-path)
  (with-output-to-file out-path #:mode 'text #:exists 'replace
    (λ () (tree-for idisplay (compile-program prog)))))
    
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

;; XXX A function that actually really calls the C compiler with the
;; appropriate -l lines, etc.
(define (compile-binary prog out-path #:shared? [shared? #f])
  ;; XXX: output C code to tmp file so we can read it to debug.
  (define-values (in out) (make-pipe))
  (parameterize ([current-libs (mutable-set)])
    (parameterize ([current-output-port out])
      (tree-for idisplay (compile-program prog)))
    (close-output-port out)
    (define libs (for/list ([l (in-set (current-libs))])
                   (format "-l~a" l)))
    (define args (flatten (list "-o" out-path libs "-xc" "-")))
    (define args* (if shared? (cons "-shared" args) args))
    (parameterize ([current-input-port in])
      (apply system* (find-executable-path "cc") args*))))

(define (compile-binary* prog c-path out-path #:shared? [shared? #f])
  (parameterize ([current-libs (mutable-set)])
    (compile-program* prog c-path)
    (define in (open-input-file c-path))
    (define libs (for/list ([l (in-set (current-libs))])
                   (format "-l~a" l)))
    (define args (flatten (list "-o" out-path libs "-xc" c-path)))
    (define args* (if shared? (cons "-shared" args) args))
    (apply system* (find-executable-path "cc") args*)))

(define (compile-library prog out-path)
  (compile-binary prog out-path #:shared? #t))

(define (compile-exe prog out-path)
  (compile-binary prog out-path))

(provide
 (contract-out
  [compile-binary* (->* (Program? path? path?) (#:shared? boolean?) boolean?)]
  [compile-library (-> Program? path? boolean?)]
  [compile-exe (-> Program? path? boolean?)]))
