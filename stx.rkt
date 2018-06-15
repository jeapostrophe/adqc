#lang racket/base
(require racket/contract/base
         racket/list
         racket/match
         syntax/parse/define
         racket/stxparam
         (for-syntax racket/base
                     syntax/parse
                     racket/list
                     racket/syntax
                     racket/dict
                     syntax/id-table)
         "ast.rkt")

;; XXX This module should use plus not ast (i.e. the thing that does
;; type checking, termination checking, and resource analysis). And
;; plus should have an "any" type that causes inference, and that
;; should be supported here.

;; XXX Should these macros record the src location in the data
;; structure some how? (perhaps plus should do that with meta?)

;; XXX Use remix for #%dot and #%braces

;; XXX fix bound occurrences

(define-syntax-rule (define-expanders&macros
                      S-free-macros define-S-free-syntax
                      S-expander define-S-expander)
  (begin
    (begin-for-syntax
      (define S-free-macros (make-free-id-table))
      (struct S-expander (impl)
        #:property prop:procedure (struct-field-index impl)))
    (define-simple-macro (define-S-free-syntax id impl)
      (begin-for-syntax (dict-set! S-free-macros #'id impl)))
    (define-simple-macro (define-S-expander id impl)
      (define-syntax id (S-expander impl)))))

(define-expanders&macros
  T-free-macros define-T-free-syntax
  T-expander define-T-expander)
(define-syntax (T stx)
  (syntax-parse stx
    #:literals (unsyntax)
    ;; XXX should array, record, and union be literals?
    [(_ ((~datum array) dim:nat elem))
     (syntax/loc stx (ArrT dim (T elem)))]
    [(_ ((~datum record) (~seq f:id ft) ...))
     (syntax/loc stx (RecT (make-immutable-hasheq (list (cons 'f (T ft)) ...))))]
    [(_ ((~datum union) (~seq m:id mt) ...))
     (syntax/loc stx (UniT (make-immutable-hasheq (list (cons 'm (T mt)) ...))))]
    [(_ (~and macro-use (~or macro-id:id (macro-id:id . _))))
     #:when (dict-has-key? T-free-macros #'macro-id)
     ((dict-ref T-free-macros #'macro-id) #'macro-use)]
    [(_ (~and macro-use (~or macro-id (macro-id . _))))
     #:declare macro-id (static T-expander? "T expander")
     ((attribute macro-id.value) #'macro-use)]
    [(_ (unsyntax e)) #'e]))

(define-expanders&macros
  P-free-macros define-P-free-syntax
  P-expander define-P-expander)
(define-syntax (P stx)
  (syntax-parse stx
    [(_ (p ... (~datum @) e))
     (syntax/loc stx (Select (P (p ...)) (E e)))]
    [(_ (p ... (~datum ->) f:id))
     (syntax/loc stx (Field (P (p ...)) 'f))]
    [(_ (p ... (~datum as) m:id))
     (syntax/loc stx (Mode (P (p ...)) 'm))]
    [(_ (unsyntax e)) #'e]
    [(_ (~and macro-use (~or macro-id:id (macro-id:id . _))))
     #:when (dict-has-key? P-free-macros #'macro-id)
     ((dict-ref P-free-macros #'macro-id) #'macro-use)]
    [(_ (~and macro-use (~or macro-id (macro-id . _))))
     #:declare macro-id (static P-expander? "P expander")
     ((attribute macro-id.value) #'macro-use)]
    [(_ x:id) #'x]
    [(_ (x:id)) #'x]))

(define-expanders&macros
  E-free-macros define-E-free-syntax
  E-expander define-E-expander)

(begin-for-syntax
  (define-literal-set E-bin-op
    #:datum-literals (
                      iadd isub imul isdiv iudiv isrem iurem ishl ilshr iashr ior iand
                      ixor ieq ine iugt isgt iuge isge iult islt iule isle

                      fadd fsub fmul fdiv frem foeq fogt foge folt fole fone fueq fugt
                      fuge fult fule fune ffalse ftrue ford funo)
    ())
  (define E-bin-op? (literal-set->predicate E-bin-op)))

(define-syntax (E stx)
  (syntax-parse stx
    #:literals (if let unsyntax)
    ;; XXX recognize literal numbers and find the smallest type
    ;; XXX make generic binops that look at the types and determine the operation
    [(_ (op:id l r))
     #:when (E-bin-op? #'op)
     (syntax/loc stx (BinOp 'op (E l) (E r)))]
    [(_ (e (~datum :) ty))
     (syntax/loc stx (Cast (T ty) (E e)))]
    ;; XXX simultaneous let
    ;; XXX let*
    [(_ (let ([x (~datum :) ty (~datum :=) xe]) be))
     (syntax/loc stx
       (let ([x-id (gensym 'x)]
             [the-ty (T ty)])
         (LetE x-id the-ty (E xe)
               (let ([the-x-ref (Var x-id the-ty)])
                 (let-syntax ([x (P-expander
                                  (syntax-parser [_ #'the-x-ref]))])
                   (E be))))))]
    ;; XXX cond
    ;; XXX and, or, not
    [(_ (if c t f))
     (syntax/loc stx (IfE (E c) (E t) (E f)))]
    [(_ (~and macro-use (~or macro-id:id (macro-id:id . _))))
     #:when (dict-has-key? E-free-macros #'macro-id)
     ((dict-ref E-free-macros #'macro-id) #'macro-use)]
    [(_ (~and macro-use (~or macro-id (macro-id . _))))
     #:declare macro-id (static E-expander? "E expander")
     ((attribute macro-id.value) #'macro-use)]
    [(_ (unsyntax e)) #'e]
    [(_ p) (quasisyntax/loc stx (Read #,(syntax/loc #'p (P p))))]))

(define-simple-macro (define-flo-stx [name:id bits] ...)
  (begin
    (begin
      (define-syntax (name stx) (raise-syntax-error 'name "Illegal outside T or E" stx))
      (define-T-free-syntax name (syntax-parser [_:id #'(FloT bits)]))
      (define-E-free-syntax name
        (syntax-parser
          [(_ n:expr)
           (syntax/loc this-syntax (Flo bits n))]))
      (provide name))
    ...))

(define-flo-stx
  [F32 32]
  [F64 64])

(define-simple-macro (define-int-stx [name:id signed? bits] ...)
  (begin
    (begin
      (define-syntax (name stx) (raise-syntax-error 'name "Illegal outside T or E" stx))
      (define-T-free-syntax name (syntax-parser [_:id #'(IntT signed? bits)]))
      (define-E-free-syntax name
        (syntax-parser
          [(_ n:expr)
           (syntax/loc this-syntax (Int signed? bits n))]))
      (provide name))
    ...))

(define-int-stx
  [S8  #t  8]
  [S16 #t 16]
  [S32 #t 32]
  [S64 #t 64]
  [U8  #f  8]
  [U16 #f 16]
  [U32 #f 32]
  [U64 #f 64])

(define-expanders&macros
  I-free-macros define-I-free-syntax
  I-expander define-I-expander)
;; XXX should undef, zero, array, record, and union be literals?
(define-syntax (I stx)
  (syntax-parse stx #:literals (unsyntax)
                [(_ ((~datum undef) ty)) (syntax/loc stx (UndI (T ty)))]
                [(_ ((~datum zero) ty)) (syntax/loc stx (ZedI (T ty)))]
                [(_ ((~datum array) i ...)) (syntax/loc stx (ArrI (list (I i) ...)))]
                [(_ ((~datum record) (~seq k:id i) ...))
                 (syntax/loc stx (RecI (make-immutable-hasheq (list (cons 'k (I i)) ...))))]
                [(_ ((~datum union) m:id i))
                 (syntax/loc stx (UniI 'm (I i)))]
                [(_ (~and macro-use (~or macro-id:id (macro-id:id . _))))
                 #:when (dict-has-key? I-free-macros #'macro-id)
                 ((dict-ref I-free-macros #'macro-id) #'macro-use)]
                [(_ (~and macro-use (~or macro-id (macro-id . _))))
                 #:declare macro-id (static I-expander? "I expander")
                 ((attribute macro-id.value) #'macro-use)]
                [(_ (unsyntax e)) #'e]
                [(_ x) (syntax/loc stx (ConI (E x)))]))

(define-expanders&macros
  S-free-macros define-S-free-syntax
  S-expander define-S-expander)

(define-syntax-parameter current-return #f)
(define-syntax-parameter current-return-var #f)
(define-syntax-parameter S-in-tail? #f)

(define-syntax (S stx)
  (syntax-parse stx
    #:literals (void error begin define set! if let/ec while return let unsyntax)
    [(_ (void)) (syntax/loc stx (Skip #f))]
    [(_ (void m)) (syntax/loc stx (Skip m))]
    [(_ (error m)) (syntax/loc stx (Fail m))]
    [(_ (begin)) (syntax/loc stx (S (void)))]
    [(_ (begin (define . d) . b)) (syntax/loc stx (S (let (d) . b)))]
    [(_ (begin s)) (syntax/loc stx (S s))]
    [(_ (begin a . d))
     (syntax/loc stx
       (Begin (syntax-parameterize ([S-in-tail? #f])
                (S a))
              (S (begin . d))))]
    [(_ (set! p e)) (syntax/loc stx (Assign (P p) (E e)))]
    [(_ {p (~datum <-) e}) (syntax/loc stx (S (set! p e)))]
    [(_ (if p t f)) (syntax/loc stx (If (E p) (S t) (S f)))]
    [(_ (let/ec k:id . b))
     (syntax/loc stx
       (let ([k-id (gensym 'k)])
         (Let/ec k-id
                 (let ([the-ret (Jump k-id)])
                   (let-syntax ([k (S-expander (syntax-parser [(_) #'the-ret]))])
                     (S (begin . b)))))))]
    [(_ (let ([x:id (~datum :) ty (~datum :=) xi]) . b))
     (syntax/loc stx
       (let ([x-id (gensym 'x)]
             [the-ty (T ty)])
         (Let x-id the-ty (I xi)
              (let ([the-x-ref (Var x-id the-ty)])
                (let-syntax ([x (P-expander
                                 (syntax-parser [_ #'the-x-ref]))])
                  (S (begin . b)))))))]
    [(_ (let ([x:id (~datum :) ty]) . b))
     (syntax/loc stx
       (let ([the-ty (T ty)])
         (S (let ([x : #,the-ty := (undef #,the-ty)]) . b))))]
    [(_ (let ([x:id (~datum :) ty (~datum :=) f (~datum <-) a ...]) . b))
     (syntax/loc stx
       (let ([x-id (gensym 'x)]
             [the-ty (T ty)])
         (Call x-id the-ty f (list (E a) ...)
               (let ([the-x-ref (Var x-id the-ty)])
                 (let-syntax ([x (P-expander
                                  (syntax-parser [_ #'the-x-ref]))])
                   (S (begin . b)))))))]
    [(_ (~and macro-use (~or macro-id:id (macro-id:id . _))))
     #:when (dict-has-key? S-free-macros #'macro-id)
     ((dict-ref S-free-macros #'macro-id) #'macro-use)]
    [(_ (~and macro-use (~or macro-id (macro-id . _))))
     #:declare macro-id (static S-expander? "S expander")
     ((attribute macro-id.value) #'macro-use)]
    [(_ (unsyntax e)) #'e]
    [(_ e ~!)
     #:fail-unless (syntax-parameter-value #'S-in-tail?)
     "Cannot end in expression when not in tail position"
     (syntax/loc stx (S (return e)))]))

(define-S-free-syntax cond
  (syntax-parser
    #:literals (else)
    [(_) (syntax/loc this-syntax (S (void)))]
    [(_ [else . b]) (syntax/loc this-syntax (S (begin . b)))]
    [(_ [q . a] . more)
     (syntax/loc this-syntax (S (if q (begin . a) (cond . more))))]))
(define-S-free-syntax when
  (syntax-parser
    [(_ p . t)
     (syntax/loc this-syntax
       (S (if p (begin . t) (void))))]))
(define-S-free-syntax unless
  (syntax-parser
    [(_ p . f)
     (syntax/loc this-syntax
       (S (if p (void) (begin . f))))]))
(define-S-free-syntax let*
  (syntax-parser
    #:literals ()
    [(_ () . b) (syntax/loc this-syntax (S (begin . b)))]
    [(_ (a . d) . b) (syntax/loc this-syntax (S (let (a) (let* d . b))))]))

(define-S-expander assert!
  (syntax-parser
    [(_
      (~optional (~and #:dyn (~bind [must-be-static? #f]))
                 #:defaults ([must-be-static? #t]))
      (~optional (~seq #:msg p-msg-expr)
                 #:defaults ([p-msg-expr #'#f]))
      p)
     #:with p-e (if (attribute must-be-static?)
                  ;; XXX use struct
                  (syntax/loc #'p (MetaE 'must-be-static p))
                  #'p)
     (syntax/loc this-syntax
       (let ([p-msg (or p-msg-expr (format "~a" 'p))])
         (S (if p-e
              (void (format "Checked: ~a" p-msg))
              (error (format "Failed! ~a" p-msg))))))]))

(define-S-expander while
  (syntax-parser
    [(_ (~optional (~seq #:I I)
                   #:defaults ([I #'(U32 1)]))
        p . b)
     (syntax/loc this-syntax
       ;; XXX use struct
       (MetaS (cons 'while-invariant (E I))
              (While (E p)
                     (syntax-parameterize ([S-in-tail? #f])
                       (S (begin . b))))))]))

(define-S-expander return
  (syntax-parser
    [(_)
     #:fail-unless (syntax-parameter-value #'current-return)
     "Illegal outside of F"
     (syntax/loc this-syntax current-return)]
    [(_ e)
     #:fail-unless (and (syntax-parameter-value #'current-return)
                        (syntax-parameter-value #'current-return-var))
     "Illegal outside of F"
     (syntax/loc this-syntax
       (S (begin (set! current-return-var e) (return))))]))

(begin-for-syntax
  (define-syntax-class Farg
    #:attributes (x ref var arg)
    #:description "function argument"
    [pattern
     ((~optional (~and #:const (~bind [mode #''copy]))
                 #:defaults ([mode #''read-only]))
      x:id (~datum :) ty)
     #:attr ref (generate-temporary #'x)
     #:attr var (syntax/loc this-syntax (Var (gensym 'x) (T ty)))
     #:attr arg (syntax/loc this-syntax (Arg (Var-x ref) (Var-ty ref) mode))])
  (define-syntax-class Fret
    #:attributes (x ref var)
    #:description "function return"
    [pattern
     (x:id (~datum :) ty)
     #:attr ref (generate-temporary #'x)
     #:attr var (syntax/loc this-syntax (Var (gensym 'x) (T ty)))]
    [pattern
     ty
     #:attr x (generate-temporary)
     #:attr ref (generate-temporary #'x)
     #:attr var (syntax/loc this-syntax (Var (gensym 'x) (T ty)))]))
(define-syntax (F stx)
  (syntax-parse stx
    [(_ (a:Farg ...) (~datum :) r:Fret
        (~optional (~seq #:pre pre)
                   #:defaults ([pre #'(S64 1)]))
        (~optional (~seq #:post post)
                   #:defaults ([post #'(S64 1)]))
        (~optional (~seq #:return ret-lab:id)
                   #:defaults ([ret-lab (generate-temporary)]))
        . bs)
     #:fail-when (check-duplicates (syntax->list #'(a.x ...)) bound-identifier=?)
     "All arguments must have unique identifiers"
     (syntax/loc stx
       (let* ([ret-lab-id (gensym 'ret-lab)]
              [a.ref a.var] ...
              [r.ref r.var])
         (let-syntax ([a.x (P-expander (syntax-parser [_ #'a.ref]))] ...)
           (let-values
               ([(the-post the-body)
                 (let-syntax ([r.x (P-expander (syntax-parser [_ #'r.ref]))])
                   (syntax-parameterize
                       ([current-return-var
                         (make-rename-transformer #'r.x)])
                     ;; DESIGN: If the return variable is synthesized,
                     ;; then the post condition COULD look at it via
                     ;; current-return-var, which is a really ugly
                     ;; name. Maybe we name it something better?
                     ;; Alternatively, maybe we want it to be illegal
                     ;; for the post condition to refer to the result
                     ;; in this case, so the user has to specify a
                     ;; name?
                     (values (E post)
                             (let ([the-ret (Jump ret-lab-id)])
                               (let-syntax
                                   ([ret-lab
                                     (S-expander (syntax-parser [(_) #'the-ret]))])
                                 (syntax-parameterize
                                     ([current-return
                                       (make-rename-transformer #'the-ret)]
                                      [S-in-tail? #t])
                                   (S (begin . bs))))))))])
             (MetaFun
              ;; XXX use struct
              (vector 'fun-invariants (E pre) the-post)
              (IntFun (list a.arg ...)
                      (Var-x r.ref) (Var-ty r.ref)
                      ret-lab-id the-body))))))]))

(define-syntax (include-fun stx)
  (syntax-parse stx
    [(_ #:maybe n:expr f:expr)
     (if (syntax-parameter-value #'current-Prog)
       (syntax/loc stx
         (hash-set! (Program-name->fun current-Prog) n f))
       #'(void))]
    [(_ n:expr f:expr)
     #:fail-unless (syntax-parameter-value #'current-Prog)
     "Cannot include function outside of Prog"
     (syntax/loc stx (include-fun #:maybe n f))]
    [(_ x:id)
     (syntax/loc stx (include-fun (symbol->string 'x) x))]
    [(_ #:maybe x:id)
     (syntax/loc stx (include-fun #:maybe (symbol->string 'x) x))]))

(define-syntax (define-fun stx)
  (syntax-parse stx
    [(_ x:id #:as n:expr . more)
     (quasisyntax/loc stx
       (begin (define x #,(syntax/loc #'more (F . more)))
              (include-fun #:maybe n x)))]
    [(_ x:id . more)
     (quasisyntax/loc stx
       (define-fun x #:as (symbol->string 'x) . more))]
    [(_ (x:id . args) . more)
     (quasisyntax/loc stx
       (define-fun x . #,(syntax/loc #'args (args . more))))]))

(define-syntax (define-extern-fun stx)
  (syntax-parse stx
    [(_ x:id
        (~optional (~seq #:name name:expr)
                   #:defaults ([name #'(symbol->string 'x)]))
        (a:Farg ...)
        (~datum :) ret-ty:expr
        #:src es:expr)
     (syntax/loc stx
       (define x
         (ExtFun es (let ([a.ref a.var] ...) (list a.arg ...))
                 ret-ty name)))]))

(define-syntax (define-global stx)
  (syntax-parse stx
    [(_ (~optional (~and #:public (~bind [public? #t]))
                   #:defaults ([public? #f]))
        x:id (~datum :) ty (~datum :=) xi)
     #:do [(define inside-Prog? (syntax-parameter-value #'current-Prog))]
     #:fail-when (and (attribute public?) (not inside-Prog?))
     "Cannot define public global outside of Prog"
     #:with make-public
     (and inside-Prog?
          (syntax/loc stx
            (hash-set! (Program-private->public current-Prog)
                       'x (symbol->string 'x))))
     #:with install-in-Prog
     (if inside-Prog?
       (syntax/loc stx
         (begin (hash-set! (Program-globals current-Prog) 'x x)
                make-public))
       #'(void))
     (syntax/loc stx
       (begin (define x (Global (T ty) (I xi)))
              install-in-Prog))]))

(define-syntax-parameter current-Prog #f)
(define-syntax (Prog stx)
  (syntax-parse stx
    [(_ pf ...)
     (syntax/loc stx
       (let ([the-prog (Program (make-hasheq) (make-hasheq) (make-hash))])
         (syntax-parameterize ([current-Prog (make-rename-transformer #'the-prog)])
           pf ...)
         the-prog))]))

(provide T P E I
         while assert! return S
         define-S-free-syntax define-S-expander
         F
         define-fun include-fun define-extern-fun define-global
         Prog)

;; XXX Array Slice
;; XXX data types

;; XXX try using multiscope2 -- https://github.com/michaelballantyne/multiscope2
