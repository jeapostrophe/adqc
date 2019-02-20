#lang racket/base
(require racket/contract/base
         racket/list
         racket/match
         racket/require
         racket/stxparam
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     racket/list
                     racket/syntax
                     racket/dict
                     syntax/id-table)
         (subtract-in "ast.rkt" "type.rkt")
         "type.rkt")

;; XXX This module should use plus not ast (i.e. the thing that does
;; type checking, termination checking, and resource analysis). And
;; plus should have an "any" type that causes inference, and that
;; should be supported here.

;; XXX Should these macros record the src location in the data
;; structure some how? (perhaps plus should do that with meta?)

;; XXX Use remix for #%dot and #%braces

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
  (with-disappeared-uses
    (syntax-parse stx
      #:literals (unsyntax)
      ;; XXX should array, record, and union be literals?
      [(_ ((~datum array) dim elem))
       (syntax/loc stx (ArrT dim (T elem)))]
      [(_ ((~datum record) (~seq f:id ft) ...))
       ;; XXX syntax for choosing C stuff
       ;; XXX smarter defaults
       (syntax/loc stx (RecT (make-immutable-hasheq (list (cons 'f (T ft)) ...))
                             (make-immutable-hasheq
                              (list (cons 'f (cify 'f)) ...))
                             '(f ...)))]
      [(_ ((~datum union) (~seq m:id mt) ...))
       ;; XXX syntax for choosing C stuff
       ;; XXX smarter defaults
       (syntax/loc stx (UniT (make-immutable-hasheq (list (cons 'm (T mt)) ...))
                             (make-immutable-hasheq
                              (list (cons 'm (cify 'm)) ...))))]
      [(_ (~and macro-use (~or macro-id:id (macro-id:id . _))))
       #:when (dict-has-key? T-free-macros #'macro-id)
       (record-disappeared-uses #'macro-id)
       ((dict-ref T-free-macros #'macro-id) #'macro-use)]
      [(_ (~and macro-use (~or macro-id (macro-id . _))))
       #:declare macro-id (static T-expander? "T expander")
       (record-disappeared-uses #'macro-id)
       ((attribute macro-id.value) #'macro-use)]
      [(_ (unsyntax e))
       (record-disappeared-uses #'unsyntax)
       #'e])))

(define-expanders&macros
  P-free-macros define-P-free-syntax
  P-expander define-P-expander)
(define-syntax (P stx)
  (with-disappeared-uses
    (syntax-parse stx
      #:literals (unsyntax)
      [(_ (p ... (~datum @) e))
       (syntax/loc stx (Select (P (p ...)) (E e)))]
      [(_ (p ... (~datum ->) f:id))
       (syntax/loc stx (Field (P (p ...)) 'f))]
      [(_ (p ... (~datum as) m:id))
       (syntax/loc stx (Mode (P (p ...)) 'm))]
      [(_ (unsyntax e))
       (record-disappeared-uses #'unsyntax)
       #'e]
      [(_ (~and macro-use (~or macro-id:id (macro-id:id . _))))
       #:when (dict-has-key? P-free-macros #'macro-id)
       (record-disappeared-uses #'macro-id)
       ((dict-ref P-free-macros #'macro-id) #'macro-use)]
      [(_ (~and macro-use (~or macro-id (macro-id . _))))
       #:declare macro-id (static P-expander? "P expander")
       (record-disappeared-uses #'macro-id)
       ((attribute macro-id.value) #'macro-use)]
      [(_ x:id) #'x]
      [(_ (x:id)) #'x])))

(define (construct-number ty n)
  (match ty
    [(IntT signed? bits)
     (define min (cond [signed? (- (expt 2 (sub1 bits)))]
                       [else 0]))
     (define max (sub1 (expt 2 (cond [signed? (sub1 bits)]
                                     [else bits]))))
     (cond
       [(< n min)
        (error 'construct-number "integer value ~a too small for type ~v" n ty)]
       [(> n max)
        (error 'construct-number "integer value ~a too large for type ~v" n ty)]
       [else
        (Int signed? bits n)])]
    [(FloT bits)
     (unless (or (and (single-flonum? n) (= bits 32))
                 (and (double-flonum? n) (= bits 64)))
       (error 'construct-number "floating-point value ~a will not fit type ~v" n ty))
     (Flo bits n)]
    [#f (cond
          [(single-flonum? n) (Flo 32 n)]
          [(double-flonum? n) (Flo 64 n)]
          [(exact-integer? n)
           (define 2^7  (expt 2 7))
           (define 2^15 (expt 2 15))
           (define 2^31 (expt 2 31))
           (define 2^63 (expt 2 63))
           (unless (and (< n (expt 2 64))
                        (>= n (- 2^63)))
             (error 'construct-number "~a is too large to fit in 64 bits" n))
           (define unsigned? (>= n 2^63))
           (define bits
             (cond [(and (< n 2^7)  (>= n (- 2^7)))   8]
                   [(and (< n 2^15) (>= n (- 2^15))) 16]
                   [(and (< n 2^31) (>= n (- 2^31))) 32]
                   [else 64]))
           (Int (not unsigned?) bits n)])]))

(define-syntax-parameter expect-ty #f)

(define-syntax (N stx)
  (syntax-parse stx
    [(_ n)
     #:with ty (or (syntax-parameter-value #'expect-ty) #'#f)
     (syntax/loc stx
       (construct-number ty n))]))

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

(define (Let*E xs tys xes be)
  (cond [(empty? xs) be]
        [else
         (LetE (first xs) (first tys) (first xes)
               (Let*E (rest xs) (rest tys) (rest xes) be))]))

(define-syntax (E stx)
  (with-disappeared-uses
    (syntax-parse stx
      #:literals (if let unsyntax)
      ;; XXX make generic binops that look at the types and determine the operation
      [(_ (op:id l r))
       #:when (E-bin-op? #'op)
       (syntax/loc stx (BinOp 'op (E l) (E r)))]
      [(_ (e (~datum :) ty))
       (syntax/loc stx (Cast (T ty) (E e)))]
      [(_ (let ([x (~datum :) xty (~datum :=) xe] ...) be))
       #:with (x-id ...) (generate-temporaries #'(x ...))
       #:with (the-ty ...) (generate-temporaries #'(xty ...))
       (record-disappeared-uses #'let)
       (syntax/loc stx
         (let ([x-id 'x-id] ... [the-ty (T xty)] ...)
           (Let*E (list x-id ...)
                  (list the-ty ...)
                  (list (syntax-parameterize ([expect-ty #'the-ty])
                          (E xe)) ...)
                  (let ([the-x-ref (Var x-id the-ty)] ...)
                    (let-syntax ([x (P-expander
                                     (syntax-parser [_ #'the-x-ref]))] ...)
                      (E be))))))]
      [(_ (if c t f))
       (record-disappeared-uses #'if)
       (syntax/loc stx (IfE (E c) (E t) (E f)))]
      [(_ (~and macro-use (~or macro-id:id (macro-id:id . _))))
       #:when (dict-has-key? E-free-macros #'macro-id)
       (record-disappeared-uses #'macro-id)
       ((dict-ref E-free-macros #'macro-id) #'macro-use)]
      [(_ (~and macro-use (~or macro-id (macro-id . _))))
       #:declare macro-id (static E-expander? "E expander")
       (record-disappeared-uses #'macro-id)
       ((attribute macro-id.value) #'macro-use)]
      [(_ (unsyntax e))
       (record-disappeared-uses #'unsyntax)
       #'e]
      [(_ n:number) (syntax/loc stx (N n))]
      [(_ p) (quasisyntax/loc stx (Read #,(syntax/loc #'p (P p))))])))

(define-E-free-syntax cond
  (syntax-parser
    #:literals (else)
    [(_ [else e]) (syntax/loc this-syntax (E e))]
    [(_ [q a] . more)
     (syntax/loc this-syntax (E (if q a (cond . more))))]))
(define-E-free-syntax and
  (syntax-parser
    [(_) (syntax/loc this-syntax (E #t))]
    [(_ e) (syntax/loc this-syntax (E e))]
    [(_ x e ...) (syntax/loc this-syntax (E (if x (and e ...) #f)))]))
(define-E-free-syntax or
  (syntax-parser
    [(_) (syntax/loc this-syntax (E #f))]
    [(_ e) (syntax/loc this-syntax (E e))]
    [(_ x e ...)
     (syntax/loc this-syntax
       (E (let ([tmp x]) (if tmp tmp (or e ...)))))]))
(define-E-free-syntax not
  (syntax-parser
    [(_ e) (syntax/loc this-syntax (E (ieq #f e)))]))
(define-E-free-syntax let*
  (syntax-parser
    [(_ () e) (syntax/loc this-syntax (E e))]
    [(_ (f r ...) e)
     (syntax/loc this-syntax
       (E (let (f) (let* (r ...) e))))]))

(define-simple-macro (define-flo-stx [name:id bits] ...)
  (begin
    (begin
      (define-syntax (name stx) (raise-syntax-error 'name "Illegal outside T or E" stx))
      (define-T-free-syntax name
        (syntax-parser
          [_:id
           (record-disappeared-uses #'name)
           #'(FloT bits)]))
      (define-E-free-syntax name
        (syntax-parser
          [(_ n:expr)
           (record-disappeared-uses #'name)
           (syntax/loc this-syntax
             (construct-number (FloT bits) n))]))
      (provide name))
    ...))

(define-flo-stx
  [F32 32]
  [F64 64])

(define-simple-macro (define-int-stx [name:id signed? bits] ...)
  (begin
    (begin
      (define-syntax (name stx) (raise-syntax-error 'name "Illegal outside T or E" stx))
      (define-T-free-syntax name
        (syntax-parser
          [_:id
           (record-disappeared-uses #'name)
           #'(IntT signed? bits)]))
      (define-E-free-syntax name
        (syntax-parser
          [(_ n:expr)
           (record-disappeared-uses #'name)
           (syntax/loc this-syntax
             (construct-number (IntT signed? bits) n))]))
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
  (with-disappeared-uses
    (syntax-parse stx
      #:literals (unsyntax)
      [(_ ((~datum undef) ty)) (syntax/loc stx (UndI (T ty)))]
      [(_ ((~datum zero) ty)) (syntax/loc stx (ZedI (T ty)))]
      [(_ ((~datum array) i ...)) (syntax/loc stx (ArrI (list (I i) ...)))]
      [(_ ((~datum record) (~seq k:id i) ...))
       (syntax/loc stx (RecI (make-immutable-hasheq (list (cons 'k (I i)) ...))))]
      [(_ ((~datum union) m:id i))
       (syntax/loc stx (UniI 'm (I i)))]
      [(_ (~and macro-use (~or macro-id:id (macro-id:id . _))))
       #:when (dict-has-key? I-free-macros #'macro-id)
       (record-disappeared-uses #'macro-id)
       ((dict-ref I-free-macros #'macro-id) #'macro-use)]
      [(_ (~and macro-use (~or macro-id (macro-id . _))))
       #:declare macro-id (static I-expander? "I expander")
       (record-disappeared-uses #'macro-id)
       ((attribute macro-id.value) #'macro-use)]
      [(_ (unsyntax e))
       (record-disappeared-uses #'unsyntax)
       #'e]
      [(_ x) (syntax/loc stx (ConI (E x)))])))

(define-expanders&macros
  S-free-macros define-S-free-syntax
  S-expander define-S-expander)

(define-syntax-parameter current-return #f)
(define-syntax-parameter current-return-var #f)
(define-syntax-parameter S-in-tail? #f)

(define-syntax (S stx)
  (with-disappeared-uses
    (syntax-parse stx
      #:literals (void error begin define set! if let/ec while return let unsyntax)
      [(_ (void))
       (record-disappeared-uses #'void)
       (syntax/loc stx (Skip #f))]
      [(_ (void m))
       (record-disappeared-uses #'void)
       (syntax/loc stx (Skip m))]
      [(_ (error m))
       (record-disappeared-uses #'error)
       (syntax/loc stx (Fail m))]
      [(_ (begin))
       (record-disappeared-uses #'begin)
       (syntax/loc stx (S (void)))]
      [(_ (begin (define . d) . b))
       ;; XXX define is not getting bound
       (record-disappeared-uses (list #'begin #'define))
       (syntax/loc stx (S (let (d) . b)))]
      [(_ (begin s))
       (record-disappeared-uses #'begin)
       (syntax/loc stx (S s))]
      [(_ (begin a . d))
       (record-disappeared-uses #'begin)
       (syntax/loc stx
         (Begin (syntax-parameterize ([S-in-tail? #f])
                  (S a))
                (S (begin . d))))]
      [(_ (set! p e))
       (record-disappeared-uses #'set!)
       (syntax/loc stx (Assign (P p) (E e)))]
      [(_ {p (~datum <-) e}) (syntax/loc stx (S (set! p e)))]
      [(_ (if p t f))
       (record-disappeared-uses #'if)
       (syntax/loc stx (If (E p) (S t) (S f)))]
      [(_ (let/ec k:id . b))
       #:with k-id (generate-temporary #'k)
       (record-disappeared-uses #'let/ec)
       (syntax/loc stx
         (let ([k-id 'k-id])
           (Let/ec k-id
                   (let ([the-ret (Jump k-id)])
                     (let-syntax ([k (S-expander (syntax-parser [(_) #'the-ret]))])
                       (S (begin . b)))))))]
      [(_ (let ([x:id (~datum :) ty (~datum :=) xi]) . b))
       #:with x-id (generate-temporary #'x)
       (record-disappeared-uses #'let)
       (syntax/loc stx
         (let ([x-id 'x-id]
               [the-ty (T ty)])
           (Let x-id the-ty (syntax-parameterize ([expect-ty #'the-ty])
                              (I xi))
                (let ([the-x-ref (Var x-id the-ty)])
                  (let-syntax ([x (P-expander
                                   (syntax-parser [_ #'the-x-ref]))])
                    (S (begin . b)))))))]
      [(_ (let ([x:id (~datum :) ty]) . b))
       (record-disappeared-uses #'let)
       (syntax/loc stx
         (let ([the-ty (T ty)])
           (S (let ([x : #,the-ty := (undef #,the-ty)]) . b))))]
      [(_ (let ([x:id (~datum :) ty (~datum :=) f (~datum <-) a ...]) . b))
       #:with x-id (generate-temporary #'x)
       (record-disappeared-uses #'let)
       (syntax/loc stx
         (let ([x-id 'x-id]
               [the-ty (T ty)])
           (Call x-id the-ty f (list (E a) ...)
                 (let ([the-x-ref (Var x-id the-ty)])
                   (let-syntax ([x (P-expander
                                    (syntax-parser [_ #'the-x-ref]))])
                     (S (begin . b)))))))]
      [(_ (~and macro-use (~or macro-id:id (macro-id:id . _))))
       #:when (dict-has-key? S-free-macros #'macro-id)
       (record-disappeared-uses #'macro-id)
       ((dict-ref S-free-macros #'macro-id) #'macro-use)]
      [(_ (~and macro-use (~or macro-id (macro-id . _))))
       #:declare macro-id (static S-expander? "S expander")
       (record-disappeared-uses #'macro-id)
       ((attribute macro-id.value) #'macro-use)]
      [(_ (unsyntax e))
       (record-disappeared-uses #'unsyntax)
       #'e]
      [(_ e ~!)
       #:fail-unless (syntax-parameter-value #'S-in-tail?)
       "Cannot end in expression when not in tail position"
       (syntax/loc stx (S (return e)))])))

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
     (record-disappeared-uses #'assert!)
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
     (record-disappeared-uses #'while)
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
     (record-disappeared-uses #'return)
     (syntax/loc this-syntax current-return)]
    [(_ e)
     #:fail-unless (and (syntax-parameter-value #'current-return)
                        (syntax-parameter-value #'current-return-var))
     "Illegal outside of F"
     (record-disappeared-uses #'return)
     (syntax/loc this-syntax
       (S (begin (set! current-return-var e) (return))))]))

(begin-for-syntax
  (define-syntax-class Farg
    #:attributes (x ref var arg)
    #:description "function argument"
    [pattern
     ((~optional (~or (~and #:copy (~bind [mode #''copy]))
                      (~and #:ref (~bind [mode #''ref])))
                 #:defaults ([mode #''read-only]))
      x:id (~datum :) ty)
     #:attr ref (generate-temporary #'x)
     #:attr var (syntax/loc this-syntax (Var 'ref (T ty)))
     #:attr arg (syntax/loc this-syntax (Arg (Var-x ref) (Var-ty ref) mode))])
  (define-syntax-class Fret
    #:attributes (x ref var)
    #:description "function return"
    [pattern
     (x:id (~datum :) ty)
     #:attr ref (generate-temporary #'x)
     #:attr var (syntax/loc this-syntax (Var 'ref (T ty)))]
    [pattern
     ty
     #:attr x (generate-temporary)
     #:attr ref (generate-temporary #'x)
     #:attr var (syntax/loc this-syntax (Var 'ref (T ty)))]))
(define-syntax (F stx)
  (with-disappeared-uses
    (syntax-parse stx
      [(_ (a:Farg ...) (~datum :) r:Fret
          (~optional (~seq #:pre pre)
                     #:defaults ([pre #'(S64 1)]))
          (~optional (~seq #:post post)
                     #:defaults ([post #'(S64 1)]))
          (~optional (~seq #:return ret-lab:id)
                     #:defaults ([ret-lab (generate-temporary 'return)]))
          . bs)
       #:fail-when (check-duplicates (syntax->list #'(a.x ...)) bound-identifier=?)
       "All arguments must have unique identifiers"
       #:with ret-lab-id (generate-temporary #'ret-lab)
       (syntax/loc stx
         (let* ([ret-lab-id 'ret-lab-id]
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
                        ret-lab-id the-body))))))])))

(define-syntax (include-fun stx)
  (with-disappeared-uses
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
       (syntax/loc stx (include-fun #:maybe (symbol->string 'x) x))])))

(define-syntax (include-ty stx)
  (with-disappeared-uses
    (syntax-parse stx
      [(_ #:maybe n:expr ty:expr)
       (if (syntax-parameter-value #'current-Prog)
           (syntax/loc stx
             (hash-set! (Program-name->ty current-Prog) n ty))
           #'(void))]
      [(_ n:expr ty:expr)
       #:fail-unless (syntax-parameter-value #'current-Prog)
       "Cannot include type outside of Prog"
       (syntax/loc stx (include-ty #:maybe n ty))])))

(define-syntax (define-fun stx)
  (with-disappeared-uses
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
         (define-fun x . #,(syntax/loc #'args (args . more))))])))

(define-syntax (define-extern-fun stx)
  (with-disappeared-uses
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
                   ret-ty name)))])))

(define-syntax (define-global stx)
  (with-disappeared-uses
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
                install-in-Prog))])))

(define-syntax-parameter current-Prog #f)
(define-syntax (Prog stx)
  (with-disappeared-uses
    (syntax-parse stx
      [(_ pf ...)
       (syntax/loc stx
         (let ([the-prog (Program (make-hasheq) (make-hasheq) (make-hash) (make-hash))])
           (syntax-parameterize ([current-Prog (make-rename-transformer #'the-prog)])
             pf ...)
           the-prog))])))

(provide T P N E I
         while assert! return S
         define-S-free-syntax define-S-expander
         F
         define-fun include-fun include-ty define-extern-fun define-global
         Prog)

;; XXX Array Slice
;; XXX data types

;; XXX try using multiscope2 -- https://github.com/michaelballantyne/multiscope2
