#lang racket/base
(require (for-syntax racket/base
                     racket/contract/base
                     racket/dict
                     racket/generic
                     racket/list
                     racket/syntax
                     syntax/id-table)
         racket/contract/base
         racket/list
         racket/match
         racket/require
         racket/runtime-path
         racket/string
         racket/stxparam
         syntax/parse/define
         (subtract-in "ast.rkt" "type.rkt")
         "type.rkt")

;; XXX This module should use plus not ast (i.e. the thing that does
;; type checking, termination checking, and resource analysis). And
;; plus should have an "any" type that causes inference, and that
;; should be supported here.

;; XXX Should these macros record the src location in the data
;; structure some how? (perhaps plus should do that with meta?)

;; XXX Use remix for #%dot and #%braces

(define (snoc l x) (append l (list x)))

(define (keyword->symbol kw)
  (string->symbol (keyword->string kw)))

(define-syntax (define-expanders&macros stx)
  (syntax-parse stx
    [(_ S-free-macros define-S-free-syntax
        S-expander S-expand define-S-expander)
     #:with expander-struct (generate-temporary #'S-expander)
     #:with gen-S-expander (format-id #'S-expander "gen:~a" #'S-expander)
     (syntax/loc stx
       (begin
         (begin-for-syntax
           (define S-free-macros (make-free-id-table))
           (define-generics S-expander [S-expand S-expander stx])
           (struct expander-struct (impl)
             #:extra-constructor-name S-expander
             #:property prop:procedure (struct-field-index impl)
             #:methods gen-S-expander
             [(define (S-expand this stx*) (this stx*))]))
         (define-simple-macro (define-S-free-syntax id impl)
           (begin-for-syntax (dict-set! S-free-macros #'id impl)))
         (define-simple-macro (define-S-expander id impl)
           (define-syntax id (expander-struct impl)))))]))

(define-expanders&macros
  T-free-macros define-T-free-syntax
  T-expander T-expand define-T-expander)
(define-syntax (T stx)
  (with-disappeared-uses
    (syntax-parse stx
      #:literals (unsyntax unsyntax-splicing)
      ;; XXX should array, record, and union be literals?
      [(_ ((~datum array) dim elem))
       (syntax/loc stx (ArrT dim (T elem)))]
      [(_ ((~datum record) (~or (unsyntax-splicing ps)
                                (~and (~seq (~seq f:id ft) ...)
                                      (~bind [ps #'(list (cons 'f (T ft)) ...)])))))
       ;; XXX syntax for choosing C stuff
       ;; XXX smarter defaults
       (syntax/loc stx
         (RecT (make-immutable-hasheq ps)
               (make-immutable-hasheq (for/list ([p (in-list ps)])
                                        (cons (car p) (cify (car p)))))
               (map car ps)))]
      [(_ ((~datum union) (~or (unsyntax-splicing ps)
                               (~and (~seq (~seq m:id mt) ...)
                                     (~bind [ps #'(list (cons 'm (T mt)) ...)])))))
       ;; XXX syntax for choosing C stuff
       ;; XXX smarter defaults
       (syntax/loc stx
         (UniT (make-immutable-hash ps)
               (make-immutable-hash (for/list ([p (in-list ps)])
                                      (cons (car p) (cify (car p)))))))]
      [(_ (~and macro-use (~or macro-id:id (macro-id:id . _))))
       #:when (dict-has-key? T-free-macros #'macro-id)
       (record-disappeared-uses #'macro-id)
       ((dict-ref T-free-macros #'macro-id) #'macro-use)]
      [(_ (~and macro-use (~or macro-id (macro-id . _))))
       #:declare macro-id (static T-expander? "T expander")
       (record-disappeared-uses #'macro-id)
       (T-expand (attribute macro-id.value) #'macro-use)]
      [(_ (unsyntax e))
       (record-disappeared-uses #'unsyntax)
       #'e])))

(define the-void-ref (VoiT))
(define-T-free-syntax void
  (syntax-parser [_ (syntax/loc this-syntax the-void-ref)]))

(define-expanders&macros
  P-free-macros define-P-free-syntax
  P-expander P-expand define-P-expander)
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
       (P-expand (attribute macro-id.value) #'macro-use)]
      [(_ x:id) #'x]
      [(_ (x:id)) #'x])))

;; freenums can be implicitly cast to a larger type by BinOp
(struct freenum-tag ())
(define freenum! (freenum-tag))
(define (freenum? e)
  (match e
    [(MetaE (== freenum!) _) #t]
    [(MetaE _ e) (freenum? e)]
    [(? Expr?) #f]))
(define (freenum e)
  (MetaE freenum! e))

(define (construct-number stx ty n)
  (define (report fmt . vs)
    ;; XXX Maybe these are contract or range exceptions, not syntax errors?
    (raise-syntax-error #f (apply format fmt vs) stx))
  (match ty
    [(IntT signed? bits)
     (define prefix (if signed? #\S #\U))
     (when (< n (if signed? (- (expt 2 (sub1 bits))) 0))
       (report "integer value ~a too small for ~a~a" n prefix bits))
     (when (> n (sub1 (expt 2 (if signed? (sub1 bits) bits))))
       (report "integer value ~a too large for ~a~a" n prefix bits))
     (Int signed? bits n)]
    [(FloT bits)
     (when (and (single-flonum? n) (not (= bits 32)))
       (report "expected single-flonum?\n  given: ~v" n))
     (when (and (double-flonum? n) (not (= bits 64)))
       (report "expected double-flonum?\n  given: ~v" n))
     (Flo bits n)]
    [#f (cond
          [(single-flonum? n) (freenum (Flo 32 n))]
          [(double-flonum? n) (freenum (Flo 64 n))]
          [(exact-integer? n)
           (define 2^7  (expt 2 7))
           (define 2^15 (expt 2 15))
           (define 2^31 (expt 2 31))
           (define 2^63 (expt 2 63))
           (unless (and (< n (expt 2 64)) (>= n (- 2^63)))
             (report "~a is too large to fit in 64 bits" n))
           (define unsigned? (>= n 2^63))
           (define bits
             (cond [(and (< n 2^7)  (>= n (- 2^7)))   8]
                   [(and (< n 2^15) (>= n (- 2^15))) 16]
                   [(and (< n 2^31) (>= n (- 2^31))) 32]
                   [else 64]))
           (freenum (Int (not unsigned?) bits n))])]))

(define-syntax-parameter expect-ty #f)

(define-syntax (N stx)
  (syntax-parse stx
    [(_ n)
     #:with ty (or (syntax-parameter-value #'expect-ty) #'#f)
     (syntax/loc stx (N ty n))]
    [(_ ty n)
     (quasisyntax/loc stx (construct-number #'#,stx ty n))]))

(define-expanders&macros
  E-free-macros define-E-free-syntax
  E-expander E-expand define-E-expander)

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

(define (implicit-castable? to from)
  (match-define (IntT to-signed? to-bits) to)
  (match-define (IntT from-signed? from-bits) from)
  (cond [(and from-signed? (not to-signed?)) #f]
        [else (> to-bits from-bits)]))

(define (make-binop op the-lhs the-rhs)
  (define l-ty (expr-type the-lhs))
  (define r-ty (expr-type the-rhs))
  ;; XXX Should resulting expression be freenum if both
  ;; lhs and rhs are freenum? (If so, only when return
  ;; type is integral)
  ;; XXX This code could examine the actual values of constant
  ;; expressions so it could be even more permissive, allowing
  ;; e.g. for (< (U32 some-val) 0), which right now fails
  ;; because signed values can never be implicitly cast to unsigned.
  (cond [(and (freenum? the-lhs) (implicit-castable? r-ty l-ty))
         (BinOp op (Cast r-ty the-lhs) the-rhs)]
        [(and (freenum? the-rhs) (implicit-castable? l-ty r-ty))
         (BinOp op the-lhs (Cast l-ty the-rhs))]
        [else (BinOp op the-lhs the-rhs)]))

(define-syntax (E stx)
  (with-disappeared-uses
    (syntax-parse stx
      #:literals (if let unsyntax)
      [(_ (op:id l r))
       #:when (E-bin-op? #'op)
       (syntax/loc stx (make-binop 'op (E l) (E r)))]
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
      [(_ (let ([x (~datum :=) xe] ...) be))
       #:with (x-id ...) (generate-temporaries #'(x ...))
       #:with (the-ty ...) (generate-temporaries #'(x ...))
       #:with (the-xe ...) (generate-temporaries #'(xe ...))
       (record-disappeared-uses #'let)
       (syntax/loc stx
         (let* ([x-id 'x-id] ...
                [the-xe (E xe)] ...
                [the-ty (expr-type the-xe)] ...)
           (Let*E (list x-id ...)
                  (list the-ty ...)
                  (list the-xe ...)
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
       (E-expand (attribute macro-id.value) #'macro-use)]
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
    [(_) (syntax/loc this-syntax (N 1))]
    [(_ e) (syntax/loc this-syntax (E e))]
    [(_ x e ...) (syntax/loc this-syntax (E (if x (and e ...) (N 0))))]))
(define-E-free-syntax or
  (syntax-parser
    [(_) (syntax/loc this-syntax (N 0))]
    [(_ e) (syntax/loc this-syntax (E e))]
    [(_ x e ...)
     (syntax/loc this-syntax
       (E (let ([tmp x]) (if tmp tmp (or e ...)))))]))
(define (not* the-e)
  (define e-ty (expr-type the-e))
  (define one
    (match e-ty
      [(? IntT?) 1]
      [(FloT 32) 1.0f0]
      [(FloT 64) 1.0]))
  (E (bitwise-xor #,the-e #,(N e-ty one))))
(define-E-free-syntax not
  (syntax-parser
    [(_ e) (syntax/loc this-syntax (not* (E e)))]))
(define-E-free-syntax let*
  (syntax-parser
    [(_ () e) (syntax/loc this-syntax (E e))]
    [(_ (f r ...) e)
     (syntax/loc this-syntax
       (E (let (f) (let* (r ...) e))))]))
(define (zero?* the-e)
  (define e-ty (expr-type the-e))
  (define zero
    (match e-ty
      [(? IntT?) 0]
      [(FloT 32) 0.0f0]
      [(FloT 64) 0.0]))
  (E (= #,the-e #,(N e-ty zero))))
(define-E-free-syntax zero?
  (syntax-parser
    [(_ e) (syntax/loc this-syntax (zero?* (E e)))]))
(define-E-free-syntax min
  (syntax-parser
    [(_ a b) (syntax/loc this-syntax (E (if (< a b) a b)))]))
(define-E-free-syntax max
  (syntax-parser
    [(_ a b) (syntax/loc this-syntax (E (if (< a b) b a)))]))

(define-syntax (define-E-increment-ops stx)
  (syntax-parse stx
    [(_ [name:id op:id] ...+)
     #:with (name^ ...) (generate-temporaries #'(name ...))
     (syntax/loc stx
       (begin
         (begin
           (define (name^ the-e)
             (define e-ty (expr-type the-e))
             (define one
               (match e-ty
                 [(? IntT?) 1]
                 [(FloT 32) 1.0f0]
                 [(FloT 64) 1.0]))
             (E (op #,the-e #,(N e-ty one))))
           (define-E-free-syntax name
             (syntax-parser
               [(_ e) (syntax/loc this-syntax (name^ (E e)))]))) ...))]))
(define-E-increment-ops [add1 +] [sub1 -])

(define (subtract the-lhs the-rhs)
  (match (expr-type the-lhs)
    [(? IntT?) (E (isub #,the-lhs #,the-rhs))]
    [(? FloT?) (E (fsub #,the-lhs #,the-rhs))]))
(define (negate the-e)
  (define e-ty (expr-type the-e))
  (define zero
    (match e-ty
      [(? IntT?) 0]
      [(FloT 32) 0.0f0]
      [(FloT 64) 0.0]))
  (subtract (N e-ty zero) the-e))
(define-E-free-syntax -
  (syntax-parser
    [(_ e) (syntax/loc this-syntax (negate (E e)))]
    [(_ l r) (syntax/loc this-syntax (subtract (E l) (E r)))]))

(define-syntax (define-free-binop stx)
  (syntax-parse stx
    [(_ name:id [match-clause op:id] ...+)
     #:with name^ (generate-temporary #'name)
     (syntax/loc stx
       (begin
         (define (name^ the-lhs the-rhs)
           (match (expr-type the-lhs)
             [match-clause (E (op #,the-lhs #,the-rhs))] ...))
         (define-E-free-syntax name
           (syntax-parser
             [(_ l r)
              (syntax/loc this-syntax
                (name^ (E l) (E r)))]))))]))
(define-free-binop + [(? IntT?) iadd] [(? FloT?) fadd])
(define-free-binop * [(? IntT?) imul] [(? FloT?) fmul])
(define-free-binop /
  [(IntT #t _) isdiv]
  [(IntT #f _) iudiv]
  [(? FloT?) fdiv])
(define-free-binop modulo
  [(IntT #t _) isrem]
  [(IntT #f _) iurem]
  [(? FloT?) frem])
(define-free-binop bitwise-ior [(? IntT?) ior])
(define-free-binop bitwise-and [(? IntT?) iand])
(define-free-binop bitwise-xor [(? IntT?) ixor])
(define-free-binop = [(? IntT?) ieq] [(? FloT?) foeq])
(define-free-binop <
  [(IntT #t _) islt]
  [(IntT #f _) iult]
  [(? FloT?) folt])
(define-free-binop <=
  [(IntT #t _) isle]
  [(IntT #f _) iule]
  [(? FloT?) fole])
(define-free-binop >
  [(IntT #t _) isgt]
  [(IntT #f _) iugt]
  [(? FloT?) fogt])
(define-free-binop >=
  [(IntT #t _) isge]
  [(IntT #f _) iuge]
  [(? FloT?) foge])

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
           (quasisyntax/loc this-syntax
             (construct-number #'#,this-syntax (FloT bits) n))]))
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
           (quasisyntax/loc this-syntax
             (construct-number #'#,this-syntax (IntT signed? bits) n))]))
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
  I-expander I-expand define-I-expander)
;; XXX should undef, zero, array, record, and union be literals?
(define-syntax (I stx)
  (with-disappeared-uses
    (syntax-parse stx
      #:literals (unsyntax unsyntax-splicing)
      [(_ ((~datum undef) ty)) (syntax/loc stx (UndI (T ty)))]
      [(_ ((~datum zero) ty)) (syntax/loc stx (ZedI (T ty)))]
      [(_ ((~datum array) (~or (unsyntax-splicing is)
                               (~and (~seq i ...)
                                     (~bind [is #'(list (I i) ...)])))))
       (syntax/loc stx (ArrI is))]
      [(_ ((~datum record) (~or (unsyntax-splicing ps)
                                (~and (~seq (~seq k:id i) ...)
                                      (~bind [ps #'(list (cons 'k (I i)) ...)])))))
       (syntax/loc stx (RecI (make-immutable-hasheq ps)))]
      [(_ ((~datum union) (~or (unsyntax m-id)
                               (~and m:id (~bind [m-id #''m]))) i))
       (syntax/loc stx (UniI m-id (I i)))]
      [(_ (~and macro-use (~or macro-id:id (macro-id:id . _))))
       #:when (dict-has-key? I-free-macros #'macro-id)
       (record-disappeared-uses #'macro-id)
       ((dict-ref I-free-macros #'macro-id) #'macro-use)]
      [(_ (~and macro-use (~or macro-id (macro-id . _))))
       #:declare macro-id (static I-expander? "I expander")
       (record-disappeared-uses #'macro-id)
       (I-expand (attribute macro-id.value) #'macro-use)]
      [(_ (unsyntax e))
       (record-disappeared-uses #'unsyntax)
       #'e]
      [(_ x) (syntax/loc stx (ConI (E x)))])))

(begin-for-syntax
  (struct F-expander (impl)
    #:property prop:procedure (struct-field-index impl)))  

(define-syntax-parameter current-return #f)
(define-syntax-parameter current-return-var #f)
(define-syntax-parameter S-in-tail? #f)

(define-expanders&macros
  S-free-macros define-S-free-syntax
  S-expander S-expand define-S-expander)
(define-syntax (S stx)
  (with-disappeared-uses
    (syntax-parse stx
      #:literals (void error begin define set! if let/ec let unsyntax unsyntax-splicing)
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
       (syntax/loc stx
         (let* ([the-p (P p)] [p-ty (path-type the-p)])
           (Assign the-p (syntax-parameterize ([expect-ty #'p-ty])
                           (E e)))))]
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
      ;; let with T/I expander
      [(_ (let ([x:id (~datum :=) (~and ctor-use (ctor-id . _))]) . b))
       #:declare ctor-id (static (and/c T-expander? I-expander?) "T/I expander")
       (record-disappeared-uses #'let)
       (syntax/loc stx (S (let ([x : ctor-id := ctor-use]) . b)))]
      ;; let with function call from F-expander
      [(_ (let ([x:id (~optional (~seq (~datum :) ty) #:defaults ([ty #'#f]))
                      (~datum :=) (fun-id . fun-args)]) . b))
       #:declare fun-id (static F-expander? "F expander")
       #:with ty* (if (syntax->datum #'ty) #'(T ty) #'(Fun-ret-ty fun-id))
       (record-disappeared-uses (list #'let #'fun-id))
       (syntax/loc stx
         (S (let ([x : #,ty* := fun-id <- . fun-args]) . b)))]
      ;; let with implicit type from expr initializaiton
      [(_ (let ([x:id (~datum :=) e]) . b))
       #:with x-id (generate-temporary #'x)
       (record-disappeared-uses #'let)
       (syntax/loc stx
         (let* ([x-id 'x-id]
                [the-e (E e)]
                [the-ty (expr-type the-e)])
           (Let x-id the-ty (ConI the-e)
                (let ([the-x-ref (Var x-id the-ty)])
                  (let-syntax ([x (P-expander
                                   (syntax-parser [_ #'the-x-ref]))])
                    (S (begin . b)))))))]
      ;; let with full type annotation
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
      ;; let with uninitialized variable
      [(_ (let ([x:id (~datum :) ty]) . b))
       (record-disappeared-uses #'let)
       (syntax/loc stx
         (let ([the-ty (T ty)])
           (S (let ([x : #,the-ty := (undef #,the-ty)]) . b))))]
      ;; let with function call
      [(_ (let ([x:id (~optional (~seq (~datum :) ty) #:defaults ([ty #'#f]))
                      (~datum :=) f (~datum <-)
                      (~or (unsyntax-splicing as)
                           (~and (~seq a ...)
                                 (~bind [as #'(list (E a) ...)])))]) . b))
       #:with x-id (generate-temporary #'x)
       #:with ty* (if (syntax->datum #'ty) #'(T ty) #'(Fun-ret-ty f))
       (record-disappeared-uses #'let)
       (syntax/loc stx
         (let ([x-id 'x-id]
               [the-ty ty*])
           (Call x-id the-ty f as
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
       (S-expand (attribute macro-id.value) #'macro-use)]
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

(define-S-free-syntax for
  (syntax-parser
    [(_ ((~optional (~seq #:I I) #:defaults ([I #'(U32 1)]))
         init pred inc) body ...)
     (record-disappeared-uses #'for)
     (syntax/loc this-syntax
       (S (let (init)
            (while #:I I pred body ... inc))))]))

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

(define-expanders&macros
  A-free-macros define-A-free-syntax
  A-expander A-expand define-A-expander)

(struct anf-nv () #:transparent)
(struct anf-void anf-nv (var pre) #:transparent)
(struct anf-let anf-nv (var xe) #:transparent)
(struct anf-call anf-nv (x ty f es) #:transparent)
(struct anf-if anf-nv (var p-arg t-nv t-arg f-nv f-arg) #:transparent)
(struct anf-type anf-nv (var es) #:transparent)
(struct anf-union anf-nv (var m e) #:transparent)

(define listof-nvs? (listof anf-nv?))

;; XXX error, let/ec
(define-syntax (ANF stx)
  (with-disappeared-uses
    (syntax-parse stx
      #:literals (void error begin define set! if let/ec let unsyntax)
      [(_ (void))
       #:with x-id (generate-temporary 'void)
       (record-disappeared-uses #'void)
       (syntax/loc stx
         (let* ([x-id 'x-id] [the-x-ref (Var x-id (T void))])
           (values (list (anf-void the-x-ref #f)) (Read the-x-ref))))]
      [(_ (begin (define . d) . b))
       (record-disappeared-uses (list #'begin #'define))
       (syntax/loc stx (ANF (let (d) . b)))]
      [(_ (begin a))
       (record-disappeared-uses #'begin)
       (syntax/loc stx (ANF a))]
      [(_ (begin a . as))
       (record-disappeared-uses #'begin)
       (syntax/loc stx
         (let-values ([(a-nv a-arg) (ANF a)]
                      [(as-nv as-arg) (ANF (begin . as))])
           (values (append a-nv as-nv) as-arg)))]
      [(_ (set! p a))
       #:with x-id (generate-temporary 'void)
       (record-disappeared-uses #'set!)
       (syntax/loc stx
         (let-values ([(a-nv a-arg) (ANF a)])
           (define x-id 'x-id)
           (define the-x-ref (Var x-id (T void)))
           (values (snoc a-nv (anf-void the-x-ref (Assign (P p) a-arg)))
                   (Read the-x-ref))))]
      [(_ (if p t f))
       #:with x-id (generate-temporary)
       (record-disappeared-uses #'if)
       (syntax/loc stx
         (let-values ([(p-nv p-arg) (ANF p)]
                      [(t-nv t-arg) (ANF t)]
                      [(f-nv f-arg) (ANF f)])
           (define x-id 'x-id)
           (define x-ty (expr-type t-arg))
           (define the-x-ref (Var x-id x-ty))
           (values (snoc p-nv (anf-if the-x-ref p-arg t-nv t-arg f-nv f-arg))
                   (Read the-x-ref))))]
      [(_ (let ([x:id xe] ...) body ...+))
       #:with (x-id ...) (generate-temporaries #'(x ...))
       #:with (xe-ty ...) (generate-temporaries #'(x ...))
       #:with (ref-ty? ...) (generate-temporaries #'(x ...))
       #:with (the-x-ref ...) (generate-temporaries #'(x ...))
       #:with (xe-nv ...) (generate-temporaries #'(x ...))
       #:with (xe-arg ...) (generate-temporaries #'(x ...))
       (record-disappeared-uses #'let)
       (quasisyntax/loc stx
         (let-values ([(xe-nv xe-arg) (ANF xe)] ...)
           (define x-id 'x-id) ...
           (define xe-ty (expr-type xe-arg)) ...
           (when (ormap VoiT? (list xe-ty ...))
             (raise-syntax-error
              'let "new variable cannot be of type void" #'#,stx))
           (define ref-ty? (or (ArrT? xe-ty) (RecT? xe-ty) (UniT? xe-ty))) ...
           (define the-x-ref (if ref-ty? (Read-p xe-arg) (Var x-id xe-ty))) ...
           (define-values (body-nv body-arg)
             (let-syntax ([x (P-expander (syntax-parser [_ #'the-x-ref]))] ...)
               (ANF (begin body ...))))
           (define the-nvs
             (for/list ([arg (in-list (list xe-arg ...))]
                        [x-ref (in-list (list the-x-ref ...))]
                        [ref? (in-list (list ref-ty? ...))]
                        #:unless ref?)
               (anf-let x-ref arg)))
           (values (append xe-nv ... the-nvs body-nv) body-arg)))]
      [(_ (fn as ...))
       #:declare fn (static F-expander? "F expander")
       #:with x-id (generate-temporary #'fn)
       #:with (as-nv ...) (generate-temporaries #'(as ...))
       #:with (as-arg ...) (generate-temporaries #'(as ...))
       (record-disappeared-uses #'fn)
       (syntax/loc stx
         (let-values ([(as-nv as-arg) (ANF as)] ...)
           (define x-id 'x-id)
           (define x-ty (fun-type fn))
           (define the-x-ref (Var x-id x-ty))
           (values (snoc (append as-nv ...)
                         (anf-call x-id x-ty fn (list as-arg ...)))
                   (Read the-x-ref))))]
      [(_ (ty m:keyword a:expr))
       #:declare ty (static (and/c T-expander? I-expander?) "T/I expander")
       #:with x-id (generate-temporary #'ty)
       (record-disappeared-uses #'ty)
       (syntax/loc stx
         (let-values ([(a-nv a-arg) (ANF a)])
           (define x-id 'x-id)
           (define x-ty (T ty))
           (define the-x-ref (Var x-id x-ty))
           (values (snoc a-nv (anf-union the-x-ref (keyword->symbol 'm) a-arg))
                   (Read the-x-ref))))]
      [(_ (ty as ...))
       #:declare ty (static (and/c T-expander? I-expander?) "T/I expander")
       #:with x-id (generate-temporary #'ty)
       #:with (as-nv ...) (generate-temporaries #'(as ...))
       #:with (as-arg ...) (generate-temporaries #'(as ...))
       (record-disappeared-uses #'ty)
       (syntax/loc stx
         (let-values ([(as-nv as-arg) (ANF as)] ...)
           (define x-id 'x-id)
           (define x-ty (T ty))
           (define the-x-ref (Var x-id x-ty))
           (values (snoc (append as-nv ...)
                         (anf-type the-x-ref (list as-arg ...)))
                   (Read the-x-ref))))]
      [(_ (~and macro-use (~or macro-id:id (macro-id:id . _))))
       #:when (dict-has-key? A-free-macros #'macro-id)
       (record-disappeared-uses #'macro-id)
       ((dict-ref A-free-macros #'macro-id) #'macro-use)]
      [(_ (~and macro-use (~or macro-id (macro-id . _))))
       #:declare macro-id (static A-expander? "A expander")
       (record-disappeared-uses #'macro-id)
       (A-expand (attribute macro-id.value) #'macro-use)]
      [(_ (unsyntax a))
       (record-disappeared-uses #'unsyntax)
       (quasisyntax/loc stx
         (let-values ([(nvs arg) a])
           (unless (Expr? arg)
             (raise-syntax-error
              #f "unsyntaxed ANF arg must be Expr" #'#,stx))
           (unless (listof-nvs? nvs)
             (raise-syntax-error
              #f "unsyntaxed ANF nvs must be list of anf-nv" #'#,stx))
           (values nvs arg)))]
      [(_ e) (syntax/loc stx (values '() (E e)))])))

(define (ANF-compose ret-fn nvs arg)
  (define (rec nvs arg) (ANF-compose ret-fn nvs arg))
  (match nvs
    ['() (ret-fn arg)]
    [(cons (anf-void var pre) more)
     (match-define (Var x ty) (unpack-MetaP var))
     (Let x ty (UndI ty) (Begin (or pre (Skip #f)) (rec more arg)))]
    [(cons (anf-let var xe) more)
     (match-define (Var x ty) (unpack-MetaP var))
     (Let x ty (UndI ty)
          (Begin (Assign var xe)
                 (rec more arg)))]
    [(cons (anf-call x ty f es) more)
     (Call x ty f es (rec more arg))]
    [(cons (anf-if var p-arg t-nv t-arg f-nv f-arg) more)
     (match-define (Var x ty) (unpack-MetaP var))
     (define (branch-ret arg)
       (Assign var arg))
     (Let x ty (UndI ty)
          (Begin
            (If p-arg
                (ANF-compose branch-ret t-nv t-arg)
                (ANF-compose branch-ret f-nv f-arg))
            (rec more arg)))]
    [(cons (anf-type var es) more)
     (match-define (Var x ty) (unpack-MetaP var))
     (define body
       (match ty
         [(ArrT dim _)
          (for/fold ([b (rec more arg)])
                    ([i (in-list (reverse (range dim)))]
                     [e (in-list (reverse es))])
            (Begin (Assign (Select var (N i)) e) b))]
         [(RecT _ _ c-order)
          (for/fold ([b (rec more arg)])
                    ([f (in-list (reverse c-order))]
                     [e (in-list (reverse es))])
            (Begin (Assign (Field var f) e) b))]))
     (Let x ty (UndI ty) body)]
    [(cons (anf-union var m e) more)
     (match-define (Var x ty) (unpack-MetaP var))
     (Let x ty (UndI ty)
          (Begin (Assign (Mode var m) e)
                 (rec more arg)))]))

(define-simple-macro (S+ e)
  (let-values ([(nvs arg) (ANF e)])
    (define (ret-fn arg)
      (S (return #,arg)))
    (ANF-compose ret-fn nvs arg)))

(define-simple-macro (define-A-free-ops op:id ...)
  (begin
    (define-A-free-syntax op
      (syntax-parser
        [(_ as (... ...))
         #:with x-id (generate-temporary)
         #:with (as-nv (... ...)) (generate-temporaries #'(as (... ...)))
         #:with (as-arg (... ...)) (generate-temporaries #'(as (... ...)))
         ;; XXX Maybe try to pull some of this out to a phase-0 function?
         (syntax/loc this-syntax
           (let-values ([(as-nv as-arg) (ANF as)] (... ...))
             (define x-id 'x-id)
             (define arg-e (E (op #,as-arg (... ...))))
             (define x-ty (expr-type arg-e))
             (define the-x-ref (Var x-id x-ty))
             (values (snoc (append as-nv (... ...))
                           (anf-let the-x-ref arg-e))
                     (Read the-x-ref))))])) ...))
(define-A-free-ops
  + - * / modulo
  bitwise-ior bitwise-and bitwise-xor
  = < <= > >=
  not zero? min max)

(begin-for-syntax
  (struct E/A-expander (E-impl A-impl)
    #:methods gen:E-expander
    [(define (E-expand this stx)
       ((E/A-expander-E-impl this) stx))]
    #:methods gen:A-expander
    [(define (A-expand this stx)
       ((E/A-expander-A-impl this) stx))]))
(define-simple-macro (define-E/A-expander x:id E-impl A-impl)
  (define-syntax x (E/A-expander E-impl A-impl)))

(define-syntax (define-E/A-binop stx)
  (syntax-parse stx
    [(_ name:id [match-clause op:id] ...+)
     #:with name^ (generate-temporary #'name)
     (syntax/loc stx
       (begin
         (define (name^ the-lhs the-rhs)
           (match (expr-type the-lhs)
             [match-clause (E (op #,the-lhs #,the-rhs))] ...))
         (define-E/A-expander name
           (syntax-parser
             [(_ l r)
              (syntax/loc this-syntax
                (name^ (E l) (E r)))])
           (syntax-parser
             [(_ as (... ...))
              #:with x-id (generate-temporary)
              #:with (as-nv (... ...)) (generate-temporaries #'(as (... ...)))
              #:with (as-arg (... ...)) (generate-temporaries #'(as (... ...)))
              (syntax/loc this-syntax
                (let-values ([(as-nv as-arg) (ANF as)] (... ...))
                  (define x-id 'x-id)
                  (define arg-e (E (name #,as-arg (... ...))))
                  (define x-ty (expr-type arg-e))
                  (define the-x-ref (Var x-id x-ty))
                  (values (snoc (append as-nv (... ...))
                                (anf-let the-x-ref arg-e))
                          (Read the-x-ref))))]))
         (provide name)))]))

(define-E/A-binop << [(? IntT?) ishl])
(define-E/A-binop >> [(IntT #t _) iashr] [(IntT #f _) ilshr])
;; Note: behavior of C's != operator is unordered for floats
(define-E/A-binop != [(? IntT?) ine] [(? FloT?) fune])

(define-simple-macro (define-E/A-aliases [name:id op:id] ...+)
  (begin
    (begin
      (define-E/A-expander name
        (syntax-parser
          [(_ . vs) (syntax/loc this-syntax (E (op . vs)))])
        (syntax-parser
          [(_ . as) (syntax/loc this-syntax (ANF (op . as)))]))
      (provide name)) ...))
(define-E/A-aliases [% modulo] [& bitwise-and] [^ bitwise-xor])

(define-A-free-syntax when
  (syntax-parser
    [(_ p . t)
     (syntax/loc this-syntax
       (ANF (if p (begin . t) (void))))]))
(define-A-free-syntax unless
  (syntax-parser
    [(_ p . f)
     (syntax/loc this-syntax
       (ANF (if p (void) (begin . f))))]))
(define-A-free-syntax let*
  (syntax-parser
    [(_ () a) (syntax/loc this-syntax (ANF a))]
    [(_ (f r ...) a)
     (syntax/loc this-syntax
       (ANF (let (f) (let* (r ...) a))))]))
(define-A-free-syntax cond
  (syntax-parser
    #:literals (else)
    [(_ [else a]) (syntax/loc this-syntax (ANF a))]
    [(_ [q a] . more)
     (syntax/loc this-syntax (ANF (if q a (cond . more))))]))
(define-A-free-syntax and
  (syntax-parser
    [(_) (syntax/loc this-syntax (ANF #,(values '() (N 1))))]
    [(_ a) (syntax/loc this-syntax (ANF a))]
    [(_ a as ...)
     (syntax/loc this-syntax
       (ANF (if a (and as ...) #,(values '() (N 0)))))]))
(define-A-free-syntax or
  (syntax-parser
    [(_) (syntax/loc this-syntax (ANF #,(values '() (N 0))))]
    [(_ a) (syntax/loc this-syntax (ANF a))]
    [(_ a as ...)
     (syntax/loc this-syntax
       (ANF (let ([tmp a]) (if tmp tmp (or as ...)))))]))

(begin-for-syntax
  (struct S/A-expander (S-impl A-impl)
    #:methods gen:S-expander
    [(define (S-expand this stx)
       ((S/A-expander-S-impl this) stx))]
    #:methods gen:A-expander
    [(define (A-expand this stx)
       ((S/A-expander-A-impl this) stx))]))
(define-simple-macro (define-S/A-expander x:id S-impl A-impl)
  (define-syntax x (S/A-expander S-impl A-impl)))

(define-simple-macro (define-S/A-assign-ops [name:id op:id] ...+)
  (begin
    (begin
      (define-S/A-expander name
        (syntax-parser
          [(_ p e)
           (syntax/loc this-syntax
             (S (set! p (op p e))))])
        (syntax-parser
          [(_ p a)
           #:with x-id (generate-temporary 'void)
           (syntax/loc this-syntax
             (let-values ([(a-nv a-arg) (ANF a)])
               (define x-id 'x-id)
               (define the-x-ref (Var x-id (T void)))
               (define the-stmt (S (name p #,a-arg)))
               (values (snoc a-nv (anf-void the-x-ref the-stmt))
                       (Read the-x-ref))))]))
      (provide name)) ...))
(define-S/A-assign-ops [+= +] [-= -] [*= *] [/= /] [%= %] [<<= <<] [>>= >>])

(define-simple-macro (define-S/A-increment-ops [name:id op:id] ...+)
  (begin
    (begin
      (define-S/A-expander name
        (syntax-parser
          [(_ p) (syntax/loc this-syntax (S (set! p (op p))))])
        (syntax-parser
          [(_ p)
           #:with x-id (generate-temporary 'void)
           (syntax/loc this-syntax
             (let* ([x-id 'x-id] [the-x-ref (Var x-id (T void))])
               (values (list (anf-void the-x-ref (S (name p))))
                       (Read the-x-ref))))]))
      (provide name)) ...))
(define-S/A-increment-ops [+=1 add1] [-=1 sub1])

;; XXX 'assert!', 'while', 'for' for ANF.

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
     #:attr arg (syntax/loc this-syntax (Arg (Var-x ref) (Var-ty ref) mode))]
    ;; XXX Delete old syntax when everything is migrated over
    [pattern
     ((~optional (~or (~and #:copy (~bind [mode #''copy]))
                      (~and #:ref (~bind [mode #''ref])))
                 #:defaults ([mode #''read-only]))
      ty x:id)
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
     #:attr x (generate-temporary #'ty)
     #:attr ref (generate-temporary #'x)
     #:attr var (syntax/loc this-syntax (Var 'ref (T ty)))]))

(define-syntax-parameter F-body-default (make-rename-transformer #'S))

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
                                     ;; XXX Make ANF the default here
                                     (F-body-default (begin . bs))))))))])
               (MetaFun
                ;; XXX use struct
                (vector 'fun-invariants (E pre) the-post)
                (IntFun (list a.arg ...)
                        (Var-x r.ref) (Var-ty r.ref)
                        ret-lab-id the-body))))))])))

(define-simple-macro (F+ . more)
  (syntax-parameterize ([F-body-default (make-rename-transformer #'S+)])
    (F . more)))

(begin-for-syntax
  (struct T/I-expander (T-impl I-impl)
    #:property prop:procedure (struct-field-index T-impl)
    #:methods gen:T-expander
    [(define (T-expand this stx)
       ((T/I-expander-T-impl this) stx))]
    #:methods gen:I-expander
    [(define (I-expand this stx)
       ((T/I-expander-I-impl this) stx))]))
(define (apply-union-ctor-init stx ty m i)
  (unless (UniT? ty)
    (raise-syntax-error #f "union syntax used for non-union type" stx))
  (I (union #,m #,i)))
(define (apply-ctor-inits stx ty is)
  (match ty
    [(? ArrT?)
     (I (array #,@is))]
    [(RecT _ _ c-order)
     (unless (= (length c-order) (length is))
       (raise-syntax-error #f "constructor arity mismatch" stx))
     (I (record #,@(map cons c-order is)))]
    [_ (raise-syntax-error #f "invalid constructor syntax" stx)]))
(define-syntax (define-type stx)
  (syntax-parse stx
    [(_ #:public name:id ty-stx)
     #:fail-unless (syntax-parameter-value #'current-Prog)
     "Cannot define public type outside of Prog"
     (syntax/loc stx
       (begin (define-type name ty-stx)
              (include-type name)))]
    [(_ name:id ty-stx)
     (syntax/loc stx
       (begin
         (define ty (T ty-stx))
         (define-syntax name
           (T/I-expander
            (syntax-parser
              [_ (syntax/loc this-syntax ty)])
            (syntax-parser
              [(_ m:keyword i:expr)
               (quasisyntax/loc this-syntax
                 (apply-union-ctor-init
                  #'#,this-syntax ty (keyword->symbol 'm) (I i)))]
              [(_ i:expr (... ...))
               (quasisyntax/loc this-syntax
                 (apply-ctor-inits
                  #'#,this-syntax ty (list (I i) (... ...))))])))))]))

(define-syntax (include-type stx)
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
       (syntax/loc stx (include-type #:maybe n ty))]
      [(_ x:id)
       (syntax/loc stx (include-type (symbol->c-name 'x) x))]
      [(_ #:maybe x:id)
       (syntax/loc stx (include-type #:maybe (symbol->c-name 'x) x))])))

(define-syntax (define-extern-type stx)
  (with-disappeared-uses
    (syntax-parse stx
      [(_ x:id
          (~optional (~seq #:name name:expr)
                     #:defaults ([name #'(symbol->c-name 'x)]))
          (~optional (~seq #:src es:expr)
                     #:defaults ([es #'(ExternSrc '() '())])))
       #:with the-ext (generate-temporary #'x)
       (syntax/loc stx
         (begin
           (define the-ext (ExtT es name))
           (define-syntax x
             (T-expander (syntax-parser [_ #'the-ext])))))])))

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
       (syntax/loc stx (include-fun (symbol->c-name 'x) x))]
      [(_ #:maybe x:id)
       (syntax/loc stx (include-fun #:maybe (symbol->c-name 'x) x))])))

(define-syntax (define-fun stx)
  (with-disappeared-uses
    (syntax-parse stx
      [(_ x:id #:as n:expr . more)
       (quasisyntax/loc stx
         (begin
           (define the-fun #,(syntax/loc #'more (give-name (F . more) 'x)))
           (define-syntax x
             (F-expander (syntax-parser [_ #'the-fun])))
           (include-fun #:maybe n x)))]
      ;; XXX Remove old-style syntax, support '#:as' in new style.
      ;; XXX Change 'F' to use new style syntax. Thinking something like:
      ;; (F ret-ty ([arg-ty arg-x] ...) body ...)
      [(_ ret-ty x:id ([arg-ty arg-x:id] ...) body ...+)
       (syntax/loc stx
         (define-fun (x [arg-x : arg-ty] ...) : ret-ty body ...))]
      [(_ x:id . more)
       (quasisyntax/loc stx
         (define-fun x #:as (symbol->c-name 'x) . more))]
      [(_ (x:id . args) . more)
       (quasisyntax/loc stx
         (define-fun x . #,(syntax/loc #'args (args . more))))])))

(define-syntax (define-extern-fun stx)
  (with-disappeared-uses
    (syntax-parse stx
      [(_ ret-ty x:id
          (~optional (~seq #:name name:expr)
                     #:defaults ([name #'(symbol->c-name 'x)]))
          (a:Farg ...) #:src es:expr)
       (syntax/loc stx
         (begin
           (define the-fun
             (ExtFun es (let ([a.ref a.var] ...) (list a.arg ...))
                     (T ret-ty) name))
           (define-syntax x (F-expander (syntax-parser [_ #'the-fun])))))])))

(define-syntax (define-global stx)
  (with-disappeared-uses
    (syntax-parse stx
      #:literals (unsyntax)
      [(_ #:public name:id . more)
       #:fail-unless (syntax-parameter-value #'current-Prog)
       "Cannot define public global outside of Prog"
       (syntax/loc stx
         (begin (define-global name . more)
                (include-global name)))]
      ;; T/I expander
      [(_ x:id (~datum :=) (~and ctor-use (ctor-id . _)))
       #:declare ctor-id (static (and/c T-expander? I-expander?) "T/I expander")
       (syntax/loc stx
         (define-global x : ctor-id := ctor-use))]
      ;; implicit type from expr initialization
      [(_ x:id (~datum :=) e)
       (syntax/loc stx
         (begin
           (define the-e (E e))
           (define e-ty (expr-type the-e))
           (define the-glob (Global e-ty (ConI the-e)))
           (define-syntax x
             (P-expander (syntax-parser [_ #'the-glob])))))]
      ;; fully annonated
      [(_ x:id (~datum :) ty (~datum :=) xi)
       (syntax/loc stx
         (begin
           (define the-ty (T ty))
           (define the-glob
             (Global the-ty
                     (syntax-parameterize ([expect-ty #'the-ty])
                       (I xi))))
           (define-syntax x
             (P-expander (syntax-parser [_ #'the-glob])))))]
      ;; uninitialied variable
      [(_ x:id (~datum :) ty)
       (syntax/loc stx
         (begin
           (define the-ty (T ty))
           (define-global x : #,the-ty := (undef #,the-ty))))])))

(define-syntax (include-global stx)
  (with-disappeared-uses
    (syntax-parse stx
      [(_ #:maybe n:expr g:expr)
       (if (syntax-parameter-value #'current-Prog)
           (syntax/loc stx
             (hash-set! (Program-name->global current-Prog) n g))
           #'(void))]
      [(_ n:expr g:expr)
       #:fail-unless (syntax-parameter-value #'current-Prog)
       "Cannot include global outside of Prog"
       (syntax/loc stx (include-global #:maybe n g))]
      [(_ x:id)
       (syntax/loc stx (include-global (symbol->c-name 'x) x))]
      [(_ #:maybe x:id)
       (syntax/loc stx (include-global #:maybe (symbol->c-name 'x) x))])))

(define (symbol->c-name x)
  (string-replace (symbol->string x) "-" "_"))

(define-syntax-parameter current-Prog #f)
(define-syntax (Prog stx)
  (with-disappeared-uses
    (syntax-parse stx
      [(_ pf ...)
       (syntax/loc stx
         (let ([the-prog (Program (make-hash) (make-hash) (make-hash))])
           (syntax-parameterize ([current-Prog (make-rename-transformer #'the-prog)])
             pf ... (void))
           the-prog))])))

(begin-for-syntax
  (define-literal-set def-forms
    #:datum-literals (
                      define-type define-fun define-fun+ define-global
                      define-extern-fun define-extern-type
                      include-fun include-type include-global) ())
  (define def-form? (literal-set->predicate def-forms)))

;; XXX Use let-syntaxes to handle multiple return values
;; from partition, instead of let-values?
;; XXX Should inline function definitions be public?
(define-syntax (Prog* stx)
  (with-disappeared-uses
    (syntax-parse stx
      [(_ pf ...)
       #:with ((def ...) (non ...))
       (let-values ([(defs nons)
                     (partition
                      (syntax-parser
                        [(x:id . _) #:when (def-form? #'x) #t]
                        [_ #f])
                      (syntax->list #'(pf ...)))])
         (list defs nons))
       (syntax/loc stx
         (Prog def ...
               (define-fun main () : S32
                 non ...
                 (return 0))))])))

(define-simple-macro (define-prog name:id pf ...+)
  (define name (Prog pf ...)))

(define-simple-macro (define-prog* name:id pf ...)
  (define name (Prog* pf ...)))

(provide while assert! return
         define-T-free-syntax define-T-expander
         define-I-free-syntax define-I-expander
         define-E-free-syntax define-E-expander
         define-S-free-syntax define-S-expander
         F-body-default
         define-type define-fun define-global
         define-extern-fun define-extern-type
         include-fun include-type include-global
         Prog Prog* define-prog define-prog*)

(define-runtime-path util-path "util.h")
(define util-h (ExternSrc '() (list (path->string util-path))))
(define-extern-type char*)
(define-extern-fun S32 c-print-string #:name "print_string"
  ([char* str] [S32 n]) #:src util-h)

(define (print-string s)
  (define bs (string->bytes/utf-8 s))
  (define init (I (array #,@(for/list ([b (in-bytes bs)])
                            (I (U8 b))))))
  (define len (bytes-length bs))
  (S (let* ([str-x : (array len U8) := #,init]
            [ret-x := (c-print-string (str-x : #,char*) (S32 len))])
       (void))))

(define printers (make-weak-hash))
(define array-printers (make-weak-hash))
(define (get-printer ty)
  (define (new-printer!)
    (define print-fn
      (match ty
        ;; XXX UniT, ExtT (?)
        [(RecT _ _ c-order)
         (F ([rec : #,ty]) : S32
            (print "{")
            #,(let loop ([f (first c-order)] [fs (rest c-order)])
                (cond [(empty? fs)
                       (S (print #,(Read (Field (P rec) f))))]
                      [else
                       (S (begin (print #,(Read (Field (P rec) f)) ", ")
                                 #,(loop (first fs) (rest fs))))]))
            (print "}")
            (return 0))]))
    (define value (make-ephemeron ty print-fn))
    (hash-set! printers ty value)
    value)
  (define (new-array-printer!)
    (define ety (ArrT-ety ty))
    (define print-fn
      (F ([arr : #,ty] [count : U64]) : S32
         (let ([i : U64 := (U64 0)])
           (print "[")
           (while (< i count)
                  (print (arr @ i))
                  (define next : U64 := (add1 i))
                  (when (< next count)
                    (print ", "))
                  (set! i next))
           (print "]")
           (return 0))))
    (define value (make-ephemeron ety print-fn))
    (hash-set! array-printers ety value)
    value)
  (match ty
    ;; 'ephemeron-value' may produce #f if the key was GC'd between our
    ;; call to 'hash-ref' and 'ephemeron-value'. In this case, we call
    ;; 'new-*-printer' directly, and we know that the subsequent call
    ;; to 'ephemeron-value' cannot produce #f because the key is used
    ;; within this function (and thus will not be GC'd).
    [(ArrT _ ety)
     (or (ephemeron-value (hash-ref array-printers ety new-array-printer!))
         (ephemeron-value (new-array-printer!)))]
    [_
     (or (ephemeron-value (hash-ref printers ty new-printer!))
         (ephemeron-value (new-printer!)))]))

(define (print-expr the-e)
  (define e-ty (expr-type the-e))
  (define printer
    (cond
      [(or (IntT? e-ty) (FloT? e-ty))
       (define fn-name
         (match e-ty
           [(FloT 32) "print_F32"]
           [(FloT 64) "print_F64"]
           [(IntT signed? bits)
            (if signed?
                (match bits
                  [8 "print_S8"]
                  [16 "print_S16"]
                  [32 "print_S32"]
                  [64 "print_S64"])
                (match bits
                  [8 "print_U8"]
                  [16 "print_U16"]
                  [32 "print_U32"]
                  [64 "print_U64"]))]))
       (ExtFun util-h (list (Arg 'n e-ty 'read-only)) (T S32) fn-name)]
      [else (get-printer e-ty)]))
  (match e-ty
    [(ArrT dim _)
     (S (let ([ret-x := printer <- #,the-e (U64 dim)]) (void)))]
    [_ (S (let ([ret-x := printer <- #,the-e]) (void)))]))

(define-S-free-syntax print
  (syntax-parser
    #:literals (unsyntax)
    [(_ e es ...+)
     (syntax/loc this-syntax
       (S (begin (print e) (print es ...))))]
    [(_ (unsyntax exp-or-str))
     (syntax/loc this-syntax
       (match exp-or-str
         [(? string? s) (print-string s)]
         [(? Expr? the-e) (print-expr the-e)]))]
    [(_ s:str)
     (syntax/loc this-syntax (print-string s))]
    [(_ e)
     (syntax/loc this-syntax (print-expr (E e)))]))
(define-S-free-syntax println
  (syntax-parser
    [(_ es ... e:str)
     (syntax/loc this-syntax (S (print es ... #,(string-append e "\n"))))]
    [(_ es ...)
     (syntax/loc this-syntax (S (print es ... "\n")))]))

(provide T P N E I F F+ S S+)


;; XXX Array Slice
;; XXX data types

;; XXX try using multiscope2 -- https://github.com/michaelballantyne/multiscope2
