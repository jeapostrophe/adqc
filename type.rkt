#lang racket/base
(require (for-syntax racket/base
                     racket/syntax)
         racket/contract/base
         racket/hash
         racket/match
         racket/set
         syntax/parse/define
         "ast.rkt"
         "print.rkt")

(define (snoc l x) (append l (list x)))

;; Type for exceptions raised by the ADQC type checker.
(struct exn:fail:adqc:type exn:fail ())
(provide (struct-out exn:fail:adqc:type))

(define (raise-adqc-type-error fmt args blame cc)
  (define out (open-output-string))
  (print-ast blame out)
  (define msg (apply format fmt (snoc args (get-output-string out))))
  (raise (exn:fail:adqc:type msg cc)))

;; XXX Maybe this isn't the best way to do this. My first instinct is to capture
;; the AST node which is creating the error and print it along with the error
;; message, but this may result in unreadable output for Let, LetE, and Call
;; nodes, as they contain the rest of the function as their body. Maybe have
;; different reporting schemes for syntax which makes declarations and has a body
;; (Let, LetE, Call, IntFun, etc?) and other types of syntax?
(define-syntax (define-reporter stx)
  (syntax-parse stx
    [(_ name:id blame-ast:expr)
     (syntax/loc stx
       (define-syntax (name stx*)
         (syntax-parse stx*
           [(_ msg:str args:expr (... ...))
            #:with msg-full (datum->syntax
                             #'msg (string-append
                                    (syntax->datum #'msg) "\nBlaming AST: ~a"))
            (syntax/loc stx*
              (raise-adqc-type-error
               msg-full (list args (... ...)) blame-ast
               (current-continuation-marks)))])))]))

(struct env-info (env) #:transparent)
(struct type-info env-info (ty) #:transparent)

(define i-arith-ops '(iadd isub imul iudiv isdiv iurem isrem ishl ilshr iashr iand ior ixor))
(define f-arith-ops '(fadd fsub fmul fdiv frem))
(define i-cmp-ops '(ieq ine iugt iuge iult iule isgt isge islt isle))
(define f-cmp-ops '(foeq fone fogt foge folt fole fueq fune
                         fugt fuge fult fule ffalse ftrue ford funo))

(define (type=? L R)
  (or (AnyT? L) (AnyT? R) (equal? L R)))

(define (union-subset? u ty)
  (match-define (UniT m->ty _) u)
  (for/or ([u-ty (in-hash-values m->ty)])
    (cond [(type=? ty u-ty) #t]
          [(UniT? u-ty) (union-subset? u-ty ty)]
          [else #f])))

(define (resolve-type L R)
  (cond [(AnyT? L) R]
        [(AnyT? R) L]
        [(equal? L R) L]
        [else (cond [(and (UniT? L) (union-subset? L R)) L]
                    [(and (UniT? R) (union-subset? R L)) R]
                    [else (error 'resolve-type "type mismatch L: ~v R: ~v" L R)])]))

(define (env-union e0 . es)
  (apply hash-union e0 es #:combine resolve-type))

(define (IntT/any? v)
  (or (IntT? v) (AnyT? v)))

(define (FloT/any? v)
  (or (FloT? v) (AnyT? v)))

(define (expr-type-info e)
  (define-reporter report e)
  (match e
    [(MetaE (? type-info? ti) _) ti]
    [(MetaE _ e) (expr-type-info e)]
    [(Int signed? bits _)
     (type-info (hasheq) (IntT signed? bits))]
    [(Flo bits _)
     (type-info (hasheq) (FloT bits))]
    [(Cast to-ty e)
     (match-define (type-info from-env from-ty)
       (expr-type-info e))
     (unless (or (ExtT? from-ty) (ExtT? to-ty))
       (unless (or (IntT? from-ty) (FloT? from-ty))
         (report "Cast: can't cast from non-numeric type ~v" from-ty))
       (unless (or (IntT? to-ty) (FloT? to-ty))
         (report "Cast: can't cast to non-numeric type ~v" to-ty)))
     (type-info from-env to-ty)]
    [(Read p)
     (path-type-info p)]
    [(BinOp op L R)
     (match-define (type-info L-env L-ty)
       (expr-type-info L))
     (match-define (type-info R-env R-ty)
       (expr-type-info R))
     (unless (type=? L-ty R-ty)
       (report "BinOp: LHS type ~v and RHS type ~v not equal" L-ty R-ty))
     (define the-ty (resolve-type L-ty R-ty))
     (when (or (set-member? i-arith-ops op) (set-member? i-cmp-ops op))
       (unless (IntT/any? the-ty)
         (report "BinOp: integer op expects integral arguments, given ~v, ~v"
                 L-ty R-ty)))
     (when (or (set-member? f-arith-ops op) (set-member? f-cmp-ops op))
       (unless (FloT/any? the-ty)
         (report
          "BinOp: floating-point op expects floating-point arguments, given ~v, ~v"
          L-ty R-ty)))
     ;; XXX add logic for union types.
     (define env (env-union L-env R-env))
     (cond [(or (set-member? i-arith-ops op) (set-member? f-arith-ops op))
            (type-info env the-ty)]
           [(or (set-member? i-cmp-ops op) (set-member? f-cmp-ops op))
            (type-info env (IntT #t 32))])]
    [(LetE x ty xe be)
     (match-define (type-info xe-env xe-ty)
       (expr-type-info xe))
     (unless (equal? ty xe-ty)
       (report "LetE: declaration '~a' has type ~v but is initialized as type ~v"
               x ty xe-ty))
     (match-define (type-info be-env be-ty)
       (expr-type-info be))
     ;; If x is not referenced in 'be', x will not be in be-env.
     ;; In this case, return ty to avoid error.
     (define be-x-ty (hash-ref be-env x ty))
     (unless (equal? ty be-x-ty)
       (report
        "LetE: declaration '~a' has type ~v but is referenced as type ~v in body"
        x ty be-x-ty))
     (type-info be-env be-ty)]
    [(IfE ce te fe)
     (match-define (type-info ce-env ce-ty)
       (expr-type-info ce))
     (unless (IntT/any? ce-ty)
       (report "IfE: predicate type ~v not integral" ce-ty))
     (match-define (type-info te-env te-ty)
       (expr-type-info te))
     (match-define (type-info fe-env fe-ty)
       (expr-type-info fe))
     (unless (type=? te-ty fe-ty)
       (report "IfE: 'then' type ~v and 'else' type ~v not equal" te-ty fe-ty))
     (define the-ty (resolve-type te-ty fe-ty))
     (define env (env-union ce-env te-env fe-env))
     (type-info env the-ty)]))

(define (path-type-info p)
  (define-reporter report p)
  (match p
    [(MetaP (? type-info? ti) _) ti]
    [(MetaP _ p) (path-type-info p)]
    [(Var x ty) (type-info (hash x ty) ty)]
    [(Global ty _) (type-info (hash) ty)]
    [(ExtVar _ name ty)
     (define x (string->symbol name))
     (type-info (hash x ty) ty)]
    [(Select p ie)
     (match-define (type-info ie-env ie-ty)
       (expr-type-info ie))
     (unless (IntT/any? ie-ty)
       (report "Select: index type ~v not integral" ie-ty))
     (match-define (type-info p-env (and (ArrT dim ety) p-ty))
       (path-type-info p))
     ;; XXX Only checks for bounds when `ie` is a constant expression.
     ;; Eventually, this code should use results from the verifier to
     ;; implement a stricter check.
     (define ie* (unpack-MetaE ie))
     (when (Int? ie*)
       (define idx (Int-val ie*))
       (unless (and (exact-nonnegative-integer? idx) (< idx dim))
         (report "Select: index ~a out of bounds for ~v" idx p-ty)))
     (define env (env-union ie-env p-env))
     (type-info env ety)]
    [(Field p f)
     (match-define (type-info p-env (RecT f->ty _ _))
       (path-type-info p))
     (type-info p-env (hash-ref f->ty f))]
    [(Mode p m)
     (match-define (type-info p-env (UniT m->ty _))
       (path-type-info p))
     (type-info p-env (hash-ref m->ty m))]))

(define (stmt-env-info s)
  (define-reporter report s)
  (define (rec s) (stmt-env-info s))
  (match s
    [(MetaS (? env-info? ei) _) ei]
    [(MetaS _ s) (stmt-env-info s)]
    [(? (or/c Skip? Fail? Jump?)) (env-info (hasheq))]
    [(Begin f s)
     (match-define (env-info f-env) (rec f))
     (match-define (env-info s-env) (rec s))
     (env-info (env-union f-env s-env))]
    [(Assign p e)
     (match-define (type-info p-env p-ty)
       (path-type-info p))
     (match-define (type-info e-env e-ty)
       (expr-type-info e))
     (unless (type=? p-ty e-ty)
       (report
        "Assign: path type ~v and expression type ~v not equal"
        p-ty e-ty))
     (env-info (env-union p-env e-env))]
    [(If p t f)
     (match-define (type-info p-env p-ty)
       (expr-type-info p))
     (match-define (env-info t-env) (rec t))
     (match-define (env-info f-env) (rec f))
     (unless (IntT/any? p-ty)
       (report "If: predicate type ~v not integral" p-ty))
     (env-info (env-union p-env t-env f-env))]
    [(While p body)
     (match-define (type-info p-env p-ty)
       (expr-type-info p))
     (unless (IntT/any? p-ty)
       (report "While: predicate type ~v not integral" p-ty))
     (match-define (env-info body-env) (rec body))
     (env-info (env-union p-env body-env))]
    [(Let/ec _ body) (rec body)]
    [(Let x ty xi bs)
     (check-init-type s ty xi)
     (match-define (env-info bs-env) (rec bs))
     (define bs-x-ty (hash-ref bs-env x ty))
     (unless (type=? ty bs-x-ty)
       (report
        "Let: declaration '~a' has type ~v but is referenced as type ~v in body"
        x ty bs-x-ty))
     (env-info bs-env)]
    [(Call x ty f as bs)
     (match-define (or (IntFun f-as _ ret-ty _ _) (ExtFun _ f-as ret-ty _))
       (unpack-MetaFun f))
     (define f-as-length (length f-as))
     (define as-length (length as))
     (unless (= f-as-length as-length)
       (report "Call: given ~a arguments, expected ~a" as-length f-as-length))
     (unless (equal? ret-ty ty)
       (report
        "Call: declaration '~a' has type ~v but is initialized as type ~v"
        x ret-ty ty))
     (for ([a (in-list as)] [fa (in-list f-as)] [i (in-naturals 1)])
       (define a-ty (type-info-ty
                     (cond [(Expr? a) (expr-type-info a)]
                           [(Path? a) (path-type-info a)])))
       (unless (type=? a-ty (Arg-ty fa))
         (report "Call: expected type ~v for argument ~a, given ~v" fa i a)))
     (match-define (env-info bs-env) (rec bs))
     (define bs-x-ty (hash-ref bs-env x ty))
     (unless (type=? ty bs-x-ty)
       (report
        "Call: declaration '~a' has type ~v but is referenced as type ~v in body"
        x ty bs-x-ty))
     (env-info bs-env)]))

;; XXX Improve this, maybe check-init-type should instead be
;; init-type and just take an Init and return a type. That way
;; the calling function would be responsible for checking for
;; outer-level type mismatches and this function would only
;; error when internally inconsistent. 
(define (check-init-type outer-s ty i)
  (define-reporter report outer-s)
  (define (rec ty i) (check-init-type outer-s ty i))
  (match i
    [(UndI u-ty)
     (unless (equal? ty u-ty)
       (report "UndI: type mismatch with ~v and ~v" ty u-ty))]
    [(ConI e)
     (match-define (type-info _ e-ty) (expr-type-info e))
     (unless (equal? ty e-ty)
       (report "ConI: type mismatch with ~v and ~v" ty e-ty))]
    [(ZedI z-ty)
     (unless (equal? ty z-ty)
       (report "ZedI: type mismatch with ~v and ~v" ty z-ty))]
    [(ArrI is)
     (define is-len (length is))
     (define ty-dim (ArrT-dim ty))
     (unless (equal? is-len ty-dim)
       (report "ArrI: length mismatch, ~a != ~a" is-len ty-dim))
     (for ([i (in-list is)])
       (rec (ArrT-ety ty) i))]
    [(RecI f->i)
     (match-define (RecT f->ty _ _) ty)
     (for ([(f i) (in-hash f->i)])
       (rec (hash-ref f->ty f) i))]
    [(UniI m i)
     (match-define (UniT m->ty _) ty)
     (rec (hash-ref m->ty m) i)]))

(define (fun-type-info f)
  (define-reporter report f)
  (match f
    [(MetaFun (? type-info? ti) _) ti]
    [(MetaFun _ f) (fun-type-info f)]
    [(IntFun args ret-x ret-ty _ body)
     (match-define (env-info env)
       (stmt-env-info body))
     (unless (VoiT? ret-ty)
       (define (ret-x-not-referenced!)
         (report "IntFun: return type declared as ~v, but no value is returned"
                 ret-ty))
       (define body-ret-x-ty (hash-ref env ret-x ret-x-not-referenced!))
       (unless (equal? ret-ty body-ret-x-ty)
         (report
          "IntFun: return type declared as ~v but returns value of type ~v"
          ret-ty body-ret-x-ty)))
     (for ([a (in-list args)] [i (in-naturals 1)])
       (match-define (Arg x ty _) a)
       (define body-x-ty (hash-ref env x ty))
       (unless (equal? ty body-x-ty)
         (report
          "IntFun: argument ~a declared as type ~v but referenced as type ~v in body"
          i ty body-x-ty)))
     (type-info env ret-ty)]
    ;; XXX Should we somehow be tracking ExtFun declarations and making sure
    ;; that all ExtFuns which share 'name' are really equal?
    [(ExtFun _ args ret-ty _) (type-info (hasheq) ret-ty)]))

(define-simple-macro (define-ensurer name:id meta-type info-proc)
  (define (name v)
    (let rec ([v* v])
      (match v*
        [(meta-type (? env-info? info) _) info]
        [(meta-type _ v*) (rec v*)]
        [_ (meta-type (info-proc v*) v)]))))

(define-ensurer ensure-expr-type MetaE expr-type-info)
(define-ensurer ensure-path-type MetaP path-type-info)
(define-ensurer ensure-stmt-env MetaS stmt-env-info)
(define-ensurer ensure-fun-type MetaFun fun-type-info)

(define-syntax (define-constructor stx)
  (syntax-parse stx
    [(_ name:id ensure)
     #:with name^ (generate-temporary #'name)
     (syntax/loc stx
       (begin
         (define-match-expander name^
           (λ (stx*)
             (syntax-parse stx*
               [(_ args:expr (... ...))
                (syntax/loc stx*
                  (name args (... ...)))]))
           (λ (stx*)
             (syntax-parse stx*
               [me:id
                (quasisyntax/loc stx*
                  (contract
                   (value-contract name)
                   (λ args (ensure (apply name args)))
                   (syntax-source #'name) #'#,stx* 'me #'name))]
               [(me:id . args)
                (syntax/loc stx*
                  (#%app me . args))])))
         (provide (rename-out [name^ name]))))]))

(define-simple-macro (define-exprs name:id ...)
  (begin (define-constructor name ensure-expr-type) ...))
(define-exprs Int Flo Cast Read BinOp LetE IfE)

(define-simple-macro (define-paths name:id ...)
  (begin (define-constructor name ensure-path-type) ...))
(define-paths Var Select Field Mode ExtVar)

(define-simple-macro (define-stmts name:id ...)
  (begin (define-constructor name ensure-stmt-env) ...))
(define-stmts Skip Fail Begin Assign If While Jump Let/ec Let Call)

(define-simple-macro (define-funs name:id ...)
  (begin (define-constructor name ensure-fun-type) ...))
(define-funs IntFun ExtFun)

(define (expr-type e)
  (type-info-ty (expr-type-info e)))
(define (path-type p)
  (type-info-ty (path-type-info p)))
(define (fun-type f)
  (type-info-ty (fun-type-info f)))
(provide
 (contract-out
  [expr-type (-> Expr? Type?)]
  [path-type (-> Path? Type?)]
  [fun-type (-> Fun? Type?)]))
