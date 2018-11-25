#lang racket/base
(require (for-syntax racket/base)
         racket/contract/base
         racket/hash
         racket/match
         racket/set
         syntax/parse/define
         "ast.rkt")

(struct env-info (env) #:transparent)
(struct type-info env-info (ty) #:transparent)

(define i-arith-ops '(iadd isub imul iudiv isdiv iurem isrem ishl ilshr iashr iand ior ixor))
(define f-arith-ops '(fadd fsub fmul fdiv frem))
(define i-cmp-ops '(ieq ine iugt iuge iult iule isgt isge islt isle))
(define f-cmp-ops '(foeq fone fogt foge folt fole fueq fune fugt fuge fult fule ffalse ftrue ford funo))

(define (union-subset? u ty)
  (match-define (UniT m->ty _) u)
  (for/or ([u-ty (in-hash-values m->ty)])
    (cond [(equal? ty u-ty) #t]
          [(UniT? u-ty) (union-subset? u-ty ty)]
          [else #f])))

(define (resolve-type L R)
  (cond [(equal? L R) L]
        [else (cond [(and (UniT? L) (union-subset? L R)) L]
                    [(and (UniT? R) (union-subset? R L)) R]
                    [else (error 'resolve-type "type mismatch L: ~v R: ~v" L R)])]))

(define (env-union e0 . es)
  (apply hash-union e0 es #:combine resolve-type))

(define (expr-type-info e)
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
     (unless (or (IntT? from-ty) (FloT? from-ty))
       (error 'expr-type-info "Cast: from type not numeric"))
     (unless (or (IntT? to-ty) (FloT? to-ty))
       (error 'expr-type-info "Cast: to type not numeric"))
     (type-info from-env to-ty)]
    [(Read p) (path-type-info p)]
    [(BinOp op L R)
     (match-define (type-info L-env L-ty)
       (expr-type-info L))
     (match-define (type-info R-env R-ty)
       (expr-type-info R))
     (unless (equal? L-ty R-ty)
       (error 'expr-type-info "BinOp: LHS and RHS types not equal"))
     (when (or (set-member? i-arith-ops op) (set-member? i-cmp-ops op))
       (unless (IntT? L-ty)
         (error 'expr-type-info "BinOp: integer op expects integral arguments")))
     (when (or (set-member? f-arith-ops op) (set-member? f-cmp-ops op))
       (unless (FloT? L-ty)
         (error 'expr-type-info "BinOp: floating-point op expects floating-point arguments")))
     ;; XXX add logic for union types.
     (define env (hash-union L-env R-env))
     (cond [(or (set-member? i-arith-ops op) (set-member? f-arith-ops op))
            (type-info env L-ty)]
           [(or (set-member? i-cmp-ops op) (set-member? f-cmp-ops op))
            (type-info env (IntT #t 32))])]
    [(LetE x ty xe be)
     (match-define (type-info xe-env xe-ty)
       (expr-type-info xe))
     (unless (equal? ty xe-ty)
       (error 'expr-type-info "LetE: x and xe types not equal"))
     (match-define (type-info be-env be-ty)
       (expr-type-info be))
       ;; If x is not referenced in 'be', x will not be in be-env.
       ;; In this case, return ty to avoid error.
     (unless (equal? ty (hash-ref be-env x ty))
       (error "LetE: be references x as incorrect type"))
     (type-info be-env be-ty)]
    [(IfE ce te fe)
     (match-define (type-info ce-env ce-ty)
       (expr-type-info ce))
     (unless (IntT? ce-ty)
       (error 'expr-type-info "IfE: predicate type not integral"))
     (match-define (type-info te-env te-ty)
       (expr-type-info te))
     (match-define (type-info fe-env fe-ty)
       (expr-type-info fe))
     (unless (equal? te-ty fe-ty)
       (error 'expr-type-info "IfE: true and false types not equal"))
     (define env (hash-union ce-env te-env fe-env))
     (type-info env te-ty)]))

(define (path-type-info p)
  (match p
    [(MetaP (? type-info? ti) _) ti]
    [(MetaP _ p) (path-type-info p)]
    [(Var x ty) (type-info (hash x ty) ty)]
    [(ExtVar _ name ty)
     (define x (string->symbol name))
     (type-info (hash x ty) ty)]
    [(Select p ie)
     (match-define (type-info ie-env ie-ty)
       (expr-type-info ie))
     (unless (IntT? ie-ty)
       (error 'path-type-info "Select: index type not integral"))
     (match-define (type-info p-env (ArrT _ ety))
       (path-type-info p))
     (define env (hash-union ie-env p-env))
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
  (define (rec s) (stmt-env-info s))
  (match s
    [(MetaS (? env-info? ei) _) ei]
    [(MetaS _ s) (stmt-env-info s)]
    [(? (or/c Skip? Fail? Jump?)) (env-info (hasheq))]
    [(Begin f s)
     (match-define (env-info f-env) (rec f))
     (match-define (env-info s-env) (rec s))
     (env-info (hash-union f-env s-env))]
    [(Assign p e)
     (match-define (type-info p-env p-ty)
       (path-type-info p))
     (match-define (type-info e-env e-ty)
       (expr-type-info e))
     (unless (equal? p-ty e-ty)
       (error 'stmt-env-info "Assign: path type and expression type not equal"))
     (env-info (hash-union p-env e-env))]
    [(If p t f)
     (match-define (type-info p-env p-ty)
       (expr-type-info p))
     (match-define (env-info t-env) (rec t))
     (match-define (env-info f-env) (rec f))
     (unless (IntT? p-ty)
       (error 'stmt-env-info "If: predicate type not integral"))
     (env-info (hash-union p-env t-env f-env))]
    [(While p body)
     (match-define (type-info p-env p-ty)
       (expr-type-info p))
     (unless (IntT? p-ty)
       (error 'stmt-env-info "While: predicate type not integral"))
     (match-define (env-info body-env) (rec body))
     (env-info (hash-union p-env body-env))]
    [(Let/ec _ body) (rec body)]
    [(Let x ty xi bs)
     (check-init-type ty xi)
     (match-define (env-info bs-env) (rec bs))
     (unless (equal? ty (hash-ref bs-env x ty))
       (error 'stmt-env-info "Let: bs references x as incorrect type"))
     (env-info bs-env)]
    [(Call x ty f as bs)
     (match-define (or (IntFun f-as _ ret-ty _ _) (ExtFun _ f-as ret-ty _))
       (unpack-MetaFun f))
     (define f-as-length (length f-as))
     (define as-length (length as))
     (cond [(> f-as-length as-length)
            (error 'stmt-env-info "Call: not enough arguments")]
           [(< f-as-length as-length)
            (error 'stmt-env-info "Call: too many arguments")]
           [else (void)])
     (unless (equal? ret-ty ty)
       (error 'stmt-env-info "Call: return type mismatch"))
     (for ([a (in-list as)] [fa (in-list f-as)])
       (unless (equal? (Arg-ty a) (Arg-ty fa))
         (error 'stmt-env-info "Call: argument type mismatch")))
     (match-define (env-info bs-env) (rec bs))
     (unless (equal? ty (hash-ref bs-env x ty))
       (error 'stmt-env-info "Call: bs references x as incorrect type"))
     (env-info bs-env)]))

(define (check-init-type ty i)
  (match i
    [(UndI u-ty)
     (unless (equal? ty u-ty)
       (error 'stmt-env-info "UndI: type mismatch"))]
    [(ConI e)
     (match-define (type-info _ e-ty) (expr-type-info e))
     (unless (equal? ty e-ty)
       (error 'stmt-env-info "ConI: type mismatch"))]
    [(ZedI z-ty)
     (unless (equal? ty z-ty)
       (error 'stmt-env-info "ZedI: type mismatch"))]
    [(ArrI is)
     (unless (equal? (length is) (ArrT-dim ty))
       (error 'stmt-env-info "ArrI: length mismatch"))
     (for ([i (in-list is)])
       (check-init-type (ArrT-ety ty) i))]
    [(RecI f->i)
     (match-define (RecT f->ty _ _) ty)
     (for ([(f i) (in-hash f->i)])
       (check-init-type (hash-ref f->ty f) i))]
    [(UniI m i)
     (match-define (UniT m->ty _) ty)
     (check-init-type (hash-ref m->ty m) i)]))

(define (fun-type-info f)
  (match f
    [(MetaFun (? type-info ti) _) ti]
    [(MetaFun _ f) (fun-type-info f)]
    [(IntFun args ret-x ret-ty _ body)
     (match-define (env-info env)
       (stmt-env-info body))
     ;; XXX Do we want to fail if ret-x is not referenced inside of body?
     (unless (equal? ret-ty (hash-ref env ret-x))
       (error 'fun-type-info "IntFun: ret-ty does not match usage of ret-x in body"))
     (for ([a (in-list args)])
       (match-define (Arg x ty _) a)
       (unless (equal? ty (hash-ref env x ty))
         (error 'fun-type-info "IntFun: arg type does not match usage in body")))
     (type-info ret-ty env)]
    ;; XXX Should we somehow be tracking ExtFun declarations and making sure
    ;; that all ExtFuns which share 'name' are really equal?
    [(ExtFun _ _ ret-ty _) (type-info (hasheq) ret-ty)]))

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
     #:with (name^) (generate-temporaries #'(name))
     (syntax/loc stx
       (begin
         (define (name^ . args) (ensure (apply name args)))
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
