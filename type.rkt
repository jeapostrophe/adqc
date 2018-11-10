#lang racket/base
(require racket/contract/base
         racket/hash
         racket/match
         racket/set
         "ast.rkt")

(struct env-info (env) #:transparent)
(struct type-info env-info (ty) #:transparent)

(define i-arith-ops '(iadd isub imul iudiv isdiv iurem isrem ishl ilshr iashr iand ior ixor))
(define f-arith-ops '(fadd fsub fmul fdiv frem))
(define i-cmp-ops '(ieq ine iugt iuge iult iule isgt isge islt isle))
(define f-cmp-ops '(foeq fone fogt foge folt fole fueq fune fugt fuge fult fule ffalse ftrue ford funo))

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

(define (ensure-expr-type e)
  (let rec ([e* e])
    (match e*
      [(MetaE (? type-info? ti) _) e]
      [(MetaE _ e*) (rec e*)]
      [(? Expr?) (MetaE (expr-type-info e*) e)])))

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

(define (ensure-path-type p)
  (let rec ([p* p])
    (match p*
      [(MetaP (? type-info? ti) _) ti]
      [(MetaP _ p*) (rec p*)]
      [(? Path?) (MetaP (path-type-info p*) p)])))

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
     ;; XXX init-type -> should return Type?
     (define xi-ty #f)
     (unless (equal? ty xi-ty)
       (error 'stmt-env-info "Let: x and xi types not equal"))
     (match-define (env-info bs-env) (rec bs))
     (unless (equal? ty (hash-ref bs-env x ty))
       (error 'stmt-env-info "Let: bs references x as incorrect type"))
     (env-info bs-env)]
    [(Call x ty f as bs)
     ;; XXX Check that arg types match expected
     ;; XXX Check that f ret-ty matches ty
     (rec bs)]))

(define (ensure-stmt-env s)
  (let rec ([s* s])
    (match s*
      [(MetaS (? env-info? ei) _) ei]
      [(MetaS _ s*) (rec s*)]
      [(? Stmt?) (MetaS (stmt-env-info s*) s)])))

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

(define (ensure-fun-type f)
  (let rec ([f* f])
    (match f*
      [(MetaFun (? type-info? ti) _) ti]
      [(MetaFun _ f*) (rec f*)]
      [(? Fun?) (MetaFun (fun-type-info f*) f)])))

;; Typed expressions constructors
(define (Int^ signed? bits val)
  (MetaE (type-info (hasheq) (IntT signed? bits))
         (Int signed? bits val)))
(define (Flo^ bits val)
  (MetaE (type-info (hasheq) (FloT bits))
         (Flo bits val)))
(define (Cast^ ty e)
  (ensure-expr-type (Cast ty e)))
(define (Read^ p)
  (ensure-expr-type (Read p)))
(define (BinOp^ op L R)
  (ensure-expr-type (BinOp op L R)))
(define (LetE^ x ty xe be)
  (ensure-expr-type (LetE x ty xe be)))
(define (IfE^ ce te fe)
  (ensure-expr-type (IfE ce te fe)))

;; Typed path constructors
(define (Var^ x ty)
  (ensure-path-type (Var x ty)))
(define (Select^ p ie)
  (ensure-path-type (Select p ie)))
(define (Field^ p f)
  (ensure-path-type (Field p f)))
(define (Mode^ p m)
  (ensure-path-type (Mode p m)))
(define (ExtVar^ src name ty)
  (ensure-path-type (ExtVar src name ty)))

;; Typed statement constructors
(define (Skip^ comment)
  (ensure-stmt-env (Skip comment)))
(define (Fail^ msg)
  (ensure-stmt-env (Fail msg)))
(define (Begin^ f s)
  (ensure-stmt-env (Begin f s)))
(define (Assign^ p e)
  (ensure-stmt-env (Assign p e)))
(define (If^ p t f)
  (ensure-stmt-env (If p t f)))
(define (While^ p body)
  (ensure-stmt-env (While p body)))
(define (Jump^ label)
  (ensure-stmt-env (Jump label)))
(define (Let/ec^ label body)
  (ensure-stmt-env (Let/ec label body)))
(define (Let^ x ty xi bs)
  (ensure-stmt-env (Let x ty xi bs)))
(define (Call^ x ty f as bs)
  (ensure-stmt-env (Call x ty f as bs)))

;; Typed function constructors
(define (IntFun^ args ret-x ret-ty ret-lab body)
  (ensure-fun-type (IntFun args ret-x ret-ty ret-lab body)))
(define (ExtFun^ src args ret-ty name)
  (ensure-fun-type (ExtFun src args ret-ty name)))

;; XXX Can we both rename and provide a contract when providing?
;; ... or maybe we don't need to, since all of these functions
;; immediately call the underlying constructors, which will check
;; the argument types.
(provide
 (rename-out
  [Int^ Int]
  [Flo^ Flo]
  [Cast^ Cast]
  [Read^ Read]
  [BinOp^ BinOp]
  [LetE^ LetE]
  [IfE^ IfE]
  [Var^ Var]
  [Select^ Select]
  [Field^ Field]
  [Mode^ Mode]
  [ExtVar^ ExtVar]
  [Skip^ Skip]
  [Fail^ Fail]
  [Begin^ Begin]
  [Assign^ Assign]
  [If^ If]
  [While^ While]
  [Jump^ Jump]
  [Let/ec^ Let/ec]
  [Let^ Let]
  [Call^ Call]
  [IntFun^ IntFun]
  [ExtFun^ ExtFun]))