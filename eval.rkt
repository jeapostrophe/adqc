#lang racket/base
(require racket/contract/base
         racket/contract/region
         racket/flonum
         racket/function
         racket/match
         racket/require
         racket/undefined
         threading
         (subtract-in "ast.rkt" "type.rkt")
         "type.rkt")

(define current-globals (make-parameter #f))

(define (arithmetic-shift-left n m) (arithmetic-shift n m))
(define (arithmetic-shift-right n m) (arithmetic-shift n (- m)))
(define (logical-shift-right n m) (quotient n (expt 2 m)))
(define (!= a b) (not (= a b)))
(define (fl-remainder a b)
  (- a (* (floor (/ a b)) b)))

;; Operation wrappers

(define (signed->unsigned bits val)
  (modulo val (expt 2 bits)))

(define (unsigned->signed bits val)
  (define val* (modulo val (expt 2 bits)))
  (if (< val* (expt 2 (sub1 bits)))
    val*
    (- val* (expt 2 bits))))

(define ((int-cast signed? bits) val)
  (if signed?
    (unsigned->signed bits val)
    (signed->unsigned bits val)))

(define (((int-op signed?) op) a b)
  (match-define (Int a-signed? a-bits a-val) (unpack-MetaE a))
  (match-define (Int b-signed? b-bits b-val) (unpack-MetaE b))
  (unless (eq? a-signed? b-signed?)
    (error "Mismatched signs" a b))
  (unless (= a-bits b-bits)
    (error "Mismatched bit widths" a b))
  (define pre-cast (int-cast signed? a-bits))
  (define post-cast (int-cast a-signed? a-bits))
  (Int a-signed? a-bits (post-cast (op (pre-cast a-val)
                                       (pre-cast b-val)))))

(define sint-op (int-op #t))
(define uint-op (int-op #f))

(define (((int-cmp signed?) op) a b)
  (match-define (Int a-signed? a-bits a-val) (unpack-MetaE a))
  (match-define (Int b-signed? b-bits b-val) (unpack-MetaE b))
  (unless (eq? a-signed? b-signed?)
    (error "Mismatched signs" a b))
  (unless (= a-bits b-bits)
    (error "Mismatched bit widths" a b))
  (define pre-cast (int-cast signed? a-bits))
  (define res (op (pre-cast a-val) (pre-cast b-val)))
  (Int #t 32 (if res 1 0)))

(define sint-cmp (int-cmp #t))
(define uint-cmp (int-cmp #f))

(define ((flo-op op) a b)
  (match-define (Flo a-bits a-val) (unpack-MetaE a))
  (match-define (Flo b-bits b-val) (unpack-MetaE b))
  (unless (= a-bits b-bits)
    (error 'flo-op "Mismatched bit widths" a b))
  (Flo a-bits (op a-val b-val)))

(define ((flo-cmp op) a b)
  (match-define (Flo a-bits a-val) (unpack-MetaE a))
  (match-define (Flo b-bits b-val) (unpack-MetaE b))
  (unless (= a-bits b-bits)
    (error 'flo-cmp "Mismatched bit widths" a b))
  (Int #t 32 (if (op a-val b-val) 1 0)))

(define ((ordered-op op) a b)
  (and (not (equal? a +nan.0))
       (not (equal? b +nan.0))
       (op a b)))

(define ((unordered-op op) a b)
  (or (equal? a +nan.0)
      (equal? b +nan.0)
      (op a b)))

(define ord-flo-cmp
  (λ~> ordered-op flo-cmp))

(define unord-flo-cmp
  (λ~> unordered-op flo-cmp))

(define bin-op-table
  (hasheq 'iadd (uint-op +)
          'isub (uint-op -)
          'imul (uint-op *)
          'iudiv (uint-op quotient)
          'isdiv (sint-op quotient)
          'iurem (uint-op remainder)
          'isrem (sint-op remainder)
          'ishl (uint-op arithmetic-shift-left)
          'ilshr (uint-op logical-shift-right)
          'iashr (sint-op arithmetic-shift-right)
          'ior (uint-op bitwise-ior)
          'iand (uint-op bitwise-and)
          'ixor (uint-op bitwise-xor)
          ;; ieq and ine technically aren't signed or unsigned, but we
          ;; use uint-cmp here because we only care if the results are
          ;; equal, which is unaffected by signed vs. unsigned.
          'ieq (uint-cmp =)
          'ine (uint-cmp !=)
          'iugt (uint-cmp >)
          'iuge (uint-cmp >=)
          'iult (uint-cmp <)
          'iule (uint-cmp <=)
          'isgt (sint-cmp >)
          'isge (sint-cmp >=)
          'islt (sint-cmp <)
          'isle (sint-cmp <=)
          'fadd (flo-op +)
          'fsub (flo-op -)
          'fmul (flo-op *)
          'fdiv (flo-op /)
          'frem (flo-op fl-remainder)
          'ffalse (flo-cmp (const #f))
          'ftrue (flo-cmp (const #t))
          'foeq (ord-flo-cmp =)
          'fogt (ord-flo-cmp >)
          'foge (ord-flo-cmp >=)
          'folt (ord-flo-cmp <)
          'fole (ord-flo-cmp <=)
          'fone (ord-flo-cmp !=)
          'ford (ord-flo-cmp (const #t))
          'fueq (unord-flo-cmp =)
          'fugt (unord-flo-cmp >)
          'fuge (unord-flo-cmp >=)
          'fult (unord-flo-cmp <)
          'fule (unord-flo-cmp <=)
          'fune (unord-flo-cmp !=)
          'funo (unord-flo-cmp (const #f))))

(define (type-cast ty v)
  (match-define (or (Int _ _ val) (Flo _ val))
    (unpack-MetaE v))
  (match ty
    [(IntT signed? bits)
     (define cast (int-cast signed? bits))
     (define val* (inexact->exact (floor val)))
     (Int signed? bits (cast val*))]
    [(FloT bits)
     (define cast
       (match bits
         [32 real->single-flonum]
         [64 real->double-flonum]))
     (Flo bits (cast val))]))

(define ((new-global! the-glob))
  (define v (eval-init (hasheq) (Global-xi the-glob)))
  (hash-set! (current-globals) the-glob v)
  v)

(define (path-read σ p)
  (define (rec p) (unbox (path-read σ p)))
  (match (unpack-MetaP p)
    [(Var x _) (hash-ref σ x)]
    [(and the-glob (Global _ xi))
     (define globals (current-globals))
     (hash-ref globals the-glob (new-global! the-glob))]
    [(Select p ie) (vector-ref (rec p) (Int-val (eval-expr σ ie)))]
    [(Field p f) (hash-ref (rec p) f)]
    [(Mode p m) (hash-ref (rec p) m)]
    [(? ExtVar?)
     (error 'path-read/ref "XXX Cannot interp external variables yet: ~e" p)]))

(define (path-write! σ p v)
  (match (unpack-MetaP p)
    [(Var x _)
     (set-box! (hash-ref σ x) v)]
    [(and the-glob (Global _ xi))
     (define globals (current-globals))
     (set-box! (hash-ref globals the-glob (new-global! the-glob)) v)]
    [(Select p ie)
     (set-box! (vector-ref (unbox (path-read σ p)) (Int-val (eval-expr σ ie))) v)]
    [(Field p f)
     (set-box! (hash-ref (unbox (path-read σ p)) f) v)]
    [(Mode p m)
     (set-box! (hash-ref (unbox (path-read σ p)) m) v)]
    [(? ExtVar?)
     (error 'path-read/ref "XXX Cannot interp external variables yet: ~e" p)]))

(define (eval-expr σ e)
  (define (rec e) (eval-expr σ e))
  (match e
    [(? Int?) e]
    [(? Flo?) e]
    [(Read p) (unbox (path-read σ p))]
    [(Cast ty e) (type-cast ty (rec e))]
    [(BinOp op L R)
     ((hash-ref bin-op-table op) (rec L) (rec R))]
    [(LetE x xt xe be)
     (eval-expr (hash-set σ x (box (eval-expr σ xe))) be)]
    [(IfE ce te fe)
     (eval-expr σ (if (eval-expr-pred σ ce) te fe))]
    [(MetaE _ e)
     (eval-expr σ e)]))

(define (eval-expr-pred σ pred)
  (not (zero? (Int-val (eval-expr σ pred)))))

(define (hash-map-ht h f)
  (define hp (make-hasheq))
  (for ([(k v) (in-hash h)])
    (hash-set! hp k (f v)))
  hp)

(define (type-zero ty)
  (match ty
    [(IntT signed? bits) (Int signed? bits 0)]
    [(FloT 32) (Flo 32 (real->single-flonum 0.0))]
    [(FloT 64) (Flo 64 (real->double-flonum 0.0))]
    [(ArrT dim ety) (build-vector dim (λ (_) (box (type-zero ety))))]
    [(RecT f->ty _ _) (hash-map-ht f->ty (λ (ty) (box (type-zero ty))))]
    [(UniT mode->ty _) (hash-map-ht mode->ty (λ (ty) (box (type-zero ty))))]
    [(? ExtT?) (error 'type-zero "XXX Cannot interp external types yet: ~e" ty)]))

(define (eval-init σ i)
  (match i
    [(UndI ty) (box (type-zero ty))]
    [(ConI e) (box (eval-expr σ e))]
    [(ZedI ty) (box (type-zero ty))]
    [(ArrI is) (box (list->vector (map (λ (i) (eval-init σ i)) is)))]
    [(RecI f->i) (box (hash-map-ht f->i (λ (i) (eval-init σ i))))]
    [(UniI m i) (box (make-hasheq (list (cons m (eval-init σ i)))))]))

(define (eval-stmt γ σ s)
  (match s
    [(Skip _) σ]
    [(Fail m) (error 'Fail m)]
    [(Begin f s)
     (eval-stmt γ σ f)
     (eval-stmt γ σ s)]
    [(Assign p e)
     (path-write! σ p (eval-expr σ e))]
    [(If p t f)
     (eval-stmt γ σ (if (eval-expr-pred σ p) t f))]
    [(While p b)
     (when (eval-expr-pred σ p)
       (eval-stmt γ σ b)
       (eval-stmt γ σ s))]
    [(Jump l)
     ((hash-ref γ l))]
    [(Let/ec l b)
     (let/ec this-return
       (eval-stmt (hash-set γ l this-return) σ b))]
    [(Let x ty xi bs)
     (define xv (eval-init σ xi))
     (eval-stmt γ (hash-set σ x xv) bs)]
    [(MetaS _ bs)
     (eval-stmt γ σ bs)]
    [(Call x ty f as bs)
     (define σ*
       (for/fold ([σ* (hasheq)])
                 ([a (in-list as)] [fa (in-list (Fun-args f))])
         (match-define (Arg x ty m) fa)
         (match (unpack-any a)
           [(or (Read p) (? Path? p))
            (match m
              ['copy
               (hash-set σ* x (box (unbox (path-read σ p))))]
              [(or 'ref 'read-only)
               (hash-set σ* x (path-read σ p))])]
           [(? Expr? e)
            (hash-set σ* x (box (eval-expr σ e)))])))
     (define xv (eval-fun σ* f))
     (eval-stmt γ (hash-set σ x (box xv)) bs)]
    [(Fail msg)
     (error 'eval-stmt msg)]))

(define (eval-fun σ f)
  (match f
    [(? ExtFun?) (error 'eval-fun "XXX Cannot interp external functions yet: ~e" f)]
    [(MetaFun _ f) (eval-fun σ f)]
    [(IntFun as ret-x ret-ty ret-lab body)
     (define ret-x-b (eval-init (hasheq) (UndI ret-ty)))
     (let/ec this-return
       (eval-stmt (hasheq ret-lab this-return) (hash-set σ ret-x ret-x-b) body))
     (unbox ret-x-b)]))

(define (eval-program p n is)
  (define n->f (Program-name->fun p))
  (parameterize ([current-globals (make-hasheq)])
    (define f (hash-ref n->f n))
    (define σ
      (for/fold ([σ (hasheq)])
                ([i (in-list is)] [a (in-list (Fun-args f))])
        (hash-set σ (Arg-x a) (eval-init (hasheq) i))))
    (eval-fun σ f)))

(define Value/c
  (or/c Int? Flo? vector? hash?))

(provide
 (contract-out
  [Value/c contract?]
  [eval-init
   (-> hash? Init? (box/c Value/c))]
  [eval-expr
   (-> hash? Expr? Value/c)]
  [eval-program
   (-> Program? string? (listof Init?)
       Value/c)]))
