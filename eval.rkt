#lang racket/base
(require racket/contract/base
         racket/contract/region
         racket/flonum
         racket/undefined
         racket/match
         threading
         "ast.rkt")

;; TODO: These contracts are no longer effective since the evaluator
;; will cast arguments anyway. Instead of using these Racket contracts,
;; maybe int ops should be able to reject arguments greater than their
;; bit width?
(define/contract (arithmetic-shift-left n m)
  (exact-integer? exact-nonnegative-integer? . -> . exact-integer?)
  (arithmetic-shift n m))

(define/contract (arithmetic-shift-right n m)
  (exact-integer? exact-nonnegative-integer? . -> . exact-integer?)
  (arithmetic-shift n (- m)))

(define/contract (logical-shift-right n m)
  (exact-integer? exact-nonnegative-integer? . -> . exact-integer?)
  (quotient n (expt 2 m)))

(define (!= a b)
  (not (= a b)))

(define (fl-remainder a b)
  (fl- a (fl* (flfloor (fl/ a b)) b)))

(define (fl-!= a b)
  (not (fl= a b)))

;;
;; Operation wrappers
;;;;;;;;;;;;;;;;;;;;;;;

(define (signed->unsigned bits val)
  (modulo val (expt 2 bits)))

(define (unsigned->signed bits val)
  (define val* (modulo val (expt 2 bits)))
  (if (< val* (expt 2 (sub1 bits)))
      val*
      (- val* (expt 2 bits))))

(define (get-cast-fn signed?)
  (if signed?
      unsigned->signed
      signed->unsigned))

(define (((int-op signed?) op) a b)
  (match-define (Int a-signed? a-bits a-val) a)
  (match-define (Int b-signed? b-bits b-val) b)
  (unless (eq? a-signed? b-signed?)
    (error "Mismatched signs" a b))
  (unless (= a-bits b-bits)
    (error "Mismatched bit widths" a b))
  (define pre-cast (get-cast-fn signed?))
  (define post-cast (get-cast-fn a-signed?))
  (define a-val* (pre-cast a-bits a-val))
  (define b-val* (pre-cast b-bits b-val))
  (Int a-signed? a-bits (post-cast a-bits (op a-val* b-val*))))

(define sint-op (int-op #t))
(define uint-op (int-op #f))

(define ((flo-op op) a b)
  (match-define (Flo a-bits a-val) a)
  (match-define (Flo b-bits b-val) b)
  (unless (= a-bits b-bits)
    (error 'flo-op "Mismatched bit widths" a b))
  (Flo a-bits (op a-val b-val)))

(define ((ordered-op op) a b)
  (and (not (equal? a +nan.0))
       (not (equal? b +nan.0))
       (op a b)))

(define ((unordered-op op) a b)
  (or (equal? a +nan.0)
      (equal? b +nan.0)
      (op a b)))

(define ((bool-op op) a b)
  (if (op a b) 1 0))

(define sint-cmp
  (λ~> bool-op sint-op))

(define uint-cmp
  (λ~> bool-op uint-op))

(define ord-flo-cmp
  (λ~> ordered-op bool-op flo-op))

(define unord-flo-cmp
  (λ~> unordered-op bool-op flo-op))

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
          'fadd (flo-op fl+)
          'fsub (flo-op fl-)
          'fmul (flo-op fl*)
          'fdiv (flo-op fl/)
          'frem (flo-op fl-remainder)
          ; 'ffalse / 'ftrue -- probably don't care about these?
          'foeq (ord-flo-cmp fl=)
          'fogt (ord-flo-cmp fl>)
          'foge (ord-flo-cmp fl>=)
          'folt (ord-flo-cmp fl<)
          'fole (ord-flo-cmp fl<=)
          'fone (ord-flo-cmp fl-!=)
          ; 'ford - #t if both args not NAN - care?
          'fueq (unord-flo-cmp fl=)
          'fugt (unord-flo-cmp fl>)
          'fuge (unord-flo-cmp fl>=)
          'fult (unord-flo-cmp fl<)
          'fule (unord-flo-cmp fl<=)
          'fune (unord-flo-cmp fl-!=)
          ; 'funo - #t if either arg is NAN - care?
          ))

(define (eval-expr σ e)
  (define (rec e) (eval-expr σ e))
  (match e
    [(or (? Int?) (? Flo?))
     e]
    ;; XXX Cast
    ;; XXX use P
    [(Read (Var x _))
     (hash-ref σ x)]
    [(BinOp op L R)
     ((hash-ref bin-op-table op)
      (rec L) (rec R))]
    [(LetE x xe be)
     (eval-expr (hash-set σ x (eval-expr σ xe)) be)]
    [(IfE ce te fe)
     (eval-expr σ (if (eval-expr-pred σ ce) te fe))]
    [(MetaE _ e)
     (eval-expr σ e)]))

(define (eval-expr-pred σ pred)
  (not (zero? (Int-val (eval-expr σ pred)))))

(define (eval-init σ ty i)
  (match i
    [(Undef) undefined]
    [(ConI e) (eval-expr σ e)]))

(define (eval-stmt γ σ s)
  (match s
    [(Skip _) σ]
    [(Fail m) (error 'Fail m)]
    [(Begin f s)
     (eval-stmt γ (eval-stmt γ σ f) s)]
    ;; XXX use P
    [(Assign (Var x _) e)
     (hash-set σ x (eval-expr σ e))]
    [(If p t f)
     (eval-stmt γ σ (if (eval-expr-pred σ p) t f))]
    [(While p _ b)
     (cond [(eval-expr-pred σ p)
            (eval-stmt γ (eval-stmt γ σ b) s)]
           [else σ])]
    [(Jump l)
     ((hash-ref γ l) σ)]
    [(Let/ec l b)
     (let/ec this-return
       (eval-stmt (hash-set γ l this-return) σ b))]
    [(Let x ty xi bs)
     (eval-stmt γ (hash-set σ x (eval-init σ ty xi)) bs)]
    [(MetaS _ bs)
     (eval-stmt γ σ bs)]))

;; XXX better interface
(define (eval-stmt* s)
  (eval-stmt (hasheq) (hasheq) s))

(provide eval-expr
         eval-stmt
         eval-stmt*)
