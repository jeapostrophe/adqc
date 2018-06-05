#lang racket/base
(require racket/contract/base
         racket/contract/region
         racket/match
         threading
         "ast.rkt")

;; XXX float operations
;; XXX correct int operations

(define/contract (arithmetic-shift-left n m)
  (exact-integer? exact-nonnegative-integer? . -> . exact-integer?)
  (arithmetic-shift n m))

(define/contract (arithmetic-shift-right n m)
  (exact-integer? exact-nonnegative-integer? . -> . exact-integer?)
  (arithmetic-shift n (- m)))

(define/contract (logical-shift-right n m)
  (exact-integer? exact-nonnegative-integer? . -> . exact-integer?)
  (quotient n (expt 2 m)))

;; For now, assume 64 bits of storage for unsigned values.
;; TODO: Should signed values also be restricted to 64-bits?
;; Right now, they are basically BigInts and can grow very
;; large or small.
(define 2^64 (expt 2 64))

(define (unsigned-quotient a b)
  (modulo (quotient a b) 2^64))

(define (unsigned-remainder a b)
  (modulo (remainder a b) 2^64))

(define (!= a b)
  (not (= a b)))

;;
;; Operation wrappers
;;;;;;;;;;;;;;;;;;;;;;;

(define (signed->unsigned bits val)
  (modulo val (expt 2 bits)))

(define (unsigned->signed bits val)
  (define val* (modulo val (expt 2 bits)))
  (if (< val (expt 2 (sub1 bits)))
      val*
      (- val* (expt 2 bits))))

(define ((int-op op) a b)
  (match-define (Int a-signed? a-bits a-val) a)
  (match-define (Int b-signed? b-bits b-val) b)
  (unless (eq? a-signed? b-signed?)
    (error "Mismatched signs" a b))
  (unless (= a-bits b-bits)
    (error "Mismatched bit widths" a b))
  (Int a-signed? a-bits (op a-val b-val)))

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

(define int-cmp
  (λ~> bool-op int-op))

(define ord-flo-cmp
  (λ~> ordered-op bool-op flo-op))

(define unord-flo-cmp
  (λ~> unordered-op bool-op flo-op))

;; TODO: This is wrong, need to implement better tracking of
;; signed vs. unsigned values.
(define ((unsigned-cmp op) a b)
  (op (modulo a 2^64)
      (modulo b 2^64)))

(define bin-op-table
  (hasheq 'iadd (int-op +)
          'isub (int-op -)
          'imul (int-op *)
          'iudiv (int-op unsigned-quotient)
          'isdiv (int-op quotient)
          'iurem (int-op unsigned-remainder)
          'isrem (int-op remainder)
          'ishl (int-op arithmetic-shift-left)
          'ilshr (int-op logical-shift-right)
          'iashr (int-op arithmetic-shift-right)
          'ior (int-op bitwise-ior)
          'iand (int-op bitwise-and)
          'ixor (int-op bitwise-xor)
          'ieq (int-cmp =)
          'ine (int-cmp !=)
          'iugt (int-cmp (unsigned-cmp >))
          'iuge (int-cmp (unsigned-cmp >=))
          'iult (int-cmp (unsigned-cmp <))
          'iule (int-cmp (unsigned-cmp <=))
          'isgt (int-cmp >)
          'isge (int-cmp >=)
          'islt (int-cmp <)
          'isle (int-cmp <=)
          'fadd (flo-op +)
          'fsub (flo-op -)
          'fmul (flo-op *)
          'fdiv (flo-op /)
          'frem (flo-op remainder)
          ; 'ffalse / 'ftrue -- probably don't care about these?
          'foeq (ord-flo-cmp =)
          'fogt (ord-flo-cmp >)
          'foge (ord-flo-cmp >=)
          'folt (ord-flo-cmp <)
          'fole (ord-flo-cmp <=)
          'fone (ord-flo-cmp !=)
          ; 'ford - #t if both args not NAN - care?
          'fueq (unord-flo-cmp =)
          'fugt (unord-flo-cmp >)
          'fuge (unord-flo-cmp >=)
          'fult (unord-flo-cmp <)
          'fule (unord-flo-cmp <=)
          'fune (unord-flo-cmp !=)
          ; 'funo - #t if either arg is NAN - care?
          ))

(define (eval-expr σ e)
  (define (rec e) (eval-expr σ e))
  (match e
    [(Read (Var x _))
     (hash-ref σ x)]
    [(or (? Int?) (? Flo?))
     e]
    [(BinOp op L R)
     ((hash-ref bin-op-table op)
      (rec L) (rec R))]))

(define (eval-expr-pred σ pred)
  (not (zero? (Int-val (eval-expr σ pred)))))

(define (eval-stmt γ σ s)
  (match s
    [(Skip _) σ]
    [(Fail m) (error 'Fail m)]
    [(Assign (Var x _) e)
     (hash-set σ x (eval-expr σ e))]
    [(Begin f s)
     (eval-stmt γ (eval-stmt γ σ f) s)]
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
       (eval-stmt (hash-set γ l this-return) σ b))]))

;; XXX better interface
(define (eval-stmt* s)
  (eval-stmt (hasheq) (hasheq) s))

(provide eval-expr
         eval-stmt
         eval-stmt*)
