#lang racket/base
(require racket/contract/base
         racket/contract/region
         racket/match
         syntax/parse/define
         "ast.rkt")

(define/contract (arithmetic-shift-left n m)
  (exact-integer? exact-nonnegative-integer? . -> . exact-integer?)
  (arithmetic-shift n m))

(define/contract (arithmetic-shift-right n m)
  (exact-integer? exact-nonnegative-integer? . -> . exact-integer?)
  (arithmetic-shift n (- m)))

;; For now, assume 64 bits of storage for unsigned values.
;; TODO: Should signed values also be restricted to 64-bits?
;; Right now, they are basically BigInts and can grow very
;; large or small.
(define 2^64 (expt 2 64))

(define (unsigned-quotient a b)
  (modulo (quotient a b) 2^64))

(define (unsigned-remainder a b)
  (modulo (remainder a b) 2^64))

(define ((bin-op op) a b)
  (match-define (Integer a-signed? a-bits a-val) a)
  (match-define (Integer b-signed? b-bits b-val) b)
  (unless (equal? a-signed? b-signed?)
    (error "Mismatched signs" a b))
  (unless (= a-bits b-bits)
    (error "Mismatched bit widths" a b))
  (Integer a-signed? a-bits (op a-val b-val)))

(define (bool->c b)
  (if b 1 0))

(define-simple-macro (make-bin-op-table ([arith-name arith-op] ...)
                                        ([cmp-name cmp-op] ...))
  ;; TODO: better way of forwarding macro args to hasheq?
  (make-immutable-hasheq
   (append
    (list (cons arith-name (bin-op arith-op)) ...)
    (list (cons cmp-name (bin-op (λ (a b) (bool->c (cmp-op a b))))) ...))))

(define bin-op-table
  (make-bin-op-table
   ;; Binary arithmetic
   (['iadd +]
    ['isub -]
    ['imul *]
    ['iudiv unsigned-quotient]
    ['isdiv quotient]
    ['iurem unsigned-remainder]
    ['isrem remainder]
    ['ishl arithmetic-shift-left]
    ; 'ilshr - logical rshift
    ['iashr arithmetic-shift-right]
    ['ior bitwise-ior]
    ['iand bitwise-and]
    ['ixor bitwise-xor])
   ;; Binary comparisons
   (['ieq =]
    ['ine (λ (a b) (not (= a b)))]
    ;; TODO: Should we keep separate signed and unsigned ops in the table if
    ;; signed-ness is part of the integer type? Right now unsigned ops are
    ;; pretty much broken since they just modulo their arguments by 2^64.
    ;; Going forward ops should probably infer sign from arguments, then modulo
    ;; the result depending on signed-ness of arguments to simulate
    ;; overflow/underflow.
    ['iugt (unsigned-cmp >)]
    ['iuge (unsigned-cmp >=)]
    ['iult (unsigned-cmp <)]
    ['iule (unsigned-cmp <=)]
    ['isgt >]
    ['isge >=]
    ['islt <]
    ['isle <=])))

(define ((unsigned-cmp op) a b)
  (op (modulo a 2^64)
      (modulo b 2^64)))

(define (eval-expr σ e)
  (define (rec e) (eval-expr σ e))
  (match e
    [(Var x)
     (hash-ref σ x)]
    [(Integer signed? bits val)
     e]
    [(IBinOp op L R)
     ((hash-ref bin-op-table op)
      (rec L) (rec R))]))

(define (eval-expr-pred σ pred)
  (not (zero? (Integer-val (eval-expr σ pred)))))

(define (eval-stmt γ σ s)
  (match s
    [(Skip) σ]
    [(Fail m) (error 'Fail m)]
    [(Assign (Var x) e)
     (hash-set σ x (eval-expr σ e))]
    [(Begin f s)
     (eval-stmt γ (eval-stmt γ σ f) s)]
    [(If p t f)
     (eval-stmt γ σ (if (eval-expr-pred σ p) t f))]
    [(While p _ b)
     (cond [(eval-expr-pred σ p)
            (eval-stmt γ (eval-stmt γ σ b) s)]
           [else σ])]
    [(Return l)
     ((hash-ref γ l) σ)]
    [(Let/ec l b)
     (let/ec this-return
       (eval-stmt (hash-set γ l this-return) σ b))]
    [(Assert _ p msg)
     (or (and (eval-expr-pred σ p) σ)
         (error 'Assert "Failed assertion: ~e" msg))]))

(define (eval-stmt* s)
  (eval-stmt (hasheq) (hasheq) s))

(provide eval-expr
         eval-stmt)
