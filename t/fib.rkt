#lang racket/base
(require racket/list
         adqc)

;; Assumes env with variable 'n set to (S64 arg)
;; Stores result in 'return variable
(define fib
  (S
   (cond
     [(ISLe (Read (Var 'n S64T)) (S64 1))
      (set! (Var 'return S64T) (Read (Var 'n S64T)))]
     [else
      (define fib : S64T := (ConI (S64 1)))
      (define prev : S64T := (ConI (S64 1)))
      (define i : S64T := (ConI (S64 2)))
      (define tmp : S64T := (Undef))
      (while (ISLt (Read i) (Read (Var 'n S64T)))
        (set! tmp (Read fib))
        (set! fib (IAdd (Read fib) (Read prev)))
        (set! prev (Read tmp))
        (set! i (IAdd (Read i) (S64 1))))
      (set! (Var 'return S64T) (Read fib))])))

(define (run-fib n)
  (define result-env (eval-stmt (hasheq) (hasheq 'n (S64 n)) fib))
  (Int-val (hash-ref result-env 'return)))

(define (rfib n)
  (for/fold ([a 0] [b 1] #:result a)
            ([i (in-range n)])
    (values b (+ a b))))

(module+ test
  (require chk)
  (for ([i (in-range 10)])
    (chk (run-fib i) (rfib i))))
