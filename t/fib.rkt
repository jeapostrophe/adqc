#lang racket/base
(require racket/list
         adqc)

;; Assumes env with variable 'n set to (S64 arg)
;; Stores result in 'return variable
(define fib
  (S
   (cond
     [(ISLe (Var 'n S64T) (S64 1))
      (set! (Var 'return S64T) (Var 'n S64T))]
     [else
      (set! (Var 'fib S64T) (S64 1))
      (set! (Var 'prev S64T) (S64 1))
      (set! (Var 'i S64T) (S64 2))
      (while (ISLt (Var 'i S64T) (Var 'n S64T))
        (set! (Var 'tmp S64T) (Var 'fib S64T))
        (set! (Var 'fib S64T)
              (IAdd (Var 'fib S64T) (Var 'prev S64T)))
        (set! (Var 'prev S64T) (Var 'tmp S64T))
        (set! (Var 'i S64T) (IAdd (Var 'i S64T) (S64 1))))
      (set! (Var 'return S64T) (Var 'fib S64T))])))

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
