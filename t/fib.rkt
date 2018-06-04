#lang racket/base
(require racket/list
         adqc)

;; Assumes env with variable 'n set to (S64 arg)
;; Stores result in 'return variable
(define fib
  (If (ISLe (Var 'n S64T) (S64 1))
      (Assign (Var 'return S64T) (Var 'n S64T))
      (Begin* (Assign (Var 'fib S64T) (S64 1))
              (Assign (Var 'prev S64T) (S64 1))
              (Assign (Var 'i S64T) (S64 2))
              (While (ISLt (Var 'i S64T) (Var 'n S64T))
                     (ISLe (Var 'i S64T) (Var 'n S64T))
                     (Begin* (Assign (Var 'tmp S64T) (Var 'fib S64T))
                             (Assign (Var 'fib S64T)
                                     (IAdd (Var 'fib S64T) (Var 'prev S64T)))
                             (Assign (Var 'prev S64T) (Var 'tmp S64T))
                             (Assign (Var 'i S64T) (IAdd (Var 'i S64T) (S64 1)))))
              (Assign (Var 'return S64T) (Var 'fib S64T)))))

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
