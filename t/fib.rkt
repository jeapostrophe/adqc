#lang racket/base
(require racket/list
         adqc)

;; Assumes env with variable 'n set to (S64 arg)
;; Stores result in 'return variable
(define fib
  (If (ISLe (Var 'n) (S64 1))
      (Assign (Var 'return) (Var 'n))
      (Begin* (Assign (Var 'fib) (S64 1))
              (Assign (Var 'prev) (S64 1))
              (Assign (Var 'i) (S64 2))
              (While (ISLt (Var 'i) (Var 'n))
                     (ISLe (Var 'i) (Var 'n))
                     (Begin* (Assign (Var 'tmp) (Var 'fib))
                             (Assign (Var 'fib) (IAdd (Var 'fib) (Var 'prev)))
                             (Assign (Var 'prev) (Var 'tmp))
                             (Assign (Var 'i) (IAdd (Var 'i) (S64 1)))))
              (Assign (Var 'return) (Var 'fib)))))

(define (run-fib n)
  (define result-env (eval-stmt (hasheq) (hasheq 'n (S64 n)) fib))
  (Integer-val (hash-ref result-env 'return)))

(define (rfib n)
  (for/fold ([a 0] [b 1] #:result a)
            ([i (in-range n)])
    (values b (+ a b))))

(module+ test
  (require chk)
  (for ([i (in-range 10)])
    (chk (run-fib i) (rfib i))))
