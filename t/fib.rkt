#lang racket/base
(require adqc)

(define fib-p
  (Prog
   (define-fun (fib [n : S64]) : S64
     (define a : S64 := (S64 0))
     (define b : S64 := (S64 1))
     (define i : S64 := (S64 0))
     (define tmp : S64)
     (while (islt i n)
       {tmp <- a}
       {a <- b}
       {b <- (iadd tmp b)}
       {i <- (iadd i (S64 1))})
     a)))

(define (rfib n)
  (for/fold ([a 0] [b 1] #:result a)
            ([i (in-range n)])
    (values b (+ a b))))

(module+ test
  (require "main.rkt")
  (TProgN fib-p
          ["fib" (S64 0) => (S64 (rfib 0))]
          ["fib" (S64 1) => (S64 (rfib 1))]
          ["fib" (S64 2) => (S64 (rfib 2))]
          ["fib" (S64 3) => (S64 (rfib 3))]
          ["fib" (S64 4) => (S64 (rfib 4))]
          ["fib" (S64 5) => (S64 (rfib 5))]
          ["fib" (S64 6) => (S64 (rfib 6))]
          ["fib" (S64 7) => (S64 (rfib 7))]
          ["fib" (S64 8) => (S64 (rfib 8))]))
