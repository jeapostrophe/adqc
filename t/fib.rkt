#lang racket/base
(require racket/list
         adqc)

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

(define (run-fib n)
  (Int-val (eval-program fib-p "fib" (list (E (S64 n))))))

(define (rfib n)
  (for/fold ([a 0] [b 1] #:result a)
            ([i (in-range n)])
    (values b (+ a b))))

(module+ test
  (require chk)
  (for ([i (in-range 10)])
    (chk (run-fib i) (rfib i))))
