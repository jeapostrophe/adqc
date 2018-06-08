#lang racket/base
(require racket/list
         adqc)

(define fib-p
  (Prog
   (define-fun (fib [n : S64T]) : S64T
     (define a : S64T := (S64 0))
     (define b : S64T := (S64 1))
     (define i : S64T := (S64 0))
     (define tmp : S64T)
     (while (ISLt (Read i) (Read n))
       {tmp <- (Read a)}
       {a <- (Read b)}
       {b <- (IAdd (Read tmp) (Read b))}
       {i <- (IAdd (Read i) (S64 1))})
     (Read a))))

(define (run-fib n)
  (Int-val (eval-program fib-p "fib" (list (S64 n)))))

(define (rfib n)
  (for/fold ([a 0] [b 1] #:result a)
            ([i (in-range n)])
    (values b (+ a b))))

(module+ test
  (require chk)
  (for ([i (in-range 10)])
    (chk (run-fib i) (rfib i))))
