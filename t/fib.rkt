#lang racket/base
(require racket/list
         adqc)

(define fib-p
  (Program
   (hasheq) (hasheq)
   (hasheq
    "fib"
    (IntFun
     (list (Arg 'n S64T 'read-only)) (S64 1)
     'return S64T (S64 1)
     '_return
     (S
      (cond
        [(ISLe (Read (Var 'n S64T)) (S64 1))
         (set! (Var 'return S64T) (Read (Var 'n S64T)))]
        [else
         (define fib : S64T := (ConI (S64 1)))
         (define prev : S64T := (ConI (S64 1)))
         (define i : S64T := (ConI (S64 2)))
         (define tmp : S64T := (UndI S64T))
         (while (ISLt (Read i) (Read (Var 'n S64T)))
           (set! tmp (Read fib))
           (set! fib (IAdd (Read fib) (Read prev)))
           (set! prev (Read tmp))
           (set! i (IAdd (Read i) (S64 1))))
         (set! (Var 'return S64T) (Read fib))]))))))

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
