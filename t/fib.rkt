#lang racket/base
(require racket/list
         adqc)

;; Assumes env with variable 'n set to (S64 arg)
;; Stores result in 'return variable
(define fib
  (If (ISLe (Variable 'n) (S64 1))
      (Assign 'return (Variable 'n))
      (Begin* (Assign 'fib (S64 1))
              (Assign 'prev (S64 1))
              (Assign 'i (S64 2))
              (While (ISLt (Variable 'i) (Variable 'n))
                     (ISLe (Variable 'i) (Variable 'n))
                     (Begin* (Assign 'tmp (Variable 'fib))
                             (Assign 'fib (IAdd (Variable 'fib)
                                                (Variable 'prev)))
                             (Assign 'prev (Variable 'tmp))
                             (Assign 'i (IAdd (Variable 'i) (S64 1)))))
              (Assign 'return (Variable 'fib)))))

(define (run-fib n)
  (define result-env (eval-stmt (hasheq 'n (S64 n)) fib))
  (Integer-val (hash-ref result-env 'return)))

(module+ test
  (require chk)
  (chk (run-fib 1) 1
       (run-fib 2) 1
       (run-fib 3) 2
       (run-fib 4) 3
       (run-fib 5) 5
       (run-fib 6) 8
       (run-fib 7) 13
       (run-fib 8) 21
       (run-fib 9) 34))
