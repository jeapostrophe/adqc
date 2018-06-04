#lang racket/base
(require racket/format
         racket/list
         racket/match
         "ast.rkt")

(define (bit-width->cast signed? bw)
  (list* "(" (if signed? "" "u") "int" (~a bw) "_t)"))

(define bin-op-table
  (hasheq 'iadd "+"
          'isub "-"
          'imul "*"
          'ieq "=="
          ))

(define (compile-expr exp)
  (match exp
    [(Integer signed? bits val)
     (list* "(" (bit-width->cast signed? bits) (~a val) ")")]
    [(Variable name)
     ;; XXX something to translate
     (~a name)]
    [(IBinOp op L R)
     (define op-str (hash-ref bin-op-table op))
     (define L-str (compile-expr L))
     (define R-str (compile-expr R))
     (list* L-str " " op-str " " R-str)]))

(define (compile-stmt stmt)
  (match stmt
    [(Skip)
     empty]
    [(Assign dest exp)
     (list* dest " = " (compile-expr exp) ";")]
    [(Begin L-stmt R-stmt)
     (list* (compile-stmt L-stmt) ind-nl (compile-stmt R-stmt))]
    [(If pred then else)
     (list* "if " (compile-expr pred) " {" ind++ ind-nl
            (compile-stmt then)
            ind-- ind-nl "} else {" ind++ ind-nl
            (compile-stmt else)
            ind-- ind-nl "}")]
    [(While pred invar do-stmt)
     (error "TODO: While stmt")]))

(define ind-nl (gensym))
(define ind++ (gensym))
(define ind-- (gensym))
(define ind-lvl (box 0))

(define (idisplay v)
  (match v
    [(== ind-nl)
     (newline)
     (for ([i (in-range (unbox ind-lvl))])
       (display #\space))]
    [(== ind++)
     (set-box! ind-lvl (+ (unbox ind-lvl) 2))]
    [(== ind--)
     (set-box! ind-lvl (- (unbox ind-lvl) 2))]
    [_ (display v)]))

(define (tree-for f t)
  (match t
    [(or (? void?) #f '()) (void)]
    [(cons a d) (tree-for f a) (tree-for f d)]
    [x (f x)]))

(module+ test
  (tree-for idisplay
            (compile-stmt (If (IEq (i32 5) (i32 6))
                              (Assign 'x (i32 1))
                              (Assign 'y (i32 2))))))
