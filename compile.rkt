#lang racket/base
(require racket/format
         racket/match
         "grammar.rkt")

(define (bit-width->cast signed? bw)
  (format "(~aint~a_t)"
          (if signed? "" "u")
          bw))

(define bin-op-table
  (hasheq 'iadd "+"
          'isub "-"
          'imul "*"
          'ieq "=="
          ))

(define (compile-expr exp)
  (match exp
    [(Integer signed? bits val)
     (format "(~a~a)" (bit-width->cast signed? bits) val)]
    [(Variable name)
     (~a name)]
    [(IBinOp op L R)
     (define op-str (hash-ref bin-op-table op))
     (define L-str (compile-expr L))
     (define R-str (compile-expr R))
     (format "(~a ~a ~a)" L-str op-str R-str)]))

(define (compile-stmt stmt)
  (match stmt
    [(Skip)
     "// Skip"]
    [(Assign dest exp)
     (format "~a = (~a);" dest (compile-expr exp))]
    [(Begin L-stmt R-stmt)
     (format "~a~n~a" (compile-stmt L-stmt) (compile-stmt R-stmt))]
    [(If pred then else)
     (list (format "if (~a) {" (compile-expr pred))
           ind++
           ind-nl
           (compile-stmt then)
           ind--
           ind-nl
           "} else {"
           ind++
           ind-nl
           (compile-stmt else)
           ind--
           ind-nl
           "}")]
    [(While pred invar do-stmt)
     (error "TODO: While stmt")]))

(define ind-nl (gensym))
(define ind++ (gensym))
(define ind-- (gensym))
(define ind-lvl (box 0))

(define ((== a) b)
  (equal? a b))

(define (idisplay v)
  (match v
    [(? (== ind-nl))
     (newline)
     (for ([i (in-range (unbox ind-lvl))])
       (display #\space))]
    [(? (== ind++))
     (set-box! ind-lvl (+ (unbox ind-lvl) 2))]
    [(? (== ind--))
     (set-box! ind-lvl (- (unbox ind-lvl) 2))]
    [_ (display v)]))

(module+ test
  (for-each idisplay
            (compile-stmt (If (IEq (i32 5) (i32 6))
                              (Assign 'x (i32 1))
                              (Assign 'y (i32 2))))))