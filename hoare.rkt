#lang racket/base
(require racket/contract/base
         racket/match
         racket/set)

(define (expr? v)
  (define (op? w)
    (set-member? '(+ - = < > and or not) w))
  (match v
    [(? number?) #t]
    [(list (? op?) (? expr?) (? expr?)) #t]
    [(? symbol?) #t]
    [_ #f]))

(define (step? v)
  (match v
    ['skip #t]
    [`(:= ,(? symbol?) ,(? expr?)) #t]
    [`(begin ,@(? (listof step?))) #t]
    ;[`(begin (? skip? skps) ...) #t]
    ;[`(begin ,@(list (? skip?) ..2)) #t]
    [`(if ,(? expr?) ,(? step?) ,(? step?)) #t]
    [`(while ,(? expr?) ,@(? (listof step?))) #t]
    [_ #f]))

;; V = nat | bool
;; A = [X -> V] ...

;; A x E -> V
(define (eval-expr env exp)
  (define (recur exp*)
    (eval-expr env exp*))
  (match exp
    [(? number?) exp]
    [`(+ ,L ,R)
     (+ (recur L) (recur R))]
    [`(- ,L ,R)
     (- (recur L) (recur R))]
    [`(= ,L ,R)
     (= (recur L) (recur R))]
    [`(< ,L ,R)
     (< (recur L) (recur R))]
    [`(> ,L ,R)
     (> (recur L) (recur R))]
    [`(and ,L ,R)
     (and (recur L) (recur R))]
    [`(or ,L ,R)
     (or (recur L) (recur R))]
    [`(not ,L ,R)
     (not (recur L) (recur R))]
    [(? symbol?)
     (hash-ref env exp)]))


;; A x S -> A
(define (apply-step env step)
  (define (recur step*)
    (apply-step env step*))
  (match step
    [`(:= ,(? symbol? var) ,exp)
     (define new-val (eval-expr env exp))
     (hash-set env var new-val)]
    [`(begin ,steps ..1)
     (for/fold ([out-env env])
               ([stp steps])
       (apply-step out-env stp))]
    [`(if ,exp ,S ,T)
     (if (eval-expr env exp)
         (recur S)
         (recur T))]
    [`(while ,exp ,steps ..1)
     (cond [(eval-expr env exp)
            (define new-env (recur `(begin ,@steps)))
            (apply-step new-env step)]
           [else env])]))


;; A x P -> ?



;; S x P -> P (weakest precondition)



(module+ test
  (require chk)
  ;; eval-expr
  (chk 5 (eval-expr (hash) 5))
  (chk 5 (eval-expr (hash 'x 5) 'x))
  (chk 11 (eval-expr (hash 'x 5 'y 6) '(+ x y)))
  (chk #t (eval-expr (hash) '(= 1 1)))
  (chk #f (eval-expr (hash) '(= 0 1)))
  (chk #t (eval-expr (hash) '(< 0 1)))
  (chk #f (eval-expr (hash) '(< 1 0)))
  (chk #t (eval-expr (hash) '(> 1 0)))
  (chk #f (eval-expr (hash) '(> 0 1)))
  ;; apply-step
  (chk (hash 'x 5)
       (apply-step (hash) '(:= x 5)))
  (chk (hash 'x 5 'y 6)
       (apply-step (hash) '(begin (:= x 5) (:= y 6))))
  (chk (hash 'x 0)
       (apply-step (hash) '(if (= 2 2) (:= x 0) (:= y 0))))
  (chk (hash 'y 0)
       (apply-step (hash) '(if (= 2 1) (:= x 0) (:= y 0))))
  (chk (hash 'x 5)
       (apply-step (hash 'x 0) '(while (< x 5) (:= x (+ x 1)))))
  (chk (hash 'x 5 'y 10)
       (apply-step (hash 'x 0) '(while (< x 5) (:= x (+ x 1)) (:= y (+ x x)))))
  )