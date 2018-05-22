#lang racket/base
(require chk
         racket/contract/base
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



;; A x S -> A



;; A x P -> ?



;; S x P -> P (weakest precondition)



;(module+ test
;  (define (test-skip?)
;    (define (check e)
;      (chk #:? skip? e))
;    (check 'skip)
;    (check '