#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         adqc
         racket/match)

#|
(adqc-interface game
                (type actor)
                (type state)
                (type action)
                (fun bool terminal_p (state)))

(adqc-module game (ttt)
             (implements game)
             (define-type actor int64))

(adqc-module exe (monaco game)
             ....)

(adqc-compile (monaco (ttt)))
|#

(define-T-expander Action
  (syntax-parser [_ (syntax/loc this-syntax (T U8))]))
(define-T-expander Actor
  (syntax-parser [_ (syntax/loc this-syntax (T U8))]))

(struct game (state-ty) #:transparent)

(define (monaco g)
  (match-define (game state-ty) g)
  (define-T-expander State (syntax-parser [_ #'state-ty]))
  
  (define-global STUNTING := (U8 0))
  (define-global debug? := (U8 0))

  ;; XXX decode-action-keys - how to pull max_key up to compile level?

  (define POOL-SIZE (sub1 (expt 2 16)))
  (define-T-expander NodePtr
    (syntax-parser [_ (syntax/loc this-syntax (T U16))]))
  (define-E-expander NULL-NODE
    (syntax-parser [_ (syntax/loc this-syntax (E (U16 0)))]))

  (define SIMULATIONS-PER-ITERATION 4)
  (define MIN-ITERS (/ POOL-SIZE SIMULATIONS-PER-ITERATION))

  (define-type Node
    (record w F32
            v U32
            pr NodePtr
            lc NodePtr
            rs NodePtr
            pq NodePtr
            nq NodePtr
            ia Action
            na Action
            wh Action))

  (define-global NODE : Node := #,(ZedI (T Node)))
  (define-global free-ptr := NULL-NODE)
  (define-global node-count := (U32 0))
  (define-global recycled := (U32 0))
  (define-global θ-head := NULL-NODE)

  (define-fun S32 θ-insert ([NodePtr x])
    (assert! #:dyn (!= (NODE @ x -> nq) NULL-NODE))
    (assert! #:dyn (!= (NODE @ x -> pq) NULL-NODE))
    (cond [(= θ-head NULL-NODE)
           (set! (NODE @ x -> pq) x)
           (set! (NODE @ x -> nq) x)]
          [else
           (assert! #:dyn (!= (NODE @ θ-head -> nq) NULL-NODE))
           (assert! #:dyn (!= (NODE @ θ-head -> pq) NULL-NODE))
           (set! (NODE @ x -> nq) θ-head)
           (set! (NODE @ x -> pq) (NODE @ θ-head -> nq))
           (set! (NODE @ (NODE @ θ-head -> pq) -> nq) x)
           (set! (NODE @ θ-head -> pq) x)])
    (assert! #:dyn (!= (NODE @ x -> nq) NULL-NODE))
    (assert! #:dyn (!= (NODE @ x -> pq) NULL-NODE))
    (set! θ-head x)
    (return 0))
  
  (define-fun S32 θ-remove ([NodePtr x])
    (define nq := (NODE @ x -> nq))
    (define pq := (NODE @ x -> pq))
    (set! (NODE @ x -> nq) NULL-NODE)
    (set! (NODE @ x -> nq) NULL-NODE)
    (cond [(= nq x)
           (assert! #:dyn (= pq x))
           (assert! #:dyn (= θ-head x))
           (set! θ-head NULL-NODE)]
          [else
           (set! (NODE @ pq -> nq) nq)
           (set! (NODE @ nq -> pq) pq)
           (when (= θ-head x)
             (set! θ-head nq))])
    (assert! #:dyn (= (NODE @ x -> nq) NULL-NODE))
    (assert! #:dyn (= (NODE @ x -> pq) NULL-NODE))
    (return 0))
  )

(provide Action Actor)
