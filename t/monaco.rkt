#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         adqc
         racket/contract/base
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

(struct game (state-ty action-ty actor-ty legal?-fn) #:transparent)

(define (monaco g)
  (match-define (game state-ty action-ty actor-ty legal?) g)
  (define-T-expander State (syntax-parser [_ #'state-ty]))
  (define-T-expander Action (syntax-parser [_ #'action-ty]))
  (define-T-expander Actor (syntax-parser [_ #'actor-ty]))
  
  (define-global STUNTING := (U8 0))
  (define-global debug? := (U8 0))

  ;; XXX decode-action-keys - how to pull max_key up to compile level?

  (define node-ptr-bits 16)
  (define node-ptr-signed? #f)
  (define the-node-ptr-ty (IntT node-ptr-signed? node-ptr-bits))
  (define-T-expander NodePtr (syntax-parser [_ #'the-node-ptr-ty]))
  (define the-null-node-expr (Int node-ptr-signed? node-ptr-bits 0))
  (define-E-expander NULL-NODE (syntax-parser [_ #'the-null-node-expr]))

  (define POOL-SIZE (sub1 (expt 2 node-ptr-bits)))
  (define SIMULATIONS-PER-ITERATION 4)
  (define MIN-ITERS (/ POOL-SIZE SIMULATIONS-PER-ITERATION))

  (define-type Node
    (record w F32       ;; wins
            v U32       ;; visits
            pr NodePtr  ;; parent
            lc NodePtr  ;; left child
            rs NodePtr  ;; right sibling
            pq NodePtr  ;; prev in queue
            nq NodePtr  ;; next in queue
            ia Action   ;; initiating action
            na Action   ;; next action to expand
            wh Action)) ;; who is acting

  (define-global NODE : Node := #,(ZedI (T Node)))
  (define-global free-ptr := NULL-NODE)
  (define-global node-count := (U32 0))
  (define-global recycled := (U32 0))
  (define-global θ-head := NULL-NODE)

  (define-fun void θ-insert ([NodePtr x])
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
    (set! θ-head x))
  
  (define-fun void θ-remove ([NodePtr x])
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
    (assert! #:dyn (= (NODE @ x -> pq) NULL-NODE)))

  (define (make-do-children fn)
    (F void ([NodePtr pr])
       (define c := (NODE @ pr -> lc))
       (while (!= c NULL-NODE)
         (define void1 := fn <- c)
         (set! c (NODE @ c -> rs)))))
  (define do-children-insert (make-do-children θ-insert))
  (define do-children-remove (make-do-children θ-remove))

  ;; XXX dg_edge, dg_topoprint, dump_graph

  (define-fun void initialize-pool ()
    (define last := NULL-NODE)
    (for ([n : NodePtr := (U32 1)] (< n (U32 POOL-SIZE)) (+=1 n))
      (set! (NODE @ n -> rs) (add1 n))
      (set! last n))
    (set! (NODE @ last -> rs) NULL-NODE)
    (set! free-ptr (U32 1)))

  (define-fun Action next-legal ([State st] [Action prev])
    (while (S32 1)
      (cond [(zero? prev)
             (return 0)]
            [else
             (-=1 prev)
             (define is-legal? := legal? <- st prev)
             (when is-legal?
               (return (add1 prev)))])))

  (define-fun void free-node ([NodePtr n])
    (-=1 node-count)
    (assert! #:dyn (= (NODE @ n -> pq) NULL-NODE))
    (assert! #:dyn (= (NODE @ n -> nq) NULL-NODE))
    (assert! #:dyn (= (NODE @ n -> pr) NULL-NODE))
    (assert! #:dyn (= (NODE @ n -> lc) NULL-NODE))
    (assert! #:dyn (= (NODE @ n -> rs) NULL-NODE))
    (set! (NODE @ n -> rs) free-ptr)
    (set! free-ptr n))

  ;; XXX free-node-rec without recursion

  (define-fun void recycle ([NodePtr curr])
    (define last := (NODE @ θ-head -> pq))
    (when (= last NULL-NODE)
      ;; no last
      (return))
    (define pr := (NODE @ last -> pr))
    (when (= pr NULL-NODE)
      ;; last has no parent
      (return))
    (+=1 recycled)
    (define c := (NODE @ pr -> lc))
    (assert! #:dyn (!= c NULL-NODE))
    (set! (NODE @ pr -> na) (add1 (NODE @ c -> ia)))
    (set! (NODE @ pr -> lc) (NODE @ c -> rs))
    (set! (NODE @ c -> rs) NULL-NODE)
    ;; XXX free-node-rec(c)
    )

  (define-fun NodePtr alloc-node ([NodePtr parent] [Actor lastp] [Action ia] [State st])
    (+=1 node-count)
    (define new := free-ptr)
    (when (= new NULL-NODE)
      (define void1 := recycle <- parent)
      (set! new free-ptr)
      (when (= new NULL-NODE)
        ;; XXX dump_graph ?
        (error "alloc-node: out of meemory"))))
  )

(provide
 (contract-out
  [monaco (-> game? Program?)]
  [struct game ([state-ty non-void-type?]
                [action-ty non-void-type?]
                [actor-ty non-void-type?]
                [legal?-fn Fun?])]))
