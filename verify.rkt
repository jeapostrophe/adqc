#lang racket/base
(require racket/contract/base
         racket/match
         "ast.rkt"
         "stx.rkt")

(define (weakest-precond stmt post-cond)
  (match stmt
    [(Skip _) post-cond]
    [(Fail _) (U32 0)]
    [(Assign (Var x _) e)
     (subst x e post-cond)]
    [(Begin L-stmt R-stmt)
     (define post-cond* (weakest-precond R-stmt post-cond))
     (weakest-precond L-stmt post-cond*)]
    [(If pred then else)
     (And (Implies pred
                   (weakest-precond else post-cond))
          (Implies (Not pred)
                   (weakest-precond then post-cond)))]
    [(While pred invar do-stmt)
     (And invar
          (And (Implies (And pred invar)
                        (weakest-precond do-stmt invar))
               (Implies (And (Not pred) invar)
                        post-cond)))]
    [(Jump label)
     (Var label)]
    [(Let/ec label stmt)
     (subst label post-cond (weakest-precond stmt post-cond))]))

(define (subst x v e)
  (define (rec e) (subst x v e))
  (match e
    [(Var (== x) _) v]
    [(or (? Var?) (? Int?) (? Flo?)) e]
    [(BinOp op L R)
     (BinOp op (rec L) (rec R))]))

;; XXX strongest postcondition
;;
;; How to deal with Return? I think you compute the post-condition as
;; you go and the recursive call gives you the main post-cond, plus a
;; list of post-conds for each label and you OR them together.
;;
;; https://www.risc.jku.at/education/oldmoodle/file.php/22/slides/02-hoare.pdf

;; XXX verify! function that compiler needs (notes about it here:)

;; As we are compiling, we could compute the strongest
;; post-condition (SP) of the code that came before this point and
;; then check the theorem (not (SP => P)) for UNSAT. If it is SAT,
;; then the condition is not verified (#f), if it is UNSAT, then the
;; condition is checked.
;;
;; It is awkard to have the compiler interact with the theorem
;; prover this way though, particularly having to compute the SP. So
;; another idea is to have the verifier run first and return a weak
;; hash-table mapping each precondition to whether it can be SAT or
;; NOT in this way, then compiler can consult the table.
;;
;; Alternatively, verify! could be a Program -> Program function
;; that simply removes the Asserts as it verifies them and the
;; compiler always treats them as comments... I think that may be
;; the most beautiful way, but it will take a lot of allocation, :(
;; That could actually be quite cool, because may it could also use
;; the SP to do optimization and constant propagation of something
;; like that.

;; eval
#;[(Assert _ p msg)
   (or (and (eval-expr-pred σ p) σ)
       (error 'Assert "Failed assertion: ~e" msg))]

;; compile
#;[(Assert must-be-static? p msg)
   (list* "/* ASSERT " msg ": " (compile-expr ρ p) " */" ind-nl
          (cond
            [(verify! p)
             "/* Statically verified! */"]
            [(not must-be-static?)
             (compile-stmt γ ρ (If p (Skip) (Fail (~a "Assertion failed: " msg))))]
            [else
             (error 'compile "Assertion not verifiable statically: ~a" msg)]))]

;;;; Interval Arithmetic
(struct ival (l e h) #:transparent)
(define (iunit x) (ival x x x))
(define (ival+ x y)
  (match-define (ival lx ex hx) x)
  (match-define (ival ly ey hy) y)
  (ival (+ lx ly) (+ ex ey) (+ hx hy)))
(define (ivalU P x y)
  (match-define (ival lx ex hx) x)
  (match-define (ival ly ey hy) y)
  (ival (min lx ly)
        (+ (* P ex) (* (- 1 P) ey))
        (max hx hy)))
(define (ival*k l e h x)
  (match-define (ival lx ex hx) x)
  (ival (* l lx) (* e ex) (* h hx)))
;;;; / Interval Arithmetic

;; XXX Bound the trips through Whiles

;; XXX Some way to enforce that a value is looked at (could be a
;; generalization of ReadOnly?)
