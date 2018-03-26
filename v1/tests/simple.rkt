#lang racket/base
(require "../ast.rkt"
         "../interp.rkt")

;; Examples
(module+ test
  (require (for-syntax racket/base)
           syntax/location)
  (define-syntax (h stx)
    #`(quote-srcloc #,stx))
  (define N 12)
  (define LinearSearch
    (Proc h S8
          (list (cons 'x U8))
          (list (cons 'array (ArrT N U8)))
          (list)
          (Seq
           h
           (Loop h 'main U8 'i N (IntV h U8 N)
                 (If h (Bin h 'ieq (VarR h 'i) (VarR h 'x))
                     (Return h (Cast h S8 (VarR h 'i)))
                     (Continue h 'main)))
           (Return h (IntV h S8 -1)))))
  (define Main
    (Proc h U8 (list) (list) (list)
          (Let h #t 'a (ArrV h (for/list ([i (in-range N)])
                                 (IntV h U8 i)))
               (Let h #t 'res (Call h LinearSearch
                                    (list (IntV h U8 5)) ;; <-- look for 5
                                    (list (VarLHS h 'a))
                                    (list))
                    (If h (Bin h 'islt (VarR h 'res) (IntV h S8 0))
                        (Return h (IntV h U8 1))
                        (Return h (IntV h U8 0)))))))
  (adqc-eval Main))

;; xxx make NES synth example

;; xxx compute space and time

;; xxx liveness analysis of variables to compute how to re-use space

;; xxx MIC-1 simulator/assembler/microcompiler

;; xxx entity component system (Artemis, EntityX, Anax)

;; xxx modules/units/components

;; xxx FSM language
