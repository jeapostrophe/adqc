#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         adqc
         (rename-in "monaco.rkt"
                    (Action the-action-ty)
                    (Actor the-actor-ty)))

;; XXX Some way to preserve expander type when providing/requiring?
(define-T-expander Action
  (syntax-parser [_ (syntax/loc this-syntax the-action-ty)]))
(define-T-expander Actor
  (syntax-parser [_ (syntax/loc this-syntax the-actor-ty)]))

(define ROWS 3)
(define COLS 3)
(define SLOTS (* ROWS COLS))

(define PLAYER-IDX 0)
(define O-START (add1 PLAYER-IDX))
(define O-LEN SLOTS)
(define X-START (+ O-START O-LEN 1))
(define X-LEN SLOTS)

(define-T-expander State
  (syntax-parser [_ (syntax/loc this-syntax (T U32))]))
(define-T-expander Board
  (syntax-parser [_ (syntax/loc this-syntax (T U16))]))
