#lang racket/base
(require adqc
         racket/contract/base)

(struct ringbuf-spec (ty make push pop) #:transparent)

(define (specify-ringbuf max-count ty)
  (define ringbuf_t (T (record buf (array max-count #,ty)
                               count U32
                               inptr U32
                               outptr U32)))
  (define make-ringbuf
    (F ([rb : #,ringbuf_t] [arr : (array max-count #,ty)]) : S32
       ;; XXX '<-' infix notation is awkward, it would be nice to have 'set!'.
       ((rb -> buf) <- arr)
       ((rb -> count) <- (U32 0))
       ((rb -> inptr) <- (U32 0))
       ((rb -> outptr) <- (U32 0))
       (S32 0)))
  (define ringbuf-push
    (F ([rb : #,ringbuf_t] [v : #,ty]) : S32
       (if (ieq (rb -> count) (U32 max-count))
           (S32 -1)
           ;; XXX If we had 'cond', we wouldn't need this 'begin'.
           (begin
             (define buf : (array max-count #,ty) := (rb -> buf))
             ((buf @ (rb -> inptr)) <- v)
             ;; XXX It would be nice to have operators like ++, --, +=, %=, etc.
             ((rb -> inptr) <- (iadd (rb -> inptr) (U32 1)))
             ((rb -> inptr) <- (iurem (rb -> inptr) (U32 max-count)))
             ((rb -> count) <- (iadd (rb -> count) (U32 1)))
             (S32 0)))))
  (define ringbuf-pop
    (F ([rb : #,ringbuf_t]) : #,ty
       (if (ieq (rb -> count) (U32 0))
           (S32 -1)
           (begin
             (define buf : (array 10 S32) := (rb -> buf))
             (define v : #,ty := (buf @ (rb -> outptr)))
             ((rb -> outptr) <- (iadd (rb -> outptr) (U32 1)))
             ((rb -> outptr) <- (iurem (rb -> outptr) (U32 10)))
             ((rb -> count) <- (isub (rb -> count) (U32 1)))
             v))))
  (ringbuf-spec ringbuf_t make-ringbuf ringbuf-push ringbuf-pop))

(provide
 (contract-out
  [specify-ringbuf (-> exact-nonnegative-integer? Type? ringbuf-spec?)]
  [struct ringbuf-spec ([ty Type?] [make Fun?] [push Fun?] [pop Fun?])]))
