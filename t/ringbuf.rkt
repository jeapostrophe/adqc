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
       (set! (rb -> buf) arr)
       (set! (rb -> count) 0)
       (set! (rb -> inptr) 0)
       (set! (rb -> outptr) 0)
       (return 0)))
  (define ringbuf-push
    (F ([rb : #,ringbuf_t] [v : #,ty]) : S32
       (cond [(ieq (rb -> count) (U32 max-count))
              (return -1)]
             [else
              (define buf : (array max-count #,ty) := (rb -> buf))
              (set! (buf @ (rb -> inptr)) v)
              ;; XXX It would be nice to have ++, --, +=, %=, etc.
              (set! (rb -> inptr) (iadd (rb -> inptr) (U32 1)))
              (set! (rb -> inptr) (iurem (rb -> inptr) (U32 max-count)))
              (set! (rb -> count) (iadd (rb -> count) (U32 1)))
              (return 0)])))
  (define ringbuf-pop
    ;; XXX It would be nice to have syntax that made returning values
    ;; through reference arguments more ergonomic.
    (F ([rb : #,ringbuf_t] [#:ref out : #,ty]) : S32
       (cond [(ieq (rb -> count) (U32 0))
              (return -1)]
             [else
              (define buf : (array max-count S32) := (rb -> buf))
              (set! out (buf @ (rb -> outptr)))
              (set! (rb -> outptr) (iadd (rb -> outptr) (U32 1)))
              (set! (rb -> outptr) (iurem (rb -> outptr) (U32 max-count)))
              (set! (rb -> count) (isub (rb -> count) (U32 1)))
              (return 0)])))
  (ringbuf-spec ringbuf_t make-ringbuf ringbuf-push ringbuf-pop))

(provide
 (contract-out
  [specify-ringbuf (-> exact-nonnegative-integer? Type? ringbuf-spec?)]
  [struct ringbuf-spec ([ty Type?] [make Fun?] [push Fun?] [pop Fun?])]))

(module+ test
  (require chk)
  (define spec (specify-ringbuf 10 (T S32)))
  (define ringbuf_t (ringbuf-spec-ty spec))
  (define p (Prog (include-fun "make_ringbuf" (ringbuf-spec-make spec))
                  (include-fun "ringbuf_push" (ringbuf-spec-push spec))
                  (include-fun "ringbuf_pop" (ringbuf-spec-pop spec))))
  (define lp (link-program p))
  (define (lp-alloc ty) (linked-program-alloc lp ty))
  (define buf (lp-alloc (T (array 10 S32))))
  (define rb (lp-alloc ringbuf_t))
  (define i (lp-alloc (T S32)))
  (chk*
   (chk (linked-program-run lp "make_ringbuf" (list rb buf)) 0)
   (chk (linked-program-run lp "ringbuf_push" (list rb 5)) 0)
   (chk (linked-program-run lp "ringbuf_pop" (list rb i)) 0)
   (chk (linked-program-read lp i) 5)
   (chk (linked-program-run lp "ringbuf_pop" (list rb i)) -1)
  ))
