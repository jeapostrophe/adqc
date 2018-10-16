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
    ;; XXX It would be nice to have syntax that made returning values
    ;; through reference arguments more ergonomic.
    (F ([rb : #,ringbuf_t] [#:ref out : #,ty]) : S32
       (if (ieq (rb -> count) (U32 0))
           (S32 -1)
           (begin
             (define buf : (array 10 S32) := (rb -> buf))
             (out <- (buf @ (rb -> outptr)))
             ((rb -> outptr) <- (iadd (rb -> outptr) (U32 1)))
             ((rb -> outptr) <- (iurem (rb -> outptr) (U32 10)))
             ((rb -> count) <- (isub (rb -> count) (U32 1)))
             (S32 0)))))
  (ringbuf-spec ringbuf_t make-ringbuf ringbuf-push ringbuf-pop))

(provide
 (contract-out
  [specify-ringbuf (-> exact-nonnegative-integer? Type? ringbuf-spec?)]
  [struct ringbuf-spec ([ty Type?] [make Fun?] [push Fun?] [pop Fun?])]))

(module+ test
  (require chk ffi/unsafe)
  (define spec (specify-ringbuf 10 (T S32)))
  (define ringbuf_t (ringbuf-spec-ty spec))
  (define p (Prog (include-fun "make_ringbuf" (ringbuf-spec-make spec))
                  (include-fun "ringbuf_push" (ringbuf-spec-push spec))
                  (include-fun "ringbuf_pop" (ringbuf-spec-pop spec))))
  (define lp (link-program p))
  (define lp-alloc (linked-program-alloc lp))
  (define buf (lp-alloc (T (array 10 S32))))
  (define rb (lp-alloc ringbuf_t))
  (define i (lp-alloc (T S32)))
  (chk*
   (chk (run-linked-program lp "make_ringbuf" (list rb buf)) 0)
   (chk (run-linked-program lp "ringbuf_push" (list rb 5)) 0)
   (chk (run-linked-program lp "ringbuf_pop" (list rb i)) 0)
   ;; XXX Right now caller has to use ffi/unsafe to make sense of any
   ;; pointer value returned by native code. Maybe we should provide
   ;; a cleaner interface?
   (chk (ptr-ref i _sint32) 5)
   (chk (run-linked-program lp "ringbuf_pop" (list rb i)) -1)
  ))
