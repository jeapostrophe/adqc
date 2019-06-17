#lang racket/base
(require adqc
         racket/contract/base)

(struct ringbuf-spec (ty make push pop) #:transparent)

(define (specify-ringbuf max-count ty)
  (define-type buf_t (array max-count #,ty))
  (define-type ringbuf_t (record buf buf_t
                                 count U32
                                 inptr U32
                                 outptr U32))
  (define-fun (make-ringbuf [rb : ringbuf_t] [arr : buf_t]) : S32
    (set! (rb -> buf) arr)
    (set! (rb -> count) 0)
    (set! (rb -> inptr) 0)
    (set! (rb -> outptr) 0)
    (return 0))
  (define-fun (ringbuf-push [rb : ringbuf_t] [v : #,ty]) : S32
    (cond [(= (rb -> count) (U32 max-count))
           (return -1)]
          [else
           (define buf := (rb -> buf))
           (set! (buf @ (rb -> inptr)) v)
           (+=1 (rb -> inptr))
           (%= (rb -> inptr) (U32 max-count))
           (+=1 (rb -> count))
           (return 0)]))
  ;; XXX It would be nice to have syntax that made returning values
  ;; through reference arguments more ergonomic.
  (define-fun (ringbuf-pop [rb : ringbuf_t] [#:ref out : #,ty]) : S32
    (cond [(zero? (rb -> count))
           (return -1)]
          [else
           (define buf := (rb -> buf))
           (set! out (buf @ (rb -> outptr)))
           (+=1 (rb -> outptr))
           (%= (rb -> outptr) (U32 max-count))
           (-=1 (rb -> count))
           (return 0)]))
  (ringbuf-spec ringbuf_t make-ringbuf ringbuf-push ringbuf-pop))

(provide
 (contract-out
  [specify-ringbuf (-> exact-nonnegative-integer? Type? ringbuf-spec?)]
  [struct ringbuf-spec ([ty Type?] [make Fun?] [push Fun?] [pop Fun?])]))

(module+ test
  (require chk racket/match)
  (match-define (ringbuf-spec ringbuf_t make-ringbuf ringbuf-push ringbuf-pop)
    (specify-ringbuf 10 (T S32)))
  (define-prog p
    (include-fun make-ringbuf)
    (include-fun ringbuf-push)
    (include-fun ringbuf-pop))
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
