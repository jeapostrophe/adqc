#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         adqc)

(define (specify-ringbuf max-count ty)
  (define ringbuf_t (T (record buf (array #,max-count #,ty)
                               count U32
                               spaces U32
                               inptr U32
                               outptr U32)))
  ringbuf_t)

#|
(define ringbuf_t (T (record buf (array 10 S32)
                             count U32
                             spaces U32
                             inptr U32
                             outptr U32)))
(define make-ringbuf
  (F ([rb : #,ringbuf_t] [arr : (array 10 S32)]) : S32
     ((rb -> buf) <- arr)
     ((rb -> count) <- (U32 0))
     ((rb -> spaces) <- (U32 10))
     ((rb -> inptr) <- (U32 0))
     ((rb -> outptr) <- (U32 0))
     (S32 0)))

(define ringbuf-push
  (F ([rb : #,ringbuf_t] [v : S32]) : S32
     (if (ieq (rb -> spaces) (U32 0))
         (S32 -1)
         (begin (define buf : (array 10 S32) := (rb -> buf))
                ((buf @ (rb -> inptr)) <- v)
                ((rb -> inptr) <- (iadd (rb -> inptr) (U32 1)))
                ((rb -> inptr) <- (iurem (rb -> inptr) (U32 10)))
                ((rb -> count) <- (iadd (rb -> count) (U32 1)))
                ((rb -> spaces) <- (isub (rb -> spaces) (U32 1)))
                (S32 0)))))

(define ringbuf-pop
  (F ([rb : #,ringbuf_t]) : S32
     (if (ieq (rb -> count) (U32 0))
         (S32 -1)
         (begin (define buf : (array 10 S32) := (rb -> buf))
                (define v : S32 := (buf @ (rb -> outptr)))
                ((rb -> outptr) <- (iadd (rb -> outptr) (U32 1)))
                ((rb -> outptr) <- (iurem (rb -> outptr) (U32 10)))
                ((rb -> count) <- (isub (rb -> count) (U32 1)))
                ((rb -> spaces) <- (iadd (rb -> spaces) (U32 1)))
                v))))

(define-syntax (specify-ringbuf stx)
  (syntax-parse stx
    [(_ max-count:nat ty)
     (syntax/loc stx
       (begin
         (define ringbuf_t (T (record buf (array max-count ty)
                                      count U32
                                      spaces U32
                                      inptr U32
                                      outptr U32)))
         (define prog
           (Prog
            (define-fun (make_ringbuf [rb : #,ringbuf_t] [arr : (array max-count ty)]) : S32
              ((rb -> buf) <- arr)
              ((rb -> count) <- (U32 0))
              ((rb -> spaces) <- (U32 max-count))
              ((rb -> inptr) <- (U32 0))
              ((rb -> outptr) <- (U32 0))
              (S32 0))
            (define-fun (ringbuf_push [rb : #,ringbuf_t] [v : ty]) : S32
              (if (ieq (rb -> spaces) (U32 0))
                  (S32 -1)
                  (begin (define buf : (array max-count ty) := (rb -> buf))
                         ((buf @ (rb -> inptr)) <- v)
                         ((rb -> inptr) <- (iadd (rb -> inptr) (U32 1)))
                         ((rb -> inptr) <- (iurem (rb -> inptr) (U32 max-count)))
                         ((rb -> count) <- (iadd (rb -> count) (U32 1)))
                         ((rb -> spaces) <- (isub (rb -> spaces) (U32 1)))
                         (S32 0))))
            (define-fun (ringbuf_pop [rb : #,ringbuf_t]) : S32
              (if (ieq (rb -> count) (U32 0))
                  (S32 -1)
                  (begin (define buf : (array max-count ty) := (rb -> buf))
                         (define v : S32 := (buf @ (rb -> outptr)))
                         ((rb -> outptr) <- (iadd (rb -> outptr) (U32 1)))
                         ((rb -> outptr) <- (iurem (rb -> outptr) (U32 max-count)))
                         ((rb -> count) <- (isub (rb -> count) (U32 1)))
                         ((rb -> spaces) <- (iadd (rb -> spaces) (U32 1)))
                         v)))))
         (values ringbuf_t prog)))]))|#