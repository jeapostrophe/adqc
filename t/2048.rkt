#lang racket/base
(require adqc
         racket/list
         racket/runtime-path)

(define-runtime-path 2048-c-path "2048.c")
(define 2048-c (ExternSrc '() (list (path->string 2048-c-path))))

(define SIZE 4)

(define-type Row (array SIZE U8))
(define-type Board (array SIZE Row))

(define-fun (make-board [b : Board] [r1 : Row] [r2 : Row] [r3 : Row] [r4 : Row]) : S32
  (set! (b @ 0) r1)
  (set! (b @ 1) r2)
  (set! (b @ 2) r3)
  (set! (b @ 3) r4)
  (return 0))

(define-fun (find-target [r : Row] [x : U8] [stop : U8]) : U8
  ;; If the position is already on the first, don't evaluate
  (when (zero? x)
    (return x))
  (for [t := (sub1 x)] (S32 1) (-=1 t)
       (cond [(not (zero? (r @ t)))
              (unless (= (r @ t) (r @ x))
                ;; merge is not possible, take next position
                (return (add1 t)))
              (return t)]
             [else
              ;; we should not slide further, return this one
              (when (= t stop)
                (return t))]))
  ;; we did not find a
  (return x))

(define-fun (slide-array [r : Row]) : U8
  (define success := (U8 0))
  (define stop := (U8 0))
  (for [x := (U8 0)] (< x (U8 SIZE)) (+=1 x)
       (when (not (zero? (r @ x)))
         (define t := find-target <- r x stop)
         ;; if target is not original position, then move or merge
         (unless (= t x)
           ;; if target is zero, this is a move
           (cond [(zero? (r @ t))
                  (set! (r @ t) (r @ x))]
                 [(= (r @ t) (r @ x))
                  ;; merge (increase power of two)
                  (+=1 (r @ t))
                  ;; XXX increase score
                  ;; set stop to avoid double merge
                  (set! stop (add1 t))]
                 [else (void)])
           (set! (r @ x) 0)
           (set! success 1))))
  (return success))

(define-fun (add-random [b : Board]) : S32
  ;; XXX Need to allocate 2-dimensional array for this? Need an
  ;; automated way to deal with those.
  (return 0))

(define-fun (step [b : Board] [c : S8]) : S32
  (return 0))
