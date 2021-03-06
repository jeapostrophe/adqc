#lang racket/base
(require adqc
         racket/file
         racket/list
         racket/runtime-path
         racket/system)

(define-runtime-path 2048-exe-path "2048")
(define-runtime-path 2048-h-path "2048.h")
(define-runtime-path 2048-c-path "2048.c")
(define 2048-c (ExternSrc '() (list (path->string 2048-c-path))))

(define SIZE 4)

(define-type Row (array SIZE U8))
(define-type Board (array SIZE Row))

(define-global score : U32 := (U32 0))

(define-fun void make-board ([Board b] [Row r1] [Row r2] [Row r3] [Row r4])
  (set! (b @ 0) r1)
  (set! (b @ 1) r2)
  (set! (b @ 2) r3)
  (set! (b @ 3) r4))

(define-fun U8 find-target ([Row r] [U8 x] [U8 stop])
  ;; If the position is already on the first, don't evaluate
  (when (zero? x)
    (return x))
  (for ([t := (sub1 x)] (S32 1) (-=1 t))
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

(define-fun U8 slide-array ([Row r])
  (define success := (U8 0))
  (define stop := (U8 0))
  (for ([x := (U8 0)] (< x (U8 SIZE)) (+=1 x))
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
               ;; increase score
                (+= score (<< 1 ((r @ t) : U32)))
               ;; set stop to avoid double merge
               (set! stop (add1 t))]
              [else (void)])
        (set! (r @ x) 0)
        (set! success 1))))
  (return success))

(define-fun void rotate-board ([Board board])
  ;; XXX This var really only exists because it's awkward to embed
  ;; SIZE in the individual expressions - SIZE is interpreted as an S8,
  ;; which is incompatible with U8, so it would require typing (U8 SIZE)
  ;; everwhere we use 'n' now.
  (define n := (U8 SIZE))
  (for ([i := (U8 0)] (< i (/ n (U8 2))) (+=1 i))
    ;; XXX Allow path for var init?
    (for ([j : U8 := i] (< j (sub1 (- n i))) (+=1 j))
      (define tmp := (board @ i @ j))
      ;; XXX Implement var args for ops so we can just do (- n i 1)
      (set! (board @ i @ j)
            (board @ j @ (sub1 (- n i))))
      (set! (board @ j @ (sub1 (- n i)))
            (board @ (sub1 (- n i)) @ (sub1 (- n j))))
      (set! (board @ (sub1 (- n i)) @ (sub1 (- n j)))
            (board @ (sub1 (- n j)) @ i))
      (set! (board @ (sub1 (- n j)) @ i) tmp))))


(define-fun U8 move-up ([Board board])
  (define success := (U8 0))
  (for ([x := (U8 0)] (< x (U8 SIZE)) (+=1 x))
    (define sa-result := (slide-array (board @ x)))
    (set! success (bitwise-ior success sa-result)))
  (return success))

(define-fun+ U8 move-left ([Board b])
  (rotate-board b)
  (define success (move-up b))
  (rotate-board b)
  (rotate-board b)
  (rotate-board b)
  success)

(define-fun+ U8 move-down ([Board b])
  (rotate-board b)
  (rotate-board b)
  (define success (move-up b))
  (rotate-board b)
  (rotate-board b)
  success)

(define-fun+ U8 move-right ([Board b])
  (rotate-board b)
  (rotate-board b)
  (rotate-board b)
  (define success (move-up b))
  (rotate-board b)
  success)

(define-fun U8 find-pair-down ([Board board])
  (for ([x := (U8 0)] (< x (U8 SIZE)) (+=1 x))
    (for ([y := (U8 0)] (< y (sub1 (U8 SIZE))) (+=1 y))
      (when (= (board @ x @ y) (board @ x @ (add1 y)))
        (return 1))))
  (return 0))

;; The U8 here is a number, not a bool
;; XXX Maybe support boolean type so this is less confusing? stdbool?
(define-fun U8 count-empty ([Board board])
  (define count := (U8 0))
  (for ([x := (U8 0)] (< x (U8 SIZE)) (+=1 x))
    (for ([y := (U8 0)] (< y (U8 SIZE)) (+=1 y))
      (when (zero? (board @ x @ y))
        (+=1 count))))
  (return count))


(define-fun U8 game-ended ([Board board])
  ;; XXX Awkward because no ANF. Need can't put function call inside
  ;; predicate, so we need 2 conditional statements instead of an 'or'
  (define num-empty := (count-empty board))
  (when (> num-empty (U8 0))
    (return 0))
  (define is-pair-down := (find-pair-down board))
  (when is-pair-down
    (return 0))
  (define void1 := (rotate-board board))
  (define is-pair-down2 := (find-pair-down board))
  (define ended := (not is-pair-down2))
  (define void2 := (rotate-board board))
  (define void3 := (rotate-board board))
  (define void4 := (rotate-board board))
  (return ended))

;; XXX Issue with type of first cond case, I think related to
;; 'or' macro?
#;
(define-fun+ U8 game-ended ([Board b])
  (cond [(or (not (zero? (count-empty b))) (find-pair-down b)) 0]
        [else
         (rotate-board b)
         (defined ended (not (find-pair-down b)))
         (rotate-board b)
         (rotate-board b)
         (rotate-board b)
         ended]))

(define-extern-fun S32 rand () #:src (ExternSrc '() '("stdlib.h")))

(define-fun void add-random ([Board board])
  (define x-lst : (array (* SIZE SIZE) U8))
  (define y-lst : (array (* SIZE SIZE) U8))
  (define len := (U8 0))
  (for ([x := (U8 0)] (< x (U8 SIZE)) (+=1 x))
    (for ([y := (U8 0)] (< y (U8 SIZE)) (+=1 y))
      (when (zero? (board @ x @ y))
        (set! (x-lst @ len) x)
        (set! (y-lst @ len) y)
        (+=1 len))))
  (unless (zero? len)
    (define rand-ret-1 := (rand))
    (define r := (% (rand-ret-1 : U8) len))
    (define x := (x-lst @ r))
    (define y := (y-lst @ r))
    (define rand-ret-2 := (rand))
    (define n : U8 := (add1 (/ (% (rand-ret-2 : U8) 10) 9)))
    (set! (board @ x @ y) n)))

(define-fun void init-board ([Board board])
  (for ([x := (U8 0)] (< x (U8 SIZE)) (+=1 x))
    (for ([y := (U8 0)] (< y (U8 SIZE)) (+=1 y))
      (set! (board @ x @ y) 0)))
  (define void1 := (add-random board))
  (define void2 := (add-random board))
  (define void3 := (add-random board))
  (set! score 0))

(define-fun U8 step ([Board board] [S8 c])
  (define success : U8)
  ;; This is another example of the difference between S-cond and E-cond
  ;; being annoying. We can't just say '(define success := (cond ...))'
  ;; because we call a function from within the cond stmt. ANF should fix this.
  (cond
    ;; left arrow
    [(= c 68)
     (define result := (move-left board))
     (set! success result)]
    ;; right arrow
    [(= c 67)
     (define result := (move-right board))
     (set! success result)]
    ;; up arrow
    [(= c 65)
     (define result := (move-up board))
     (set! success result)]
    ;; down arrow
    [(= c 66)
     (define result := (move-down board))
     (set! success result)]
    [else (set! success 0)])
  (return success))
  

(define-prog 2048-prog
  (include-type Row)
  (include-type Board)
  (include-global score)
  (include-fun make-board)
  (include-fun init-board)
  (include-fun add-random)
  (include-fun game-ended)
  (include-fun step))

(module+ test
  (require chk)
  (define c-path (make-temporary-file "adqc~a.c"))
  (define o-path (make-temporary-file "adqc~a.o"))
  (unless (compile-obj 2048-prog c-path o-path 2048-h-path)
    (newline (current-error-port))
    (define in (open-input-file c-path))
    (for ([ch (in-port read-char in)])
      (display ch (current-error-port)))
    (close-input-port in)
    (delete-file c-path)
    (delete-file o-path)
    (error "compile-obj failed (see stderr)"))
  (define cc (find-executable-path "cc"))
  ;; XXX There should be an interface for including obj files
  ;; when calling compile-exe so we don't have to call cc directly.
  (unless (system* cc "-Wall" "-Werror" "-o" 2048-exe-path 2048-c-path o-path)
    (delete-file c-path)
    (delete-file o-path)
    (error "call to cc failed (see stderr)"))    
  (delete-file c-path)
  (delete-file o-path))
