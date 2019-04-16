#lang racket/base
(require adqc
         racket/list
         racket/runtime-path)

(define-runtime-path 2048-h-path "2048.h")
(define 2048-h (ExternSrc '() (list 2048-h-path)))
(define set-buffered-input
  (ExtFun 2048-h (list (Arg 'enable (T S32) 'read-only))
          (T S32) "set_buffered_input"))

(define stdio-h (ExternSrc '() '("stdio.h")))
(define getchar (ExtFun stdio-h '() (T S32) "getchar"))

(define SIZE 4)
(define board_t (T (array 4 (array 4 U8))))

(define add-random
  (F ([board : #,board_t]) : S32
     ;; XXX add syntax for for loop before implementing this
     (return 0)))

;; XXX Should main be an unsafe C function? ADQC code would then be a
;; "step" function called with the input from getchar. make-executable
;; wouldn't work with this, would need to call compile-exe directly.
(define main
  (F () : S32
     ;; Init board
     (define board : #,board_t :=
       (array #,@(make-list SIZE (I (zero (array SIZE U8))))))
     ;; XXX void function, syntax so we don't have to store result?
     (define unused := set-buffered-input <- 1)
     (return 0)))