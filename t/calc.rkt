#lang racket/base
(require adqc
         racket/file
         racket/system)

(define char* (ExtT (ExternSrc '() '()) "char*"))
(define stdlib-h (ExternSrc '() '("stdlib.h")))
(define atoi (ExtFun stdlib-h
                     (list (Arg 'nptr char* 'read-only))
                     (T S32)
                     "atoi"))

(define main
  (F ([argc : S32] [argv : (array 4 #,char*)]) : S32
     (assert! #:dyn #:msg "exactly 3 arguments supplied" ; 4 args w/ user cmd
              (ieq argc (S32 4)))
     (define n1 : S32 := atoi <- (argv @ 1))
     (define n2 : S32 := atoi <- (argv @ 3))
     ;; XXX Need a way to dereference op argument to see what kind of character
     ;; it is (+, -, etc.), so we can dispatch on that. Also need way to print
     ;; result rather than return it from main.
     (iadd n1 n2)))

(define calc (Prog (include-fun "main" main)))

(module+ test
  (require chk)
  ;; XXX Maybe we should have helper functions (like in linker.rkt)
  ;; to improve interacting with executables 
  (define c-path (make-temporary-file))
  (define bin-path (make-temporary-file))
  (unless (compile-exe calc c-path bin-path)
    (newline (current-error-port))
    (define in (open-input-file c-path))
    (for ([ch (in-port read-char in)])
      (display ch (current-error-port)))
    (close-input-port in)
    (delete-file c-path)
    (delete-file bin-path)
    (error "call to compile-exe failed (see stderr)"))
  (define r (system*/exit-code bin-path "2" "+" "2"))
  (chk r 4)
  (delete-file c-path)
  (delete-file bin-path)
  )