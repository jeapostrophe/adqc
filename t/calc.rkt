#lang racket/base
(require adqc
         racket/file
         racket/system)

(define char* (ExtT (ExternSrc '() '()) "char*"))
(define stdlib-h (ExternSrc '() '("stdlib.h")))
(define atoi (ExtFun stdlib-h (list (Arg 'nptr char* 'read-only)) (T S32) "atoi"))

(define main
  (F ([argc : S32] [argv : (array 4 (union c-str #,char* ch-ptr (array 1 S8)))]) : S32
     (assert! #:dyn #:msg "exactly 3 arguments supplied" ; 4 args w/ user cmd
              (ieq argc (S32 4)))
     (define n1 : S32 := atoi <- (argv @ (S32 1) as c-str))
     (define n2 : S32 := atoi <- (argv @ (S32 3) as c-str))
     (define op :  S8 := (argv @ (S32 2) as ch-ptr @ (S32 0)))
     (cond [(ieq op #,(N (char->integer #\+)))
            (iadd n1 n2)]
           [(ieq op #,(N (char->integer #\-)))
            (isub n1 n2)]
           [(ieq op #,(N (char->integer #\*)))
            (imul n1 n2)]
           [(ieq op #,(N (char->integer #\/)))
            (isdiv n1 n2)]
           [else (error "invalid op")])))

(define calc (Prog (include-fun "main" main)))

(module+ test
  (require chk)
  ;; XXX Maybe we should have helper functions (like in linker.rkt)
  ;; to improve interacting with executables 
  (define c-path (make-temporary-file "adqc~a.c"))
  (define bin-path (make-temporary-file "adqc~a"))
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