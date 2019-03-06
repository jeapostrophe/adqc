#lang racket/base
(require adqc
         racket/file
         racket/system)

(require "../exec.rkt")

(define char* (ExtT (ExternSrc '() '()) "char*"))

(define stdlib-h (ExternSrc '() '("stdlib.h")))
(define atoi (ExtFun stdlib-h (list (Arg 'nptr char* 'read-only)) (T S32) "atoi"))

(define unistd-h (ExternSrc '() '("unistd.h")))
(define c-write (ExtFun unistd-h
                        (list (Arg 'fd (T S32) 'read-only)
                              (Arg 'buf (T (array 1 S32)) 'read-only)
                              (Arg 'nbytes (T S32) 'read-only))
                        (T S32)
                        "write"))

(define main
  (F ([n1 : S32] [op : S32] [n2 : S32])
   #;([argc : S32] [argv : (array 4 (union c-str #,char* ch-ptr (array 1 S8)))]) : S32
     #;(assert! #:dyn #:msg "exactly 3 arguments supplied" ; 4 args w/ user cmd
              (ieq argc (S32 4)))
     #;(define n1 : S32 := atoi <- (argv @ (S32 1) as c-str))
     #;(define n2 : S32 := atoi <- (argv @ (S32 3) as c-str))
     #;(define op :  S8 := (argv @ (S32 2) as ch-ptr @ (S32 0)))
     ;; XXX Right now now this always results in the 'else' case because
     ;; the 'ch-ptr' part of argv is being compiled as storage, not as
     ;; a pointer. One of the side effects of treating union members as
     ;; values and not references is that we can no longer use unions
     ;; to pun over C-strings by pretending we know how long they are.
     (cond [(ieq op (S32 (char->integer #\+)))
            (iadd n1 n2)]
           [(ieq op (S32 (char->integer #\-)))
            (isub n1 n2)]
           [(ieq op (S32 (char->integer #\*)))
            (imul n1 n2)]
           [(ieq op (S32 (char->integer #\/)))
            (isdiv n1 n2)]
           [else (error "invalid op\n")])))

(define calc (Prog (include-fun "main" main)))

(module+ test
  (require chk)
  ;; XXX Maybe we should have helper functions (like in linker.rkt)
  ;; to improve interacting with executables 
  (define c-path (make-temporary-file "adqc~a.c"))
  (define bin-path (make-temporary-file "adqc~a"))
  (unless (make-exe calc c-path bin-path)
    (newline (current-error-port))
    (define in (open-input-file c-path))
    (for ([ch (in-port read-char in)])
      (display ch (current-error-port)))
    (close-input-port in)
    (delete-file c-path)
    (delete-file bin-path)
    (error "call to compile-exe failed (see stderr)"))
  ;(define r (system*/exit-code bin-path "2" "+" "2"))
  ;(chk r 4)
  (delete-file c-path)
  (delete-file bin-path)
  )