#lang racket/base
(require adqc
         racket/file
         racket/system)

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
  (F ([n1 : S32] [op : S32] [n2 : S32]) : S32
     ;; Because we have no way to pass ASCII values from the command line
     ;; (currently exec will only convert arguments to integer or float values)
     ;; we alias 1 as +, 2 as -, 3 as *, 4 as /
     
     ;; XXX There should be some unsafe C-routine that can be used to get
     ;; character values from the command line. Maybe we would infer that `S8`
     ;; arguments are ASCII characters and that we should just derefernce the
     ;; corresponding argv isntead of passing it to atoi or atof?

     ;; XXX Currently the calculator returns its result as the program's exit
     ;; code. We should be able to write the result to stdout instead.
     (cond [(ieq op (S32 1))
            (iadd n1 n2)]
           [(ieq op (S32 2))
            (isub n1 n2)]
           [(ieq op (S32 3))
            (imul n1 n2)]
           [(ieq op (S32 4))
            (isdiv n1 n2)]
           [else (error "invalid op\n")])))

(define calc (Prog (include-fun "main" main)))

(module+ test
  (require chk)
  ;; XXX Write some helper routine for compilation so that users don't have to
  ;; write their own error-handling code every time.
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
  ;; XXX Write some helper routine for users to call executables without
  ;; having to use `system` directly.
  (define (go . args)
    (apply system*/exit-code bin-path args))
  (chk*
   (chk (go "2" "1" "3") 5)
   (chk (go "3" "2" "1") 2)
   (chk (go "2" "3" "4") 8)
   (chk (go "6" "4" "2") 3))
  (delete-file c-path)
  (delete-file bin-path)
  )