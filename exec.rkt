#lang racket/base
(require racket/contract/base
         racket/list
         racket/match
         racket/require
         racket/runtime-path
         (subtract-in "ast.rkt" "type.rkt")
         "compile.rkt"
         "stx.rkt"
         "type.rkt")

(define char* (ExtT (ExternSrc '() '()) "char*"))
(define stdlib-h (ExternSrc '() '("stdlib.h")))

(define-runtime-path util-path "util.h")
(define util-h (ExternSrc '() (list (path->string util-path))))

(define (ty->ExtFun ty)
  (match ty
    [(FloT 64)
     (ExtFun stdlib-h (list (Arg 'str char* 'read-only)) ty "atof")]
    [(IntT #t 8)
     (ExtFun util-h (list (Arg 'str char* 'read-only)) ty "cstr_first_char")]
    [(IntT #t 32)
     (ExtFun stdlib-h (list (Arg 'str char* 'read-only)) ty "atoi")]
    [(IntT #t 64)
     (ExtFun stdlib-h (list (Arg 'str char* 'read-only)) ty "atol")]))

(define (wrap-main main)
  (define args (Fun-args main))
  (define nargs (length args))
  (define tys (map Arg-ty args))
  (define fns (map ty->ExtFun tys))
  (define arg-xs (map gensym (make-list nargs 'arg)))
  (F ([argc : S32] [argv : (array (add1 nargs) #,char*)]) : S32
     (assert! #:dyn #:msg (format "exactly ~a arguments supplied" nargs)
              (ieq argc (S32 (add1 nargs))))
     #,(let ([user-call
              (S (let ([x := main <- #,@(for/list ([x (in-list arg-xs)]
                                                   [ty (in-list tys)])
                                          (Var x ty))])
                   (return x)))])
         (for/fold ([body user-call])
                   ([ty (in-list tys)]
                    [fn (in-list fns)]
                    [x (in-list arg-xs)]
                    [i (in-naturals 1)])
           (Call x ty fn (list (E (argv @ (S32 i)))) body)))))

(define (make-exe prog c-path out-path)
  (define n->f (hash-copy (Program-name->fun prog)))
  (hash-set! n->f "main" (wrap-main (hash-ref n->f "main")))
  (compile-exe (struct-copy Program prog [name->fun n->f]) c-path out-path))

(provide
 (contract-out
  [make-exe (-> Program? path? path? boolean?)]))
