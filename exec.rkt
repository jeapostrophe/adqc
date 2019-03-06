#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/base
         racket/list
         racket/match
         racket/require
         (subtract-in "ast.rkt" "type.rkt")
         "compile.rkt"
         "stx.rkt"
         "type.rkt")

(define char* (ExtT (ExternSrc '() '()) "char*"))
(define stdlib-h (ExternSrc '() '("stdlib.h")))

(define (wrap-main main)
  (define args (Fun-args main))
  (define nargs (length args))
  (define tys (map Arg-ty args))
  (define fns
    (for/list ([ty (in-list tys)])
        (define name (match ty
                       [(FloT 64) "atof"]
                       [(IntT #t 32) "atoi"]
                       [(IntT #t 64) "atol"]))
        (ExtFun stdlib-h (list (Arg 'str char* 'read-only)) ty name)))
  (define arg-xs (map gensym (make-list nargs 'arg)))
  (define ret-x (gensym 'ret))
  (define argc (gensym 'argc))
  (define argv (gensym 'argv))
  (define ret-lab (gensym 'ret-lab))
  (define user-call
    (Call ret-x (T S32) main
          (for/list ([x (in-list arg-xs)] [ty (in-list tys)])
            (Var x ty))
          (Jump ret-lab))) ;; XXX return?
  (define argv-ty (ArrT (add1 nargs) char*))
  (IntFun (list (Arg argc (T S32) 'read-only)
                (Arg argv argv-ty 'read-only))
          ret-x
          (T S32)
          ret-lab
          (for/fold ([body user-call])
                    ([ty tys] [fn fns] [x arg-xs] [i (in-naturals 1)])
            (Call x ty fn (list (Select (Var argv argv-ty) (E (S32 i)))) body)))
  #;
  (F ([argc : S32] [argv : (array (add1 nargs) #,char*)]) : S32
     (assert: #:dyn #:msg (format "exactly ~a arguments supplied" nargs)
              (ieq argc (S32 nargs)))
     #,(for/fold ([body user-call])
                 ([ty tys] [fn fns] [x arg-xs] [i (in-naturals 1)])
         (Call x ty fn (list (E (argv @ (S32 i)))) body))))

(define (make-exe prog c-path out-path)
  (define name->fun (Program-name->fun prog))
  (define main (hash-ref name->fun "main"))
  ;; XXX name->fun is represented as a mutable hash-table... is it okay for
  ;; this function to mutate that table in-place?
  (hash-set! name->fun "main" (wrap-main main))
  (compile-exe prog c-path out-path))

(provide
 (contract-out
  [make-exe (-> Program? path? path? boolean?)]))
