#lang racket/base
(require racket/contract/base
         racket/file
         racket/match
         (rename-in ffi/unsafe [-> ffi:->])
         "ast.rkt"
         "compile.rkt")

(struct linked-program (ffi-obj type-map) #:transparent)

(define (ty->ctype ty)
  (match ty
    [(IntT signed? bits)
     (if signed?
         (match bits
           [8  _sint8]
           [16 _sint16]
           [32 _sint32]
           [64 _sint64])
         (match bits
           [8  _uint8]
           [16 _uint16]
           [32 _uint32]
           [64 _uint64]))]
    [(FloT bits)
     (match bits
       [32 _float]
       [64 _double])]))

(define (link-program p)
  (define bin-path (make-temporary-file "adqc_bin_~a"))
  (unless (compile-binary p bin-path)
    (error 'link-program "call to compile-binary failed (see stderr)")) 
  (printf "wrote binary to ~a\n" bin-path)
  (define ffi-obj (ffi-lib bin-path))
  (match-define (Program _  _ name->fun) p)
  (define type-map
    (for/hasheq ([(name fun) (in-hash name->fun)])
      (match-define (IntFun args _ ret-ty _ _) fun)
      (define c-args (map ty->ctype (map Arg-ty args)))
      (define c-ret (ty->ctype ret-ty))
      (values name (_cprocedure c-args c-ret))))
  (linked-program ffi-obj type-map))

(define (run-linked-program lp n args)
  (match-define (linked-program ffi-obj type-map) lp)
  (define fun (get-ffi-obj n ffi-obj (hash-ref type-map n)))
  (apply fun args))

(provide
 (contract-out
  [link-program (-> Program? linked-program?)]
  [run-linked-program (-> linked-program? symbol? list? any/c)]))