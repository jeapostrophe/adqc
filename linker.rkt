#lang racket/base
(require racket/contract/base
         racket/file
         racket/match
         (except-in ffi/unsafe ->)
         "ast.rkt"
         "compile.rkt")

(struct linked-program (lib type-map src-path) #:transparent)

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
  (define c-path (make-temporary-file "adqc~a.c"))
  (define bin-path (make-temporary-file "adqc~a"))
  (unless (compile-binary* p c-path bin-path #:shared? #t)
    (error 'link-program "call to compile-binary* failed (see stderr)"))
  (define lib (ffi-lib bin-path))
  (match-define (Program _ _ name->fun) p)
  (define type-map
    (for/hash ([(name fun) (in-hash name->fun)])
      (match-define (IntFun args _ ret-ty _ _) (unpack-MetaFun fun))
      (define c-args (map ty->ctype (map Arg-ty args)))
      (define c-ret (ty->ctype ret-ty))
      (values name (_cprocedure c-args c-ret))))
  (linked-program lib type-map c-path))

(define (run-linked-program lp n args)
  (match-define (linked-program lib type-map _) lp)
  (define fun (get-ffi-obj n lib (hash-ref type-map n)))
  (apply fun args))

(provide
 (contract-out
  [struct linked-program ([lib ffi-lib?]
                          [type-map (hash/c c-identifier-string? ctype?)]
                          [src-path path?])]
  [link-program (-> Program? linked-program?)]
  [run-linked-program (-> linked-program? c-identifier-string? list? any/c)]))
