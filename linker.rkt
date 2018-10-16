#lang racket/base
(require racket/contract/base
         racket/file
         racket/match
         racket/port
         syntax/parse/define
         (except-in ffi/unsafe ->)
         "ast.rkt"
         "compile.rkt"
         "stx.rkt")

(struct linked-program (lib type-map src-path alloc) #:transparent)

(define (Int/Flo->ctype ty)
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

(define (ty->ctype tags ty)
  (match ty
    [(or (? IntT?) (? FloT?))
     (Int/Flo->ctype ty)]
    [(or (? ArrT?) (? RecT?) (? UniT?))
     (define (new-type!)
       (define tag (gensym 'tag))
       (hash-set! tags ty tag)
       tag)
     (_cpointer (hash-ref tags ty new-type!))]))

(define (Arg->ctype tags arg)
  (match-define (Arg _ ty mode) arg)
  (match ty
    [(or (? IntT?) (? FloT?))
     (if (eq? mode 'ref)
         (_cpointer (hash-ref tags ty))
         (Int/Flo->ctype ty))]
    [_ (ty->ctype tags ty)]))

(define (ty->untagged-ctype ty)
  (match ty
    [(or (? IntT?) (? FloT?)) (Int/Flo->ctype ty)]
    [(or (? ArrT?) (? RecT?) (? UniT?)) _pointer]))

(define (ty->alloc-ctype ty)
  (match ty
    [(or (? IntT?) (? FloT?))
     (Int/Flo->ctype ty)]
    [(ArrT dim ety)
     (make-array-type (ty->untagged-ctype ty) dim)]
    [(RecT f->ty _ c-order)
     (make-cstruct-type (for/list ([f (in-list c-order)])
                          (ty->untagged-ctype (hash-ref f->ty f))))]
    [(UniT m->ty _)
     (apply make-union-type
            (map ty->untagged-ctype (hash-values m->ty)))]))

(define-simple-macro (mutable-hash (~seq k v) ...)
  (make-hash (list (cons k v) ...)))

(define (default-tags)
  (mutable-hash (T S8) 'sint8 (T S16) 'sint16 (T S32) 'sint32 (T S64) 'sint64
                (T U8) 'uint8 (T U16) 'uint16 (T U32) 'uint32 (T U64) 'uint64
                (T F32) 'float (T F64) 'double))
       
(define (link-program p)
  (define c-path (make-temporary-file "adqc~a.c"))
  (define bin-path (make-temporary-file "adqc~a"))
  (unless (compile-library p c-path bin-path)
    (newline (current-error-port))
    (display (port->string (open-input-file c-path)) (current-error-port))
    (error 'link-program "call to compile-library failed (see stderr)"))
  (define lib (ffi-lib bin-path))
  (define name->fun (Program-name->fun p))
  (define tags (default-tags))
  (define type-map
    (for/hash ([(name fun) (in-hash name->fun)])
      (match-define (IntFun args _ ret-ty _ _) (unpack-MetaFun fun))
      (define c-args (for/list ([a (in-list args)])
                       (Arg->ctype tags a)))
      (define c-ret (ty->ctype tags ret-ty))
      (values name (_cprocedure c-args c-ret))))
  (define (alloc ty)
    (define p (malloc (ty->alloc-ctype ty)))
    (cpointer-push-tag! p (hash-ref tags ty))
    p)
  (linked-program lib type-map c-path alloc))

(define (run-linked-program lp n args)
  (match-define (linked-program lib type-map _ _) lp)
  (define fun (get-ffi-obj n lib (hash-ref type-map n)))
  (apply fun args))

(provide
 (contract-out
  [struct linked-program ([lib ffi-lib?]
                          [type-map (hash/c c-identifier-string? ctype?)]
                          [src-path path?]
                          [alloc (-> Type? cpointer?)])]
  [link-program (-> Program? linked-program?)]
  [run-linked-program (-> linked-program? c-identifier-string? list? any/c)]))
