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

(struct linked-program (lib type-map src-path ty->tag tag->ty) #:transparent)
(struct typed-pointer (ty ptr) #:transparent)

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

(define (ty->ctype ty->tag tag->ty ty)
  (match ty
    [(or (? IntT?) (? FloT?))
     (Int/Flo->ctype ty)]
    [(or (? ArrT?) (? RecT?) (? UniT?))
     (define (new-type!)
       (define tag (gensym 'tag))
       (hash-set! ty->tag ty tag)
       (hash-set! tag->ty tag ty)
       tag)
     (_cpointer (hash-ref ty->tag ty new-type!))]))

(define (Arg->ctype ty->tag tag->ty arg)
  (match-define (Arg _ ty mode) arg)
  (match ty
    [(or (? IntT?) (? FloT?))
     (if (eq? mode 'ref)
         (_cpointer (hash-ref ty->tag ty))
         (Int/Flo->ctype ty))]
    [_ (ty->ctype ty->tag tag->ty ty)]))

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

(define (ty->read-ctype ty->tag tag->ty ty)
  (match ty
    [(or (? IntT?) (? FloT?))
     (Int/Flo->ctype ty)]
    [(ArrT dim ety)
     (_array/vector (ty->ctype ty->tag tag->ty ety) dim)]
    [(RecT f->ty _ c-order)
     (apply _list-struct (for/list ([f (in-list c-order)])
                           (ty->ctype ty->tag tag->ty (hash-ref f->ty f))))]
    ;; XXX Read for unions?
    ))

(define (default-tag-tables)
  (define-simple-macro (make-tag-tables (~seq k v) ...)
    (values (make-hash (list (cons k v) ...))
            (make-hasheq (list (cons v k) ...))))
  (make-tag-tables (T S8) 'sint8 (T S16) 'sint16 (T S32) 'sint32 (T S64) 'sint64
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
  (define-values (ty->tag tag->ty) (default-tag-tables))
  (define type-map
    (for/hash ([(name fun) (in-hash name->fun)])
      (match-define (IntFun args _ ret-ty _ _) (unpack-MetaFun fun))
      (define c-args (for/list ([a (in-list args)])
                       (Arg->ctype ty->tag tag->ty a)))
      (define c-ret (ty->ctype ty->tag tag->ty ret-ty))
      (values name (_cprocedure c-args c-ret))))
  (linked-program lib type-map c-path ty->tag tag->ty))

(define (linked-program-run lp n args)
  (match-define (linked-program lib type-map _ _ _) lp)
  (define fun (get-ffi-obj n lib (hash-ref type-map n)))
  (define args* (for/list ([a (in-list args)])
                  (cond [(typed-pointer? a)
                         (typed-pointer-ptr a)]
                        [else a])))
  ;; XXX If return type is cpointer, marshal into typed-pointer.
  (apply fun args*))

(define (linked-program-alloc lp ty)
  (define p (malloc (ty->alloc-ctype ty)))
  (cpointer-push-tag! p (hash-ref (linked-program-ty->tag lp) ty))
  (typed-pointer ty p))

(define (linked-program-read lp maybe-tp)
  (match maybe-tp
    [(typed-pointer ty ptr)
     (define ty->tag (linked-program-ty->tag lp))
     (define tag->ty (linked-program-tag->ty lp))
     (define r (ptr-ref ptr (ty->read-ctype ty->tag tag->ty ty)))
     (match ty
       [(ArrT dim ety)
        ;; XXX Maybe use _array instead of _array/vector to avoid extra copy.
        (for/vector #:length dim ([e (in-vector r)])
          (cond [(or (ArrT? ety) (RecT? ety) (UniT? ety))
                 (typed-pointer (ty->read-ctype ty->tag tag->ty ety) e)]
                [else e]))]
       [(RecT f->ty _ c-order)
        ;; XXX Maybe use make-cstruct-type instead of _list-struct to avoid extra copy.
        (for/list ([e (in-list r)]
                   [f (in-list c-order)])
          (define ety (hash-ref f->ty f))
          (cond [(or (ArrT? ety) (RecT? ety) (UniT? ety)) 
                 (typed-pointer (ty->read-ctype ty->tag tag->ty ety) e)]
                [else e]))]
       ;; XXX UniT?
       [_ r])]
    [_ maybe-tp]))

(provide
 (contract-out
  [struct linked-program ([lib ffi-lib?]
                          [type-map (hash/c c-identifier-string? ctype?)]
                          [src-path path?]
                          [ty->tag (hash/c Type? symbol?)]
                          [tag->ty (hash/c symbol? Type?)])]
  [struct typed-pointer ([ty Type?] [ptr cpointer?])]
  [link-program (-> Program? linked-program?)]
  [linked-program-run (-> linked-program? c-identifier-string? list? any/c)]
  [linked-program-alloc (-> linked-program? Type? typed-pointer?)]
  [linked-program-read (-> linked-program? any/c any/c)]))
