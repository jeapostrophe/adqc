#lang racket/base
(require racket/contract/base
         racket/file
         racket/match
         syntax/parse/define
         (except-in ffi/unsafe ->)
         "ast.rkt"
         "compile.rkt"
         "stx.rkt")

(struct linked-program (lib type-map ty->tag tag->ty ret-tys) #:transparent)
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
     (_array (ty->ctype ty->tag tag->ty ety) dim)]
    [(RecT f->ty _ c-order)
     (apply _list-struct (for/list ([f (in-list c-order)])
                           (ty->ctype ty->tag tag->ty (hash-ref f->ty f))))]
    ;; XXX Read for unions?
    ))

(define-simple-macro (make-tag-tables (~seq k v) ...)
  (values (make-hash (list (cons k v) ...))
          (make-hasheq (list (cons v k) ...))))

(define (default-tag-tables)
  (make-tag-tables (T S8) 'sint8 (T S16) 'sint16 (T S32) 'sint32 (T S64) 'sint64
                   (T U8) 'uint8 (T U16) 'uint16 (T U32) 'uint32 (T U64) 'uint64
                   (T F32) 'float (T F64) 'double))
       
(define (link-program p [given-c-path #f])
  (define c-path (or given-c-path (make-temporary-file "adqc~a.c")))
  (define bin-path (make-temporary-file "adqc~a"))
  (unless (compile-library p c-path bin-path)
    (newline (current-error-port))
    (define in (open-input-file c-path))
    (for ([ch (in-port read-char in)])
      (display ch (current-error-port)))
    (close-input-port in)
    (unless given-c-path (delete-file c-path))
    (error 'link-program "call to compile-library failed (see stderr)"))
  (unless given-c-path (delete-file c-path))
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
  (define ret-tys
    (for/hash ([(name fun) (in-hash name->fun)])
      (values name (IntFun-ret-ty (unpack-MetaFun fun)))))
  (linked-program lib type-map ty->tag tag->ty ret-tys))

(define (linked-program-run lp n args)
  (match-define (linked-program lib type-map _ _ ret-tys) lp)
  (define fun (get-ffi-obj n lib (hash-ref type-map n)))
  (define r (apply fun (for/list ([a (in-list args)])
                         (cond [(typed-pointer? a)
                                (typed-pointer-ptr a)]
                               [else a]))))
  (cond [(cpointer? r)
         (typed-pointer (hash-ref ret-tys n) r)]
        [else r]))

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
        (for/vector #:length dim ([e (in-array r)])
          (cond [(or (ArrT? ety) (RecT? ety) (UniT? ety))
                 (typed-pointer ety e)]
                [else e]))]
       [(RecT f->ty _ c-order)
        ;; XXX Maybe use make-cstruct-type instead of _list-struct to avoid extra copy.
        (for/list ([e (in-list r)] [f (in-list c-order)])
          (define ety (hash-ref f->ty f))
          (cond [(or (ArrT? ety) (RecT? ety) (UniT? ety)) 
                 (typed-pointer ety e)]
                [else e]))]
       ;; XXX UniT?
       [_ r])]
    [_ maybe-tp]))

(define (linked-program-write lp tp val)
  (match-define (typed-pointer ty ptr) tp)
  (define ty->tag (linked-program-ty->tag lp))
  (define tag->ty (linked-program-tag->ty lp))
  (match ty
    [(ArrT dim ety)
     (define a (ptr-ref ptr (ty->read-ctype ty->tag tag->ty ty)))
     (for ([e (in-vector val)] [i (in-naturals)])
       (array-set! a i (cond [(or (ArrT? ety) (RecT? ety) (UniT? ety))
                              (typed-pointer-ptr e)]
                             [else e])))]
    [(RecT f->ty _ c-order)
     ;; XXX Maybe use make-cstruct-type instead of _list-struct to avoid extra copy.
     (define val* (for/list ([e (in-list val)] [f (in-list c-order)])
                    (define ety (hash-ref f->ty f))
                    (cond [(or (ArrT? ety) (RecT? ety) (UniT? ety))
                           (typed-pointer-ptr e)]
                          [else e])))
     (ptr-set! ptr (ty->read-ctype ty->tag tag->ty ty) val*)]
    [_ (ptr-set! ptr (ty->read-ctype ty->tag tag->ty ty) val)]))

(provide
 (contract-out
  [struct linked-program ([lib ffi-lib?]
                          [type-map (hash/c c-identifier-string? ctype?)]
                          [ty->tag (hash/c Type? symbol?)]
                          [tag->ty (hash/c symbol? Type?)]
                          [ret-tys (hash/c c-identifier-string? Type?)])]
  [struct typed-pointer ([ty Type?] [ptr cpointer?])]
  [link-program (->* [Program?] [(or/c path? #f)] linked-program?)]
  [linked-program-run (-> linked-program? c-identifier-string? list? any/c)]
  [linked-program-alloc (-> linked-program? Type? typed-pointer?)]
  [linked-program-read (-> linked-program? any/c any/c)]
  [linked-program-write (-> linked-program? typed-pointer? any/c void?)]))
