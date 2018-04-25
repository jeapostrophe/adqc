#lang racket/base
(require racket/contract/base
         racket/contract/region
         racket/match
         racket/set
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/parse/experimental/template))
(module+ test
  (require chk
           racket/pretty))

;;; Library
(begin-for-syntax
  (define (format-ids base-stx fmt base-name vars)
    (for/list ([v (in-list (syntax->list vars))])
      (format-id base-stx fmt base-name v)))
  (define symbol->keyword (compose1 string->keyword symbol->string)))
(define-syntax (define-interface stx)
  (syntax-parse stx
    [(_ x:id ([f:id f/ctc:expr] ...))
     #:with *x (generate-temporary #'x)
     #:with x? (format-id #'x "~a?" #'x)
     #:with *x? (format-id #'*x "~a?" #'*x)
     #:with (x-f ...) (format-ids #'x "~a-~a" #'x #'(f ...))
     #:with (*x-f ...) (format-ids #'*x "~a-~a" #'*x #'(f ...))
     #:with (kw-f ...) (map (compose1 symbol->keyword syntax-e)
                            (syntax->list #'(f ...)))
     (template/loc stx
                   (begin (struct *x (f ...) #:transparent)
                          (define x? *x?)
                          (define/contract (x (?@ kw-f f) ...)
                            (->* ((?@ kw-f f/ctc) ...) x?)
                            (*x f ...))
                          (define/contract x-f (-> x? any) *x-f) ...
                          (provide x? x-f ...)))]))

;;; Types

(define bitwidths '(1 8 16 32 64))
(define IntegerBitWidth?
  (apply or/c bitwidths))
(define FloatBitWidth?
  (or/c 32 64))
(define Dimension?
  exact-nonnegative-integer?)
(define Index?
  exact-nonnegative-integer?)
(define Field? symbol?)

(define-interface Type
  ([size exact-nonnegative-integer?]
   [ctc contract?]
   [fmt any/c]
   [Int (or/c #f (cons/c boolean? IntegerBitWidth?))]
   [Flo (or/c #f FloatBitWidth?)]))
;; XXX fmt should have a variable to add it to

(define (Type-union x y)
  (or (and (eq? x y) x)
      (error 'type-union "Cannot union ~a & ~a"
             x y)))

(define Void
  (Type #:size 0
        #:ctc none/c
        #:fmt "void"
        #:Int #f
        #:Flo #f))

(define PtrT
  (Type #:size 64
        #:ctc none/c
        #:fmt "(void *)"
        #:Int #f
        #:Flo #f))

(define/contract (IntT signed? w)
  (-> boolean? IntegerBitWidth? Type?)
  (define W (expt 2 w))
  (Type #:size w
        #:ctc
        (if (= w 1) (or/c 0 1 boolean?)
            (integer-in (if signed? (* -1 W) 0)
                        (sub1 W)))
        #:fmt
        (if (= w 1) "bool"
            (format "~aint~a_t" (if signed? "" "u") w))

        #:Int (cons signed? w)
        #:Flo #f))

(define-syntax-rule (define-Integers [id signed? w] ...)
  (begin (define id (IntT signed? w)) ...))
(define-Integers
  [ U1 #f  1]
  [ U8 #f  8] [ S8 #t  8]
  [U16 #f 16] [S16 #t 16]
  [U32 #f 32] [S32 #t 32]
  [U64 #f 64] [S64 #t 64])
(define Bool U1)

(define/contract (FloT w)
  (-> FloatBitWidth? Type?)
  (Type #:size w
        #:ctc (match w [32 single-flonum?] [64 double-flonum?])
        #:fmt (match w [32 "float"] [64 "double"])
        #:Int #f
        #:Flo w))

(define F32 (FloT 32))
(define F64 (FloT 64))

;; NOTE This means that functions are not polymorphic in the array
;; dimension, so we have to use meta-programming to generate many
;; versions of the same function specialized for different sizes, or
;; we have to slice arrays.
(define/contract (ArrT dim elem)
  (-> Dimension? Type? Type?)
  (Type #:size (* dim (Type-size elem))
        #:ctc (and/c vector? (λ (v) (= (vector-length v) dim))
                     (vectorof (Type-ctc elem)))
        #:fmt (format "~a[~a]" (Type-fmt elem) dim)
        #:Int #f
        #:Flo #f))

(define (hasheq-filter-tv h)
  (for/hasheq ([(k v) (in-hash h)]
               #:when v)
    (values k v)))

(define/contract (RecT -field->ty)
  (-> (hash/c #:immutable #t Field? (or/c #f Type?)) (or/c #f Type?))
  (define field->ty (hasheq-filter-tv -field->ty))
  (and (not (hash-empty? field->ty))
       (Type #:size (for/sum ([t (in-hash-values field->ty)])
                      (Type-size t))
             #:ctc (and/c hash? (λ (h) (= (hash-count h) (hash-count field->ty)))
                          (hash/dc
                           [f (apply or/c (hash-keys field->ty))]
                           [v (f) (Type-ctc (hash-ref field->ty f))]
                           #:immutable #t))
             #:fmt (list "struct {"
                         (for/list ([(k v) (in-hash field->ty)])
                           (list (Type-fmt v) " " k ";"))
                         "}")
             #:Int #f
             #:Flo #f)))

(define/contract (UniT -mode->ty)
  (-> (hash/c #:immutable #t Field? (or/c #f Type?)) (or/c #f Type?))
  (define mode->ty (hasheq-filter-tv -mode->ty))
  (and (not (hash-empty? mode->ty))
       (Type #:size (for/fold ([m 0]) ([t (in-hash-values mode->ty)])
                      (max m (Type-size t)))
             #:ctc (and/c hash? (λ (h) (= (hash-count h) (hash-count mode->ty)))
                          (hash/dc
                           [f (apply or/c (hash-keys mode->ty))]
                           [v (f) (Type-ctc (hash-ref mode->ty f))]
                           #:immutable #t))
             #:fmt (list "union {"
                         (for/list ([(k v) (in-hash mode->ty)])
                           (list (Type-fmt v) " " k ";"))
                         "}")
             #:Int #f
             #:Flo #f)))

(provide PtrT
         U1 Bool U8 S8 U16 S16 U32 S32 U64 S64
         F32 F64
         ArrT RecT UniT)
(module+ test
  (chk (Type-size (UniT (hash 'x (RecT (hasheq 'a U32 'b S64))
                              'y (UniT (hasheq 'c S8 'd U16)))))
       96))

;;; Expression

;;;; Interval Arithmetic
(struct ival (l e h) #:transparent)
(define (iunit x) (ival x x x))
(define (ival+ x y)
  (match-define (ival lx ex hx) x)
  (match-define (ival ly ey hy) y)
  (ival (+ lx ly) (+ ex ey) (+ hx hy)))
(define (ivalU P x y)
  (match-define (ival lx ex hx) x)
  (match-define (ival ly ey hy) y)
  (ival (min lx ly)
        (+ (* P ex) (* (- 1 P) ey))
        (max hx hy)))
;;;; / Interval Arithmetic

(define Variable? symbol?)
(define mt-set (seteq))
(define mt-map (hasheq))
(define (map-union x y)
  (for/fold ([x x]) ([(k v) (in-hash y)])
    (hash-update x k
                 (λ (old)
                   (or (and (not old) v)
                       (Type-union old v)))
                 #f)))

(define-interface Expr
  ([type Type?]
   [unsafe? boolean?]
   [read-vs (set/c Variable?)]
   [write-vs (set/c Variable?)]
   [v->ty (hash/c #:immutable #t Variable? Type?)]
   [mem (or/c #f Type?)]
   [rtime ival?]))
;; XXX format (should take a mapping from variable to path)
;; XXX weakest-precondition
;; XXX strongest-postcondition
;; XXX lval

(define (Exprs-unsafe? . es)
  (for/or ([e (in-list es)])
    (Expr-unsafe? e)))

(define ((Exprs-*-vs set-union Expr-read-vs) e . es)
  (for/fold ([s (Expr-read-vs e)]) ([e (in-list es)])
    (set-union s (Expr-read-vs e))))

(define Exprs-read-vs (Exprs-*-vs set-union Expr-read-vs))
(define Exprs-write-vs (Exprs-*-vs set-union Expr-write-vs))
(define Exprs-v->ty (Exprs-*-vs map-union Expr-v->ty))

(define/contract (VarR ty x)
  (-> Type? Variable? Expr?)
  (Expr #:type ty
        #:unsafe? #f
        #:read-vs (seteq x)
        #:write-vs mt-set
        #:v->ty (hasheq x ty)
        #:mem #f
        #:rtime (iunit 1)))

(define/contract (Val ty v)
  (->i ([ty Type?] [v (ty) (Type-ctc ty)]) [r Expr?])
  (Expr #:type ty
        #:unsafe? #f
        #:read-vs mt-set
        #:write-vs mt-set
        #:v->ty mt-map
        #:mem #f
        #:rtime (iunit 1)))

(define/contract (Let v #:read-only? [read-only? boolean?]
                      e be)
  (->* (Variable? Expr? Expr?) (#:read-only? boolean?) Expr?)
  (define bs-writes (Expr-write-vs be))
  (when (and read-only?
             (set-member? bs-writes v))
    (error 'Let "Cannot mutate read-only variable: ~a" v))
  (Expr #:type (Expr-type be)
        #:unsafe? (Exprs-unsafe? e be)
        #:read-vs (set-union (Expr-read-vs e)
                             (set-remove (Expr-read-vs be) v))
        #:write-vs (set-union (Expr-write-vs e)
                              (set-remove bs-writes v))
        #:v->ty (map-union (Expr-v->ty e)
                           (hash-remove (Expr-v->ty be) v))
        #:mem
        ;; NOTE e might be substitutable -- which we could track by
        ;; changing read/write to multisets. However, doing the
        ;; substitution would require recomputing `be`, which is not
        ;; possible, unless we add that to the interface, which we
        ;; don't want to.
        (RecT (hasheq 'var (Expr-type e)
                      'body (UniT (hasheq 'e (Expr-mem e)
                                          'be (Expr-mem be)))))
        #:rtime (ival+ (Expr-rtime e) (ival+ (iunit 1) (Expr-rtime be)))))

(define BinOperator?
  (or/c
   ;; IBinOp
   'iadd 'isub 'imul 'iudiv 'isdiv 'iurem 'isrem
   'ishl 'ilshr 'iashr 'iand 'ior 'ixor
   ;; FBinOp
   'fadd 'fsub 'fmul 'fdiv 'frem
   ;; ICmp
   'ieq 'ine 'iugt 'iuge 'iult 'iule 'isgt 'isge 'islt 'isle
   ;; FCmp
   'ffalse 'foeq 'fogt 'foge 'folt 'fole 'fone 'ford
   'ftrue 'fueq 'fuge 'fuge 'fult 'fule 'fune 'funo))

(define (Type-cmp= Type-cmp x y)
  (define cx (Type-cmp x))
  (define cy (Type-cmp y))
  (and cx cy (equal? cx cy)))

(define/contract (Bin op lhs rhs)
  (-> BinOperator? Expr? Expr? Expr?)
  (define lhs-ty (Expr-type lhs))
  (define rhs-ty (Expr-type rhs))
  (define-values (Type-cmp result-ty)
    (match op
      [(or 'iadd 'isub 'imul 'iudiv 'isdiv 'iurem 'isrem
           'ishl 'ilshr 'iashr 'iand 'ior 'ixor)
       (values Type-Int lhs-ty)]
      [(or 'fadd 'fsub 'fmul 'fdiv 'frem)
       (values Type-Flo lhs-ty)]
      [(or 'ieq 'ine 'iugt 'iuge 'iult 'iule 'isgt 'isge 'islt 'isle)
       (values Type-Int Bool)]
      [(or 'ffalse 'foeq 'fogt 'foge 'folt 'fole 'fone 'ford
           'ftrue 'fueq 'fuge 'fuge 'fult 'fule 'fune 'funo)
       (values Type-Flo Bool)]))
  (unless (Type-cmp= Type-cmp lhs-ty rhs-ty)
    (error 'Bin "Cannot perform ~a on ~a and ~a, types are wrong ~a and ~a"
           op lhs rhs lhs-ty rhs-ty))
  (Expr #:type result-ty
        #:unsafe? (Exprs-unsafe? lhs rhs)
        #:read-vs (Exprs-read-vs lhs rhs)
        #:write-vs (Exprs-write-vs lhs rhs)
        #:v->ty (Exprs-v->ty lhs rhs)
        #:mem (UniT (hasheq 'lhs (Expr-mem lhs) 'rhs (Expr-mem rhs)))
        #:rtime (ival+ (Expr-rtime lhs) (ival+ (Expr-rtime rhs) (iunit 1)))))

;; XXX ArrR
;; XXX RecR
;; XXX UniR

;; XXX Unsafe (i.e. call C function)
;; XXX Cast

;; XXX Assign

(define/contract (Seq f s)
  (-> Expr? Expr? Expr?)
  (Expr #:type (Expr-type s)
        #:unsafe? (Exprs-unsafe? f s)
        #:read-vs (Exprs-read-vs f s)
        #:write-vs (Exprs-write-vs f s)
        #:v->ty (Exprs-v->ty f s)
        #:mem (UniT (hasheq 'f (Expr-mem f) 's (Expr-mem s)))
        #:rtime (ival+ (Expr-rtime f) (Expr-rtime s))))

(define/contract (If c #:P [P 0.5] t f)
  (->* (Expr? Expr? Expr?) (#:P real?) Expr?)
  (Expr #:type (Type-union (Expr-type t) (Expr-type f))
        #:unsafe? (Exprs-unsafe? c t f)
        #:read-vs (Exprs-read-vs c t f)
        #:write-vs (Exprs-write-vs c t f)
        #:v->ty (Exprs-v->ty c t f)
        #:mem (RecT (hasheq 'c (Expr-mem c)
                            'k (UniT (hasheq 't (Expr-mem t)
                                             'f (Expr-mem f)))))
        #:rtime (ival+ (Expr-rtime c) (ivalU P (Expr-rtime t) (Expr-rtime f)))))

(define/contract Skip Expr?
  (Expr #:type Void
        #:unsafe? #f
        #:read-vs mt-set
        #:write-vs mt-set
        #:v->ty mt-map
        #:mem #f
        #:rtime (iunit 0)))

(define/contract Abort Expr?
  (Expr #:type Void
        #:unsafe? #f
        #:read-vs mt-set
        #:write-vs mt-set
        #:v->ty mt-map
        #:mem #f
        #:rtime (iunit 0)))

(define/contract (Assert ?) (-> Expr? Expr?)
  (If ? Skip Abort))

;; XXX Loop
;; XXX Break
;; XXX Continue

;; XXX Specification (i.e. function call spot, rather than fully-inlining everything)

(module+ test
  (pretty-print
   (If (Val Bool #t)
       (Let 'x (Val U32 6)
            (Let 'y (Val F64 3.14)
                 (Bin 'iadd (VarR U32 'x) (Val U32 8))))
       (Let 'z (Val U32 16)
            (Bin 'iadd (VarR U32 'z) (Val U32 9))))))

;; XXX

#;(
   (define-type (Expr [ty Type?] [v->ty (hash/c Variable? Type?)])

     (*RecR [r Expr?] [f Field?])
     ;; NOTE In compiler/verifier, assert that i is within bounds. This
     ;; means we need to know a's type during the emit/verify process.
     (*ArrR [a Expr?] [i Expr?])
     ;; NOTE This is a static slice (so we can predict what the size of
     ;; the resulting array will be)... and is basically pointer
     ;; arithmetic.
     (*ArrS [a Expr?] [s Index?] [e Index?])
     (*Cast [e Expr?]))

   (define (RecR r f)
     (match (Expr-ty r)
       [(RecT f->t)
        (define f-ty (hash-ref f->t f #f))
        (and (or f-ty
                 (fail! r "has no" f "field"))
             (*RecR f-ty (Expr-v->ty r) r f))]
       [_ (fail! r "is not a record")]))
   (define (ArrR a i)
     (match (Expr-ty a)
       [(ArrT dim e-ty)
        (*ArrR e-ty (Expr-v->ty a) a i)]
       [_ (fail! a "is not an array")]))
   (define (ArrS a s e)
     (and (or (<= s e)
              (fail! s "is not less than or equal to" e))
          (match (Expr-ty a)
            [(ArrT dim e-ty)
             (and (or (<= s dim)
                      (fail! a "is" dim "large, not long enough for slice to start at" s))
                  (or (<= e dim)
                      (fail! a "is" dim "large, not long enough for slice to end at" e))
                  (*ArrS (ArrT (- e s) e-ty) (Expr-v->ty a) a s e))]
            [_ (fail! a "is not an array")])))
   (define (Cast ty e)
     (define ety (Expr-ty e))
     (and (or (NumT? ty)
              (fail! ty "must be number type"))
          (or (NumT? ety)
              (fail! e "is not a number"))
          (*Cast ty (Expr-v->ty e) e)))
   )
