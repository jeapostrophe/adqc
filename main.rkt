#lang racket/base
(require racket/contract/base
         racket/contract/region
         racket/match
         racket/set
         racket/format
         racket/pretty
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/parse/experimental/template))
(module+ test
  (require chk))

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
(define Variable? symbol?)

(define-interface Type
  ([size exact-nonnegative-integer?]
   [ctc contract?]
   [fmt-decl (-> Variable? any/c)]
   [fmt-val (-> any/c any/c)]
   [Int (or/c #f (cons/c boolean? IntegerBitWidth?))]
   [Flo (or/c #f FloatBitWidth?)]))

(define (Type-union x y)
  (or (and (or (eq? x y)
               (Type-cmp= Type-Int x y)
               (Type-cmp= Type-Flo x y))
           x)
      (error 'type-union "Cannot union ~a & ~a"
             x y)))

(define (fmt-right ty)
  (λ (v) (list* ty " " v)))
(define (fmt-assign v-id)
  (λ (v) (list v-id " = " v ";")))

(define Void
  (Type #:size 0
        #:ctc none/c
        #:fmt-decl (fmt-right "void")
        #:fmt-val (λ (v) (error 'fmt-val "No values of type Void"))
        #:Int #f
        #:Flo #f))

(define PtrT
  (Type #:size 64
        #:ctc none/c
        #:fmt-decl (fmt-right "(void *)")
        #:fmt-val (λ (v) (error 'fmt-val "No values of type Void"))
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
        #:fmt-decl
        (fmt-right
         (if (= w 1) "bool"
             (format "~aint~a_t" (if signed? "" "u") w)))
        #:fmt-val (if (= w 1)
                    (λ (x) (if x "1" "0"))
                    number->string)
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

(define (nearest-IntT signed? max-count)
  (for/or ([w (in-list bitwidths)])
    (and (< max-count (expt 2 w))
         (IntT signed? w))))

(define/contract (FloT w)
  (-> FloatBitWidth? Type?)
  (Type #:size w
        #:ctc (match w [32 single-flonum?] [64 double-flonum?])
        #:fmt-decl (fmt-right (match w [32 "float"] [64 "double"]))
        #:fmt-val number->string
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
        #:fmt-decl (λ (v) (list* ((Type-fmt-decl elem) v) "[" dim
                                 "]"))
        #:fmt-val (λ (v) (error 'fmt-val "XXX ArrT"))
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
             #:fmt-decl (λ (v)
                          (list* "struct {"
                                 (for/list ([(k v) (in-hash field->ty)])
                                   (list* ((Type-fmt-decl v) k) ";"))
                                 "} " v))
             #:fmt-val (λ (v) (error 'fmt-val "XXX RecT"))
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
             #:fmt-decl (λ (v)
                          (list* "union {"
                                 (for/list ([(k v) (in-hash mode->ty)])
                                   (list* ((Type-fmt-decl v) k) ";"))
                                 "} " v))
             #:fmt-val (λ (v) (error 'fmt-val "XXX UniT"))
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
(define (ival*k l e h x)
  (match-define (ival lx ex hx) x)
  (ival (* l lx) (* e ex) (* h hx)))
;;;; / Interval Arithmetic

;;;; Logic
(struct logic () #:prefab)
(struct logic-const logic (c) #:prefab)
(struct logic-var logic (v) #:prefab)
(struct *logic-bin logic (op lhs rhs) #:prefab)
(struct *logic-not logic (arg) #:prefab)
(struct logic-XXX logic (where) #:prefab)

(define (logic-not a)
  (match a
    [(logic-const #t) (logic-const #f)]
    [(logic-const #f) (logic-const #t)]
    [(*logic-not a) a]
    [_ (*logic-not a)]))

(define (logic-bin o l r)
  (match* (o l r)
    [((or 'eq 'ieq) x x) (logic-const #t)]

    [('or (logic-const #t) _) (logic-const #t)]
    [('or _ (logic-const #t)) (logic-const #t)]
    [('or (logic-const #f) r) r]
    [('or l (logic-const #f)) l]

    [('and (logic-const #t) r) r]
    [('and l (logic-const #t)) l]
    [('and (logic-const #f) r) (logic-const #f)]
    [('and l (logic-const #f)) (logic-const #f)]

    [(_ _ _) (*logic-bin o l r)]))

(define (logic-eq lhs rhs) (logic-bin 'eq lhs rhs))
(define (logic-or lhs rhs) (logic-bin 'or lhs rhs))
(define (logic-and lhs rhs) (logic-bin 'and lhs rhs))
(define (logic-implies lhs rhs) (logic-bin 'or (logic-not lhs) rhs))

(define (logic-subst e x c)
  (match e
    [(logic-var y)
     (if (eq? x y) c e)]
    [(*logic-bin op l r)
     (logic-bin op
                (logic-subst l x c)
                (logic-subst r x c))]
    [(*logic-not e)
     (logic-not (logic-subst e x c))]
    [(or (? logic-const?)
         (? logic-XXX?))
     e]))

(define (logic-format l)
  (define rec
    (match-lambda
      [(logic-const c) c]
      [(logic-var v) v]
      [(*logic-not l) (list 'not (rec l))]
      [(*logic-bin op l r) (list op (rec l) (rec r))]
      [(logic-XXX where) (list 'XXX where)]))
  (pretty-format (rec l)))
;;;; / Logic

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
   [lval (or/c #f Variable?)]
   [v->ty (hash/c #:immutable #t Variable? Type?)]
   [mem (or/c #f Type?)]
   [rtime ival?]
   [fmt-c (-> (hash/c Variable? Variable?) (-> any/c any/c) any/c)]
   [lval-ret (-> (hash/c Variable? Variable?) (-> any/c any/c))]
   [wp (-> (or/c #f Variable?) logic? logic?)]))

(define (Exprs-unsafe? . es)
  (for/or ([e (in-list es)])
    (Expr-unsafe? e)))

(define ((Exprs-*-vs set-union Expr-read-vs) e . es)
  (for/fold ([s (Expr-read-vs e)]) ([e (in-list es)])
    (set-union s (Expr-read-vs e))))

(define Exprs-read-vs (Exprs-*-vs set-union Expr-read-vs))
(define Exprs-write-vs (Exprs-*-vs set-union Expr-write-vs))
(define Exprs-v->ty (Exprs-*-vs map-union Expr-v->ty))

(define/contract (Var ty x)
  (-> Type? Variable? Expr?)
  (Expr #:type ty
        #:unsafe? #f
        #:read-vs (seteq x)
        #:lval x
        #:write-vs mt-set
        #:v->ty (hasheq x ty)
        #:mem #f
        #:rtime (iunit 1)

        #:fmt-c (λ (v->c ret) (ret (hash-ref v->c x)))
        #:lval-ret (λ (v->c) (λ (v) (list* (hash-ref v->c x) " = " v ";")))

        #:wp (λ (r Q) (logic-subst Q r (logic-var x)))))

(define not-lval
  (λ (v->c)
    (error 'lval-ret "Not an lval")))

(define/contract (Val ty v)
  (->i ([ty Type?] [v (ty) (Type-ctc ty)]) [r Expr?])
  (Expr #:type ty
        #:unsafe? #f
        #:read-vs mt-set
        #:lval #f
        #:lval-ret not-lval
        #:write-vs mt-set
        #:v->ty mt-map
        #:mem #f
        #:rtime (iunit 1)
        #:fmt-c (λ (v->c ret) (ret ((Type-fmt-val ty) v)))

        #:wp (λ (r Q) (logic-subst Q r (logic-const v)))))

(define True (Val Bool #t))
(define False (Val Bool #f))

(define/contract (Let v #:read-only? [read-only? #f]
                      e be)
  (->* (Variable? Expr? Expr?) (#:read-only? boolean?) Expr?)
  (define bs-writes (Expr-write-vs be))
  (define was-mutated? (set-member? bs-writes v))
  (when (and read-only? was-mutated?)
    (error 'Let "Cannot mutate read-only variable: ~a" v))
  (define et (Expr-type e))
  (define v-id (gensym 'Let_v))
  (Expr #:type (Expr-type be)
        #:unsafe? (Exprs-unsafe? e be)
        #:read-vs (set-union (Expr-read-vs e)
                             (set-remove (Expr-read-vs be) v))
        #:lval #f
        #:lval-ret not-lval
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
        (RecT (hasheq 'var et
                      'body (UniT (hasheq 'e (Expr-mem e)
                                          'be (Expr-mem be)))))
        #:rtime (ival+ (Expr-rtime e) (ival+ (iunit 1) (Expr-rtime
                                                        be)))
        #:fmt-c (λ (v->c ret)
                  (list* "{ " indent++
                         ((Type-fmt-decl et) v-id) ";" indent-nl
                         ((Expr-fmt-c e) v->c (fmt-assign v-id)) indent-nl
                         ((Expr-fmt-c be) (hash-set v->c v v-id) ret)
                         indent-- " }"))

        ;; XXX might need to use v-id and subst
        #:wp (λ (r Q) ((Expr-wp e) v ((Expr-wp be) r Q)))
        ))

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

(define bin->op
  (hasheq 'iadd "+" 'isub "-" 'imul "*" 'iudiv "/" 'isdiv "/" 'iurem "%" 'isrem "%"
          'ishl "<<" 'ilshr ">>" 'iashr ">>" 'iand "&" 'ior "|" 'ixor "^"
          'fadd "+" 'fsub "-" 'fmul "*" 'fdiv "/" 'frem "%"
          'ieq "==" 'ine "!="
          'iugt ">" 'iuge ">=" 'iult "<" 'iule "<="
          'isgt ">" 'isge ">=" 'islt "<" 'isle "<="
          'foeq "==" 'fogt ">" 'foge ">=" 'folt "<" 'fole "<="))

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
  (define lhs-id (gensym 'Bin_lhs))
  (define rhs-id (gensym 'Bin_rhs))
  (Expr #:type result-ty
        #:unsafe? (Exprs-unsafe? lhs rhs)
        #:read-vs (Exprs-read-vs lhs rhs)
        #:lval #f
        #:lval-ret not-lval
        #:write-vs (Exprs-write-vs lhs rhs)
        #:v->ty (Exprs-v->ty lhs rhs)
        #:mem (UniT (hasheq 'lhs (Expr-mem lhs) 'rhs (Expr-mem rhs)))
        #:rtime (ival+ (Expr-rtime lhs) (ival+ (Expr-rtime rhs) (iunit 1)))
        #:fmt-c (λ (v->c ret)
                  (list* "{ " indent++
                         ((Type-fmt-decl lhs-ty) lhs-id) ";" indent-nl
                         ((Expr-fmt-c lhs) v->c (fmt-assign lhs-id)) indent-nl
                         ((Type-fmt-decl rhs-ty) rhs-id) ";" indent-nl
                         ((Expr-fmt-c rhs) v->c (fmt-assign rhs-id)) indent-nl
                         (ret (list* lhs-id " " (hash-ref bin->op op) " " rhs-id))
                         indent-- " }"))

        #:wp (λ (r Q)
               ((Expr-wp lhs)
                lhs-id
                ((Expr-wp rhs)
                 rhs-id
                 (logic-subst Q r
                              (logic-bin op (logic-var lhs-id) (logic-var rhs-id))))))))

;; XXX ArrR
;; XXX RecR
;; XXX UniR

;; XXX Unsafe (i.e. call C function)
;; XXX Cast

(define/contract (Assign lhs rhs)
  (-> (and/c Expr?
             (flat-named-contract 'Expr-LVal Expr-lval))
      Expr? Expr?)
  (Expr #:type Void
        #:unsafe? (Exprs-unsafe? lhs rhs)
        #:read-vs (Exprs-read-vs lhs rhs)
        #:lval #f
        #:lval-ret not-lval
        #:write-vs (set-add (Exprs-write-vs lhs rhs) (Expr-lval lhs))
        #:v->ty (Exprs-v->ty lhs rhs)
        #:mem (UniT (hasheq 'lhs (Expr-mem lhs) 'rhs (Expr-mem rhs)))
        #:rtime (ival+ (Expr-rtime lhs) (ival+ (Expr-rtime rhs) (iunit
                                                                 1)))
        #:fmt-c (λ (v->c ret) ((Expr-fmt-c rhs) v->c ((Expr-lval-ret lhs) v->c)))

        #:wp (λ (r Q) ((Expr-wp rhs) (Expr-lval lhs) Q))))

(define/contract (Seq f s)
  (-> (and/c Expr?
             (flat-named-contract 'Expr-Void (λ (v) (= 0 (Type-size (Expr-type v))))))
      Expr? Expr?)
  (Expr #:type (Expr-type s)
        #:unsafe? (Exprs-unsafe? f s)
        #:read-vs (Exprs-read-vs f s)
        #:lval #f
        #:lval-ret not-lval
        #:write-vs (Exprs-write-vs f s)
        #:v->ty (Exprs-v->ty f s)
        #:mem (UniT (hasheq 'f (Expr-mem f) 's (Expr-mem s)))
        #:rtime (ival+ (Expr-rtime f) (Expr-rtime s))
        #:fmt-c (λ (v->c ret)
                  (list* ((Expr-fmt-c f)
                          v->c
                          (λ (v) (error 'Seq-f "Should not return")))
                         indent-nl
                         ((Expr-fmt-c s) v->c ret)))

        #:wp (λ (r Q) ((Expr-wp f) #f ((Expr-wp s) r Q)))))

(define Expr-Bool?
  (and/c Expr?
         (flat-named-contract 'Expr-Bool
                              (λ (v) (Type-cmp= Type-Int Bool (Expr-type v))))))

(define/contract (If c #:P [P 0.5] t f)
  (->* (Expr-Bool? Expr? Expr?) (#:P real?) Expr?)
  (define c-id (gensym 'If_c))
  (Expr #:type (Type-union (Expr-type t) (Expr-type f))
        #:unsafe? (Exprs-unsafe? c t f)
        #:read-vs (Exprs-read-vs c t f)
        #:lval #f
        #:lval-ret not-lval
        #:write-vs (Exprs-write-vs c t f)
        #:v->ty (Exprs-v->ty c t f)
        #:mem (RecT (hasheq 'c (Expr-mem c)
                            'k (UniT (hasheq 't (Expr-mem t)
                                             'f (Expr-mem f)))))
        #:rtime (ival+ (Expr-rtime c) (ivalU P (Expr-rtime t)
                                             (Expr-rtime f)))
        #:fmt-c (λ (v->c ret)
                  ;; XXX if c can be an expression (not a statement,
                  ;; then drop it in)
                  (list* "{ " indent++
                         ((Type-fmt-decl (Expr-type c)) c-id) ";" indent-nl
                         ((Expr-fmt-c c) v->c (fmt-assign c-id)) indent-nl
                         "if (" c-id ") {" indent++ indent-nl
                         ((Expr-fmt-c t) v->c ret)
                         indent-- " }" indent-nl "else {" indent++ indent-nl
                         ((Expr-fmt-c f) v->c ret) indent-- " }" indent-- " }"))

        #:wp (λ (r Q)
               ((Expr-wp c)
                c-id
                (logic-and (logic-implies (logic-var c-id)
                                          ((Expr-wp t) r Q))
                           (logic-implies (logic-not (logic-var c-id))
                                          ((Expr-wp f) r Q)))))))

(define/contract Skip Expr?
  (Expr #:type Void
        #:unsafe? #f
        #:read-vs mt-set
        #:lval #f
        #:lval-ret not-lval
        #:write-vs mt-set
        #:v->ty mt-map
        #:mem #f
        #:rtime (iunit 0)
        #:fmt-c (λ (v->c ret) ";")

        #:wp (λ (r Q) Q)))

(define/contract (Seq* . more)
  (-> Expr? ... Expr?)
  (for/fold ([a Skip]) ([e (in-list more)])
    (Seq a e)))

(define/contract (When c #:P [P 0.5] t)
  (->* (Expr? Expr?) (#:P real?) Expr?)
  (If c #:P P t Skip))

(define/contract (Unless c #:P [P 0.5] f)
  (->* (Expr? Expr?) (#:P real?) Expr?)
  (If c #:P P Skip f))

(define/contract (Abort msg) (-> string? Expr?)
  (Expr #:type Void
        #:unsafe? #f
        #:read-vs mt-set
        #:lval #f
        #:lval-ret not-lval
        #:write-vs mt-set
        #:v->ty mt-map
        #:mem #f
        #:rtime (iunit 0)
        #:fmt-c (λ (v->c ret) (list* "fprintf(stderr, " (~v msg) ");" indent-nl
                                     "exit(1);"))

        #:wp (λ (r Q) (logic-const #f))))

(define/contract (Assert ?) (-> Expr? Expr?)
  ;; XXX Annotate that it can be removed?
  (If #:P 1.0 ? Skip (Abort (~a "Assertion violation: " (gensym 'assert)))))

(define/contract (Loop idx max-count c b
                       #:I [invariant True]
                       #:E [expected-count max-count])
  (->* (Variable? exact-nonnegative-integer? Expr-Bool? Expr?)
       (#:I Expr? #:E exact-nonnegative-integer?)
       Expr?)
  (define c-ty (Expr-type c))
  (define idx-ty (nearest-IntT #f max-count))
  (define cb-write-vs (Exprs-write-vs c b))
  (when (set-member? cb-write-vs idx)
    (error 'Loop "Cannot mutate loop index"))
  (define c-id (gensym 'Loop_cond))
  (define inv-id (gensym 'Loop_inv))
  (define idx-id (gensym 'Loop_idx))
  (Expr #:type Void
        #:unsafe? (Exprs-unsafe? c b)
        #:read-vs (set-remove (Exprs-read-vs c b) idx)
        #:lval #f
        #:lval-ret not-lval
        #:write-vs cb-write-vs
        #:v->ty (hash-remove (Exprs-v->ty c b) idx)
        #:mem (RecT (hasheq 'c (Expr-mem c)
                            'b (Expr-mem b)))
        #:rtime (ival*k
                 0 expected-count max-count
                 (ival+ (Expr-rtime c) (ival+ (Expr-rtime b) (iunit 1))))
        #:fmt-c
        (λ (v->c ret)
          (define v->c+ (hash-set v->c idx idx-id))
          (list* "{ " indent++
                 ((Type-fmt-decl idx-ty) idx-id) " = 0;" indent-nl
                 ((Type-fmt-decl c-ty) c-id) " = 1;" indent-nl
                 ;; XXX Need to assert that c-id is false when idx-id = max-count
                 "while ( " idx-id " < " max-count " && " c-id " ) {" indent++ indent-nl
                 ((Expr-fmt-c c) v->c+ (fmt-assign c-id)) indent-nl
                 "if ( " c-id ") {" indent++ indent-nl
                 ((Expr-fmt-c b) v->c+ ret)
                 indent-- " }" indent-nl
                 idx-id "++;" indent-nl
                 indent-- "}"
                 indent-- " }"))

        #:wp (λ (r Q)
               (logic-implies
                (logic-bin 'iult (logic-var idx) (logic-const max-count))
                ((Expr-wp c)
                 c-id
                 ((Expr-wp invariant)
                  inv-id
                  (logic-and
                   (logic-var inv-id)
                   (logic-and (logic-implies (logic-and (logic-var c-id)
                                                        (logic-var inv-id))
                                             ((Expr-wp b) r (logic-var inv-id)))
                              (logic-implies (logic-and (logic-not (logic-var c-id))
                                                        (logic-var inv-id)) Q)))))))))

(define/contract (While max-count c b
                        #:I [invariant True]
                        #:E [expected-count max-count])
  (->* (exact-nonnegative-integer? Expr? Expr?)
       (#:I Expr? #:E exact-nonnegative-integer?)
       Expr?)
  (Loop (gensym 'While-idx) max-count c b
        #:I invariant #:E expected-count))

(define/contract (For idx max-count max-e b
                      #:label [label (gensym 'While-label)]
                      #:I [invariant True]
                      #:E [expected-count max-count])
  (->* (Variable? exact-nonnegative-integer? Expr? Expr?)
       (#:I Expr? #:E exact-nonnegative-integer?)
       Expr?)
  (Loop idx max-count (Bin 'iult (Var (nearest-IntT #f max-count) idx) max-e)
        b #:I invariant #:E expected-count))

;; XXX Specification (i.e. function call spot, rather than
;; fully-inlining everything)

(define (tree-for f t)
  (match t
    [(cons a d) (tree-for f a) (tree-for f d)]
    [(or '() #f (? void?)) (void)]
    [x (f x)]))

(define indent-nl (gensym))
(define indent++ (gensym))
(define indent-- (gensym))
(define indent-level (box 0))
(define (idisplay v)
  (match v
    [(== indent-nl)
     (display "\n")
     (for ([i (in-range (unbox indent-level))]) (display #\space))]
    [(== indent++) (set-box! indent-level (+ (unbox indent-level) 2))]
    [(== indent--) (set-box! indent-level (- (unbox indent-level) 2))]
    [x (display x)]))

(define (Expr-emit e)
  ;; XXX Check for no undefined variables
  (match-define (ival rl re rh) (Expr-rtime e))
  (tree-for idisplay
            (list*
             "#include <stdbool.h>" indent-nl
             "#include <stdint.h>" indent-nl
             "#include <stdio.h>" indent-nl
             "#include <stdlib.h>" indent-nl
             indent-nl
             "int main() {" indent++ indent-nl
             "/* Pre = " (logic-format ((Expr-wp e) 'r (logic-const #t)))
             " */" indent-nl
             "// Memory = " (Type-size (Expr-mem e)) " bits" indent-nl
             "// Runtime = ["rl","re","rh"]" indent-nl
             ((Expr-fmt-c e) (hasheq) (λ (v) (list "return " v ";"))) indent-nl
             "return 0;"
             indent-- " }")))

;; XXX Study what LLVM does to this program

(module+ test
  (Expr-emit
   (Seq*
    Skip
    (When False (Assert (Bin 'ieq (Val U32 8) (Val U32 9))))
    (Unless True (Assert (Bin 'ieq (Val U32 11) (Val U32 10))))
    (Let 'ans
         (If (Bin 'ieq (Val U32 7) (Val U32 7))
             (Let 'x (Val U32 6)
                  (Seq
                   (Assign (Var U32 'x) (Val U32 7))
                   (Let 'y (Val F64 3.14)
                        (Bin 'iadd (Var U32 'x) (Val U32 8)))))
             (Let 'z (Val U32 16)
                  (Bin 'iadd (Var U32 'z) (Val U32 9))))
         (Seq*
          (Loop 'i 10 False
                (Assert (Bin 'ieq (Val U32 10) (Val U32 11))))
          (While 10 False
                 (Assert (Bin 'ieq (Val U32 10) (Val U32 11))))
          (For 'i 10 (Val U8 8)
               (Assert (Bin 'iule (Var U8 'i) (Val U8 11)))))))))

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
