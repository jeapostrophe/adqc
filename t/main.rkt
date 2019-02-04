#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         adqc
         chk
         racket/file
         racket/match
         ;; XXX Poor code organization? The *-type functions need to be defined
         ;; in type.rkt so they can access the private functions there. But we
         ;; also want them available here. However adding type.rkt to list of
         ;; modules exported by adqc would cause conflicts since type.rkt
         ;; shadows many of the names exported in ast.rkt. Maybe the relevant
         ;; functions should be re-provided by stx.rkt? Or maybe adqc's main.rkt
         ;; should include type.rkt, but only provide certain functions from it?
         (only-in "../type.rkt" expr-type))

;; This assumes the same representation used by the evaluator for data types.
(define (raw-value ty v)
  (match ty
    [(? IntT?) (Int-val v)]
    [(? FloT?) (Flo-val v)]
    [(RecT f->ty _ c-order)
     (for/list ([f (in-list c-order)])
       (raw-value (hash-ref f->ty f) (unbox (hash-ref v f))))]
    [(ArrT dim ety)
     (for/vector #:length dim ([v* (in-vector v)])
       (raw-value ety (unbox v*)))]
    ;; XXX Structs and Unions.
    ))

(define current-invert? (make-parameter #f))
(define-syntax-rule (! . b)
  (parameterize ([current-invert? #t]) . b))

(define ((print-src! c-path) _)
  (define in (open-input-file c-path))
  (for ([ch (in-port read-char in)])
    (display ch (current-error-port)))
  (newline (current-error-port))
  (close-input-port in)
  (delete-file c-path))

(define (TProg1* stx the-p the-cp n args-i expect-ans-i)
  ;; Get type info for args and ans.
  (match-define (IntFun (list (Arg _ arg-tys _) ...) _ ans-ty _ _)
    (unpack-MetaFun (hash-ref (Program-name->fun the-p) n)))
  (define args (for/list ([ai (in-list args-i)])
                 (unbox (eval-init (hash) ai))))
  (define eval-expect-ans (unbox (eval-init (hash) expect-ans-i)))
  (define eval-ans #f)
  (define comp-ans #f)
  (chk #:t (#:src stx (set! eval-ans (eval-program the-p n args-i))))
  (when eval-ans
    (chk (#:src stx eval-ans)
         (#:src stx eval-expect-ans)))
  (define c-path (make-temporary-file "adqc~a.c"))
  (unless the-cp
    (chk #:t (set! the-cp (link-program the-p c-path))))
  (when the-cp
    (with-chk ([chk-inform! (print-src! c-path)])
      ;; XXX Once linked-program-write exists, we can automatically
      ;; convert arguments into the data layout expected by linked-program-run.
      (define comp-args (for/list ([a (in-list args)]
                                   [ty (in-list arg-tys)])
                          (raw-value ty a)))
      (chk #:t (#:src stx
                (set! comp-ans (linked-program-run the-cp n comp-args))))
      (when comp-ans
        (define comp-expect-ans (raw-value ans-ty eval-ans))
        (define comp-ans* (linked-program-read the-cp comp-ans))
        (if (current-invert?)
            (chk #:! (#:src stx comp-ans*)
                 (#:src stx comp-expect-ans))
            (chk (#:src stx comp-ans*)
                 (#:src stx comp-expect-ans)))))
    (delete-file c-path)))

(define-syntax (TProg1 stx)
  (syntax-parse stx
    [(_ the-p:id
        (~optional (~seq #:compiled compiled-p:id)
                   #:defaults ([compiled-p #''#f]))
        . t)
     #:with (n:expr (~and arg-e (~not (~datum =>))) ...
                    (~optional (~seq (~datum =>) ans (~bind [ans-e #'(I ans)]))
                               #:defaults ([ans-e #'#f]))) #'t
     (quasisyntax/loc stx
       (TProg1* #'t the-p compiled-p n (list (I arg-e) ...) ans-e))]))

(define-syntax (TProgN stx)
  (syntax-parse stx
    [(_ the-p:id
        (~optional (~seq #:compiled compiled-p:id)
                   #:defaults ([compiled-p #f]))
        t ...)
     (syntax/loc stx
       (begin (TProg1 the-p (~@ . (~? (#:compiled compiled-p) ())) . t)
              ...))]))

(define-syntax (TProg stx)
  (syntax-parse stx
    [(_ p-body ... #:tests t ...)
     (quasisyntax/loc stx
       (let ([the-p #f]
             [the-cp #f])
         (chk #:t (#:stx #,stx (set! the-p (Prog p-body ...))))
         (chk #:t (#:stx #,stx (set! the-cp (link-program the-p))))
         (TProgN the-p #:compiled the-cp t ...)))]))

(define-syntax (TS stx)
  (syntax-parse stx
    [(_ the-s (~optional (~seq ans)
                         #:defaults ([ans #f])))
     #:with f (generate-temporary)
     ;; XXX Right now TS & TE only work when returning Int/Flo types
     ;; because the 'ans' must be a value which can be initialized with 'E'.
     #:with the-ty #'(~? #,(expr-type (E ans)) S64)
     (quasisyntax/loc stx
       (TProg (define-fun (f) : the-ty the-s)
              #:tests
              #,(syntax/loc stx
                  [(symbol->string 'f) (~@ . (~? (=> ans) ()))])))]))

(define-syntax (TE stx)
  (syntax-parse stx
    [(_ the-e (~optional ans
                         #:defaults ([ans #f])))
     #:with f (generate-temporary)
     (syntax/loc stx
       (TS the-e (~@ . (~? (ans) ()))))]))

(provide TProg1 TProgN TProg TS TE)

(module+ test
  (chk*
   (TE (S64 5) (S64 5))
   (TS (let ([x : S64 := (S64 5)]) x) (S64 5))
   (TE (iadd (S64 5) (S64 6)) (S64 11))
   (TS (begin (define x : S64 := (S64 5))
              (define y : S64 := (S64 6))
              (iadd x y)) (S64 11))
   (TE (isub (S64 6) (S64 5)) (S64 1))
   (TE (imul (S64 3) (S64 4)) (S64 12))
   (TE (iudiv (U64 100) (U64 10)) (U64 10))
   (TE (isdiv (S64 13) (S64 4)) (S64 3))
   (TE (isdiv (S64 12) (S64 4)) (S64 3))
   (TE (isrem (S64 12) (S64 5)) (S64 2))
   (TE (iurem (U64 105) (U64 10)) (U64 5))
   (TE (ishl (S64 2) (S64 1)) (S64 4))
   (TE (iashr (S64 4) (S64 1)) (S64 2))
   (TE (iashr (S8 -128) (S8 2)) (S8 -32) )
   (TE (ilshr (U8 16) (U8 2)) (U8 4))
   (TE (ior (S64 1) (S64 2)) (S64 3))
   (TE (ior (U8 1) (U8 2)) (U8 3))
   (TE (iand (S64 3) (S64 1)) (S64 1))
   (TE (iand (U8 3) (U8 1)) (U8 1))
   (TE (ixor (S64 3) (S64 2)) (S64 1))
   (TE (ixor (U8 3) (U8 2)) (U8 1))
   ;; Integer comparison
   (TE (ieq (S64 1) (S64 1)) (S32 1))
   (TE (ieq (S64 -1) (S64 2)) (S32 0))
   (TE (ieq (S64 -2) (S64 -2)) (S32 1))
   (TE (ieq (S64 1) (S64 2)) (S32 0))
   (TE (ine (S64 1) (S64 2)) (S32 1))
   (TE (ine (S64 1) (S64 1)) (S32 0))
   (TE (ine (S64 -2) (S64 5)) (S32 1))
   (TE (ine (S64 -2) (S64 -2)) (S32 0))
   (TE (iugt (U8 5) (U8 4)) (S32 1))
   (TE (iugt (U8 4) (U8 5)) (S32 0))
   (TE (isgt (S8 1) (S8 0)) (S32 1))
   (TE (isgt (S8 0) (S8 1)) (S32 0))
   (TE (isgt (S8 0) (S8 -1)) (S32 1))
   (TE (iuge (U8 1) (U8 1)) (S32 1))
   (TE (isge (S8 1) (S8 1)) (S32 1))
   (TE (isge (S8 1) (S8 -1))(S32 1))
   (TE (iult (U8 4) (U8 5)) (S32 1))
   (TE (iult (U8 5) (U8 4)) (S32 0))
   (TE (islt (S8 0) (S8 1)) (S32 1))
   (TE (islt (S8 1) (S8 0)) (S32 0))
   (TE (islt (S8 -1) (S8 0))(S32 1))
   (TE (iule (U8 1) (U8 1)) (S32 1))
   (TE (isle (S8 1) (S8 1)) (S32 1))
   (TE (isle (S8 -1) (S8 1)) (S32 1))
   ;; Floating point arithmetic
   (TE (fadd (F64 2.0) (F64 1.5)) (F64 3.5))
   (TE (fsub (F64 6.1) (F64 0.1)) (F64 6.0))
   (TE (fmul (F64 1.5) (F64 1.5)) (F64 2.25))
   (TE (fdiv (F64 9.0) (F64 1.5)) (F64 6.0))
   (TE (frem (F64 5.3) (F64 2.0)) (F64 1.2999999999999998))
   ;; Floating point comparisons
   (TE (ffalse (F64 +nan.0) (F64 +nan.0)) (S32 0))
   (TE (ftrue (F64 +nan.0) (F64 +nan.0)) (S32 1))
   (TE (foeq (F64 1.0) (F64 1.0)) (S32 1))
   (TE (foeq (F64 1.0) (F64 2.0)) (S32 0))
   (TE (foeq (F64 1.0) (F64 +nan.0)) (S32 0))
   (TE (fogt (F64 1.0) (F64 0.0)) (S32 1))
   (TE (fogt (F64 0.0) (F64 1.0)) (S32 0))
   (TE (fogt (F64 1.0) (F64 +nan.0)) (S32 0))
   (TE (foge (F64 1.0) (F64 1.0)) (S32 1))
   (TE (foge (F64 0.0) (F64 1.0)) (S32 0))
   (TE (foge (F64 1.0) (F64 +nan.0)) (S32 0))
   (TE (folt (F64 0.0) (F64 1.0)) (S32 1))
   (TE (folt (F64 1.0) (F64 0.0)) (S32 0))
   (TE (folt (F64 1.0) (F64 +nan.0)) (S32 0))
   (TE (fole (F64 1.0) (F64 1.0)) (S32 1))
   (TE (fole (F64 1.0) (F64 0.0)) (S32 0))
   (TE (fole (F64 1.0) (F64 +nan.0)) (S32 0))
   (TE (fone (F64 1.0) (F64 1.0)) (S32 0))
   (TE (fone (F64 0.0) (F64 1.0)) (S32 1))
   (TE (fone (F64 1.0) (F64 +nan.0)) (S32 0))
   (TE (ford (F64 1.0) (F64 1.0)) (S32 1))
   (TE (ford (F64 1.0) (F64 +nan.0)) (S32 0))
   (TE (fueq (F64 1.0) (F64 1.0)) (S32 1))
   (TE (fueq (F64 0.0) (F64 1.0)) (S32 0))
   (TE (fueq (F64 1.0) (F64 +nan.0)) (S32 1))
   (TE (fugt (F64 1.0) (F64 0.0)) (S32 1))
   (TE (fugt (F64 0.0) (F64 1.0)) (S32 0))
   (TE (fugt (F64 1.0) (F64 +nan.0)) (S32 1))
   (TE (fuge (F64 1.0) (F64 1.0)) (S32 1))
   (TE (fuge (F64 0.0) (F64 1.0)) (S32 0))
   (TE (fuge (F64 1.0) (F64 +nan.0)) (S32 1))
   (TE (fult (F64 0.0) (F64 1.0)) (S32 1))
   (TE (fult (F64 1.0) (F64 0.0)) (S32 0))
   (TE (fult (F64 1.0) (F64 +nan.0)) (S32 1))
   (TE (fule (F64 1.0) (F64 1.0)) (S32 1))
   (TE (fule (F64 1.0) (F64 0.0)) (S32 0))
   (TE (fule (F64 1.0) (F64 +nan.0)) (S32 1))
   (TE (fune (F64 0.0) (F64 1.0)) (S32 1))
   (TE (fune (F64 1.0) (F64 1.0)) (S32 0))
   (TE (fune (F64 1.0) (F64 +nan.0)) (S32 1))
   (TE (fueq (F64 1.0) (F64 1.0)) (S32 1))
   (TE (fueq (F64 0.0) (F64 1.0)) (S32 0))
   (TE (fueq (F64 1.0) (F64 +nan.0)) (S32 1))
   (TE (funo (F64 1.0) (F64 +nan.0)) (S32 1))
   (TE (funo (F64 1.0) (F64 2.0)) (S32 0))
   ;; Cast
   (TE ((S8 23) : U32) (U32 23))
   ;; XXX Test fails because eval isn't casting between F32 and F64 correctly.
   (! (TE ((F64 23.3) : F32) (F32 23.3f0)))
   (TE ((F64 23.3) : S32) (S32 23))
   (TE ((S32 23) : F64) (F64 23.0))
   ;; XXX How to test Fail?

   (TS (let ([x : S64 := (S64 1)])
         (void)
         x)
       (S64 1))
   (TS (let ([x : S64 := (S64 1)])
         {x <- (S64 5)}
         x)
       (S64 5))
   (TS (let ([x : S64 := (S64 1)])
         {x <- (iadd (S64 5) (S64 6))}
         x)
       (S64 11))
   (TS (begin (define x : S64 := (S64 0))
              (define y : S64 := (S64 0))
              {x <- (S64 1)} {y <- (S64 2)}
              (iadd x y))
       (S64 3))
   (TS (begin (define x : S64 := (S64 0))
              (define y : S64 := (S64 0))
              (if (ieq (S64 0) (S64 0))
                  {x <- (S64 1)}
                  {y <- (S64 2)})
              (iadd x y))
       (S64 1))
   (TS (begin (define x : S64 := (S64 0))
              (define y : S64 := (S64 0))
              (if (ieq (S64 0) (S64 1))
                  {x <- (S64 1)}
                  {y <- (S64 2)})
              (iadd x y))
       (S64 2))
   (TS (begin (define x : S64 := (S64 0))
              (while (islt x (S64 5))
                     {x <- (iadd x (S64 1))})
              x)
       (S64 5))
   (TS (begin (define x : S64 := (S64 0))
              (define y : S64 := (S64 0))
              (while (islt x (S64 5))
                     {y <- (iadd y x)}
                     {x <- (iadd x (S64 1))})
              y)
       (S64 (+ (+ (+ (+ (+ 0 0) 1) 2) 3) 4)))

   (TS (begin
         (define x : S32 := (S32 0))
         (define y : S32 := (S32 1))
         (set! x (S32 0))
         (void)
         (unless (ieq x (S32 0))
           (error "The world is upside-down!"))
         (let/ec end
           (assert! #:dyn #:msg "y is positive"
                    (iult (S32 0) y))
           (if (ieq (S32 5) (S32 6))
               (set! x (S32 1))
               (set! y (S32 2)))
           (when (ieq y (S32 2))
             (set! x (S32 1))
             (end))
           (while (iult x (S32 6))
                  (set! x (iadd x (S32 1)))))
         (set! y (S32 42))
         (iadd x y))
       (S32 43))

   (TS (begin
         (define x : U32 := (U32 0))
         {x <- (U32 100)}
         x)
       (U32 100))
   (TS (return (if (islt (U32 5) (U32 6))
                   (iadd (S64 2) (S64 3))
                   (isub (S64 5) (S64 6))))
       (S64 5))
   (TS (return (let ([x : U32 := (U32 5)])
                 (iadd x (U32 1))))
       (U32 6))

   (TS (begin (define my-array : (array 3 U32) := (array (U32 0) (U32 1) (U32 2)))
              (iadd (my-array @ (U32 0))
                    (iadd (my-array @ (U32 1))
                          (my-array @ (U32 2)))))
       (U32 (+ 0 1 2)))
   (TS (begin (define a : (array 3 U32) := (array (U32 0) (U32 1) (U32 2)))
              ((a @ (U32 0)) <- (U32 3))
              (a @ (U32 0)))
       (U32 3))
   (TS (begin (define y : S32 := (S32 5))
              (define my-array : (array 3 S32) := (zero (array 3 S32)))
              {y <- (iadd y (my-array @ (U32 2)))}
              y)
       (S32 5))
   ;; ABI for array arguments
   ;; XXX Can't trivially convert vector -> array for ABI right now.
   #;
   (TProg (define-fun (foo [arr : (array 3 S64)]) : S64
            (define a : S64 := (arr @ (U32 0)))
            (define b : S64 := (arr @ (U32 1)))
            (define c : S64 := (arr @ (U32 2)))
            (iadd a (iadd b c)))
          #:tests ["foo" (array (S64 2) (S64 3) (S64 4)) => (S64 9)])
   ;; Callee takes an array as an argument, assigns to it
   (TProg (define-fun (bar [arr : (array 3 S64)]) : S64
            ((arr @ (U32 0)) <- (S64 3))
            (S64 0))
          (define-fun (foo) : S64
            (define arr : (array 3 S64) := (array (S64 0) (S64 1) (S64 2)))
            (define z : S64 := bar <- arr)
            (arr @ (U32 0)))
          #:tests ["foo" => (S64 3)])
   (TProg (define-fun (bar [m : S64]) : S64
            (iadd m (S64 1)))
          (define-fun (foo [n : S64]) : S64
            (define a : S64 := bar <- n)
            a)
          #:tests ["foo" (S64 5) => (S64 6)])
   ;; Callee takes an integer argument by reference, assigns to it
   (let ([bar (F ([#:ref m : S64]) : S64
                 (set! m (iadd m (S64 1)))
                 (S64 1))])
     (TProg (define-fun (foo [n : S64]) : S64
              (define a : S64 := bar <- n)
              n)
            #:tests ["foo" (S64 5) => (S64 6)]))
   (TProg (define-fun (bar [#:ref m : S64]) : S64
            (set! m (iadd m (S64 1)))
            (S64 1))
          (define-fun (foo [n : S64]) : S64
            (define a : S64 := bar <- n)
            n)
          #:tests ["foo" (S64 5) => (S64 6)])
   ;; Private function
   (let ([c-add1 (F ([n : S32]) : S32 (iadd n (S32 1)))])
     (TProg (define-fun (foo [x : S32]) : S32
              (define r : S32 := c-add1 <- x)
              r)
            #:tests ["foo" (S32 5) => (S32 6)]))
   ;; Structs
   (let ([Coord (T (record x S64 y S64))])
     (TS (begin (define c : #,Coord := (record x (S64 5) y (S64 4)))
                (c -> x))
         (S64 5))
     (TS (begin (define c : #,Coord := (record x (S64 1) y (S64 2)))
                ((c -> x) <- (S64 3))
                (c -> x))
         (S64 3))
     (TProg (define-fun (bar [c : #,Coord]) : S64
              (c -> y))
            (define-fun (foo [n : S64]) : S64
              (define p : #,Coord := (record x (S64 0) y n))
              (define m : S64 := bar <- p)
              m)
            #:tests ["foo" (S64 4) => (S64 4)])
     ;; Duplicate public type
     (TProg (include-ty "Coord1" Coord)
            (include-ty "Coord2" Coord)
            (define-fun (foo [n : S64] [m : S64]) : S64
              (define c : #,Coord := (record x n y m))
              (iadd (c -> x) (c -> y)))
            #:tests ["foo" (S64 2) (S64 3) => (S64 5)])
     ;; XXX Can't return structs by pointer while they're compiled on stack.
     #;
     (TProg (define-fun (foo) : #,Coord
              (define c : #,Coord := (record x (S64 1) y (S64 2)))
              c)
            #:tests ["foo" => (record x (S64 1) y (S64 2))])
     ;; Fails with following sterr:
     ;;
     ;; SIGSEGV MAPPER si_code 1 fault on addr 0x1
     ;; Aborted (core dumped)
     ;;
     ;; Cause seems to be passing a record (struct) as an argument.
     #;
     (TProg (define-fun (foo [c : #,Coord]) : S64
              (c -> x))
            #:tests ["foo" (record x (S64 1) y (S64 2)) => (S64 1)])
     )
   ))
