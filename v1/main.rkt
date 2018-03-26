#lang racket/base

;; xxx When is (type-)checking done?
;;     During macro expansion
;;     During AST construction
;;     During verification pass

;; xxx Do we construct an AST for the emitted language or do we
;;     construct some other kind of AST and then do work on it? In
;;     particular, we want the emitted language to have types
;;     everywhere, but do inference on the other

;; xxx Does it feel like we are writing Racket or does it feel like we
;;     are writing in ADQC?

;; xxx Do pre/post conditions look like contracts or do they look like
;;     their own thing? How do they "emerge" from the program?

;; xxx How do we create records/arrays and ensure that they are
;;     initialized without creating cycles/etc?

;; xxx Do components contain types? Do interfaces? Do they both? Do
;;     components name their interfaces or are interfaces applied to
;;     them?


;; xxx The language should support multiple return values and
;;     returning records, but the C-subset/low-level language can't do
;;     that, so we need to compile to allocating and then passing by
;;     reference. But the source language doesn't have
;;     pass-by-reference? Or does it? Maybe expresss this with (In :
;;     ...) (Out : ...) (In/Out : ...)

;; xxx Force a particular indentation when writing directly

;; xxx Enforce lower-bound on loops?

#;(interface addy
  )

#;(component add addy
  (define (f [S8 x] [U32 y] [S64 z] -> [S8 a] [U8 b])
    #:pre (> x 0) (> y x) (> z (+ x y))
    #:post (= (+ a b) (+ z x))
    
    ))
