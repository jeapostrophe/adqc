#lang info
(define collection "adqc")
(define deps '("reprovide-lang"
               "threading-lib"
               "graph"
               "pprint"
               "base"))
(define build-deps '("chk-lib"
                     "scribble-lib"))
(define version "0.1")
(define pkg-authors '(jeapostrophe))
(define compile-omit-paths '("v1" "v2" "v3"))
(define scribblings '(("doc/adqc.scrbl" (multi-page) ("ADQC"))))
