#lang info
(define collection "adqc")
(define deps '("reprovide-lang"
               "threading-lib"
               "graph"
               "base"))
(define build-deps '("chk-lib"))
(define version "0.1")
(define pkg-authors '(jeapostrophe))
(define compile-omit-paths '("v1" "v2" "v3"))
