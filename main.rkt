#lang racket/base
(require racket/require reprovide/reprovide)
(reprovide
 (subtract-in "ast.rkt" "type.rkt")
 "type.rkt"
 "eval.rkt"
 "stx.rkt"
 "compile.rkt"
 "linker.rkt"
 "exec.rkt"
 "print.rkt")
(module reader syntax/module-reader
  adqc/module)
