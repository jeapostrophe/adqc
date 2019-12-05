#lang racket/base
(require syntax/parse/define
         "exec.rkt"
         "stx.rkt"
         "util.rkt")

;; XXX support require
;; XXX fix syntax so that failures blame consuming module, not this one
;; XXX issue with '*' as command line argument? creates huge vector of useless stuff
;; XXX Some way to write tests for '#lang adqc' code without having a different
;; module? Maybe enabling @ reader can help with this?
;; XXX Disable REPL after running? Right now the user gets dumped into a racket/base
;; REPL after running, which is awkward/misleading.

(define (run-module the-p)
  (with-temp-files (c-path bin-path)
    (define exe (make-executable the-p c-path bin-path))
    (define (on-error e)
      (define in (open-input-file c-path))
      (echo-port in (current-error-port))
      (newline (current-error-port))
      (raise e))
    (with-handlers ([exn:fail? on-error])
      (define args (vector->list (current-command-line-arguments)))
      (define stdout (apply executable-run exe args))
      (echo-port stdout)
      (close-input-port stdout))))

(define-simple-macro (adqc-module-begin body ...+)
  (#%module-begin (run-module (Prog* body ...))))

(provide
 (all-from-out "stx.rkt")
 (except-out (all-from-out racket/base) #%module-begin)
 (rename-out [adqc-module-begin #%module-begin]))
