#lang racket/base
(require racket/file
         syntax/parse/define
         "exec.rkt"
         "stx.rkt")

;; XXX support require
;; XXX fix syntax so that failures blame consuming module, not this one
;; XXX issue with '*' as command line argument? creates huge vector of useless stuff
;; XXX Some way to write tests for '#lang adqc' code without having a different
;; module? Maybe enabling @ reader can help with this?
;; XXX Disable REPL after running? Right now the user gets dumped into a racket/base
;; REPL after running, which is awkward/misleading.

(define (echo-port port [out (current-output-port)])
  (for ([ch (in-port read-char port)])
    (display ch out)))

(define (run-module the-p)
  (define c-path (make-temporary-file "adqc~a.c"))
  (define bin-path (make-temporary-file "adqc~a"))
  (define (cleanup! exn)
    (define in (open-input-file c-path))
    (echo-port in (current-error-port))
    (newline (current-error-port))
    (close-input-port in)
    (delete-file c-path)
    (delete-file bin-path)
    (raise exn))
  (define exe (make-executable the-p c-path bin-path))
  (with-handlers ([exn:fail? cleanup!])
    (define args (vector->list (current-command-line-arguments)))
    (define stdout (apply executable-run exe args))
    (echo-port stdout)
    (close-input-port stdout))
  (delete-file c-path)
  (delete-file bin-path))

(define-simple-macro (adqc-module-begin body ...+)
  (#%module-begin
   (run-module
    (Prog
     (define-fun (main) : S32
       body ...
       (return 0))))))

(provide
 (all-from-out "stx.rkt")
 (except-out (all-from-out racket/base) #%module-begin)
 (rename-out [adqc-module-begin #%module-begin]))