#lang racket/base
(require racket/file
         racket/port
         "exec.rkt"
         "stx.rkt")

;; XXX support require
;; XXX fix syntax so that failures blame consuming module, not this one
;; XXX issue with '*' as command line argument? creates huge vector of useless stuff

(define-syntax-rule (adqc-module-begin . more)
  (#%module-begin
   ;; XXX delete files after
   (let* ([c-path (make-temporary-file "adqc~a.c")]
          [bin-path (make-temporary-file "adqc~a")]
          [args (vector->list (current-command-line-arguments))]
          [exe (make-executable (Prog . more) c-path bin-path)]
          [res (apply executable-run exe args)])
     ;; XXX don't use port->string
     (displayln (port->string res)))))

(provide
 (all-from-out "stx.rkt")
 (except-out (all-from-out racket/base) #%module-begin)
 (rename-out [adqc-module-begin #%module-begin]))