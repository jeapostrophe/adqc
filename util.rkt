#lang racket/base
(require racket/contract/base
         racket/file
         syntax/parse/define)

(define-simple-macro (with-temp-files (c-path:id bin-path:id) body ...+)
  (let ([c-path (make-temporary-file "adqc~a.c")])
    (with-handlers ([any/c (λ (e) (delete-file c-path) (raise e))])
      (let ([bin-path (make-temporary-file "adqc~a")])
        (with-handlers ([any/c (λ (e) (delete-file bin-path) (raise e))])
          body ...
          (delete-file bin-path)
          (delete-file c-path))))))

(define (echo-port port [out (current-output-port)])
  (for ([ch (in-port read-char port)])
    (display ch out)))

(define (snoc l x) (append l (list x)))

(provide
 with-temp-files
 (contract-out
  [echo-port (->* (input-port?) (output-port?) void?)]
  [snoc (-> list? any/c list?)]))
