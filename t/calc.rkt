#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         adqc
         racket/file
         racket/port
         racket/stxparam
         syntax/parse/define)

(define-E-expander C
  (syntax-parser
    [(_ c)
     (syntax/loc this-syntax
       (E (U8 (char->integer c))))]))

(define main
  (F+ ([S32 n1] [U8 op] [S32 n2]) : S32
      (define result
        (cond [(= op (C #\+)) (+ n1 n2)]
              [(= op (C #\-)) (- n1 n2)]
              [(= op (C #\*)) (* n1 n2)]
              [(= op (C #\/)) (/ n1 n2)]
              [else (error "invalid op\n")]))
      (println result)
      ;; XXX type can be inferred here.
      (S32 0)))

(define calc (Prog (include-fun main)))

(module+ test
  (require chk)
  (define c-path (make-temporary-file "adqc~a.c"))
  (define bin-path (make-temporary-file "adqc~a"))
  (define exe (make-executable calc c-path bin-path))
  (define (go args expect)
    (define output (apply executable-run exe args))
    (define result (read output))
    (close-input-port output)
    (chk result expect))
  (define (!go args)
    (define result
      (with-handlers ([exn:fail? (λ (e) e)])
        (define output
          (parameterize ([current-error-port (open-output-nowhere)])
            (apply executable-run exe args)))
        (define r (read output))
        (close-input-port output)
        r))
    (chk #:? exn:fail? result))
  (chk*
   (go '("2" "+" "3") 5)
   (go '("3" "-" "1") 2)
   (go '("2" "*" "4") 8)
   (go '("6" "/" "2") 3)
   (!go '("5" "x" "3"))
   )
  (delete-file c-path)
  (delete-file bin-path)
  )
