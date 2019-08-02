#lang racket/base
(require adqc
         racket/file
         syntax/parse/define)

(define-simple-macro (C c)
  (E (U8 (char->integer c))))

(define-fun S32 main ([S32 n1] [U8 op] [S32 n2])
  (define result : S32 :=
       (cond [(= op #,(C #\+)) (+ n1 n2)]
             [(= op #,(C #\-)) (- n1 n2)]
             [(= op #,(C #\*)) (* n1 n2)]
             [(= op #,(C #\/)) (/ n1 n2)]
             [else 0]
             ;; XXX Can't put error statement inside of LetE
             #;[else (error "invalid op\n")]))
     (print result "\n")
     (return 0))

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
  (chk*
   (go '("2" "+" "3") 5)
   (go '("3" "-" "1") 2)
   (go '("2" "*" "4") 8)
   (go '("6" "/" "2") 3))
  (delete-file c-path)
  (delete-file bin-path)
  )
