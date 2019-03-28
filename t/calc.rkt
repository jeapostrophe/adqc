#lang racket/base
(require adqc
         racket/file
         racket/system
         syntax/parse/define)

(define-simple-macro (C c)
  (E (S8 (char->integer c))))

(define main
  (F ([n1 : S32] [op : S8] [n2 : S32]) : S32
     ;; XXX Need some way to write result to stdout instead of returning it
     (cond [(= op #,(C #\+))
            (+ n1 n2)]
           [(= op #,(C #\-))
            (- n1 n2)]
           [(= op #,(C #\*))
            (* n1 n2)]
           [(= op #,(C #\/))
            (/ n1 n2)]
           [else (error "invalid op\n")])))

(define calc (Prog (include-fun "main" main)))

(module+ test
  (require chk)
  ;; XXX Write some helper routine for compilation so that users don't have to
  ;; write their own error-handling code every time.
  (define c-path (make-temporary-file "adqc~a.c"))
  (define bin-path (make-temporary-file "adqc~a"))
  (unless (make-exe calc c-path bin-path)
    (newline (current-error-port))
    (define in (open-input-file c-path))
    (for ([ch (in-port read-char in)])
      (display ch (current-error-port)))
    (close-input-port in)
    (delete-file c-path)
    (delete-file bin-path)
    (error "call to compile-exe failed (see stderr)"))
  ;; XXX Write some helper routine for users to call executables without
  ;; having to use `system` directly.
  (define (go . args)
    (apply system*/exit-code bin-path args))
  (chk*
   (chk (go "2" "+" "3") 5)
   (chk (go "3" "-" "1") 2)
   (chk (go "2" "*" "4") 8)
   (chk (go "6" "/" "2") 3))
  (delete-file c-path)
  (delete-file bin-path)
  )