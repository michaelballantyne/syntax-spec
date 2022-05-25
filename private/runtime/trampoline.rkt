#lang racket/base

(provide (for-syntax wrap-bind-trampoline))

(require
  racket/base
  
  (for-syntax
   racket/base
   syntax/parse
   ee-lib
   racket/control
   "binding-spec.rkt"))

; MF suggestion: Use `control`

(begin-for-syntax
  (define tag (make-continuation-prompt-tag))
  
  (define (trampoline-bind! id rhs-e #:space [space #f])
    (control-at
     tag k
     #`(begin
         (define-syntax #,((in-space space) id) #,rhs-e)
         (continue-trampoline #,(lambda () (prompt-at tag (k id)))))))

  (define (wrap-bind-trampoline transformer)
    (lambda (stx)
      (prompt-at
       tag
       (parameterize ([do-bind! trampoline-bind!])
         (transformer stx))))))
    
(define-syntax (continue-trampoline stx)
  (syntax-parse stx
    [(_ k)
     ((syntax-e #'k))]))