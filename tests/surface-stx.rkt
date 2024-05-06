#lang racket/base

(require "../testing.rkt")


(define (check-exn-syntax-datum-equal datum thunk)
  (check-exn
   (lambda (exn) (equal? (syntax->datum (car (exn:fail:syntax-exprs exn)))
                         datum))
   thunk))

(define-syntax-rule
  (check-syntax-error-datum datum expr)
  (check-exn-syntax-datum-equal
   datum
   (lambda ()
     (convert-compile-time-error
      expr))))

(begin-for-syntax

  (define (source-location-contained? inner outer)
    (and (equal? (syntax-source inner)
                 (syntax-source outer))
         (>= (syntax-position inner)
             (syntax-position outer))
         (<= (+ (syntax-position inner)
                (syntax-span inner))
             (+ (syntax-position outer)
                (syntax-span outer)))))

  ;; Example: (and g) → g
  ;; This would naively highlight (and g), but in this case
  ;; we want to highlight g instead. So, we check whether
  ;; one expression is contained in the other, and if so,
  ;; keep the srcloc of the inner one, to handle this.
  (define (propagate-syntax-loc f)
    (λ (stx)
      (let ([res (f stx)])
        (datum->syntax res  ; lexical context
                       ;; datum
                       (syntax-e res)
                       ;; for srcloc
                       (if (source-location-contained? res stx)
                           res
                           stx)
                       ;; for properties
                       res))))

  (define (raise-surface-syntax-error message form)
    (raise-syntax-error #f message (syntax-surface-stx form))))


(syntax-spec
  (extension-class flow-macro)
  
  (nonterminal floe
    #:allow-extension flow-macro
    id
    (err f:floe)
    (thread f:floe ...))

  (host-interface/expression
   (flow f:floe)
   #'(compile-flow f)))

(begin-for-syntax
  (define (built-in-flow-macro f)
    (flow-macro
     (propagate-syntax-loc
      f))))

(define-syntax compile-flow
  (syntax-parser
    #:datum-literals (id err thread)
    [(_ (thread f ...))
     #'(list (compile-flow f) ...)]
    [(_ id)
     #'(lambda (x) x)]
    [(_ (~and form (err f)))
     (raise-surface-syntax-error "error" #'form)]))

(flow (thread (err (thread id))))

(flow (thread (err-id)))
->
(flow (thread (err id)))

#;(check-syntax-error-datum
 '(err (thread id))
 (flow (thread (err (thread id)))))

(define-syntax err-id
  (flow-macro
   (syntax-rules ()
     [(_)
      (err id)])))

(define-syntax err-id/built-in
  (built-in-flow-macro
   (syntax-rules ()
     [(_)
      (err id)])))

#;(flow (thread (err-id)))

#;(convert-compile-time-error
 (flow (thread (err-id/built-in))))

#;(check-syntax-error-datum
   '(err-id/built-in)
   (flow (thread (err-id/built-in))))

#|
(define-syntax id-id
  (flow-macro
   (syntax-rules ()
     [(_)
      (thread id id)])))

    
;; Wrong!
(check-syntax-error-datum
 '(err (thread id id))
 (flow (thread (err (id-id)))))
|#
