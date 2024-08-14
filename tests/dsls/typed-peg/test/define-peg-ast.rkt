#lang racket/base
#|
(require
  "../main.rkt"
  "../private/forms.rkt"
  (for-syntax racket/base syntax/parse))

(provide define-peg-ast (struct-out ast))

(struct ast [srcloc] #:transparent)

(define-for-syntax (find-parse-var-bindings stx)
  (syntax-parse stx
    #:literal-sets (peg-literals)
    [eps '()]
    [(seq e1 e2)
     (append (find-parse-var-bindings #'e1)
             (find-parse-var-bindings #'e2))]
    [(alt e1 e2) '()]
    [(? e) (find-parse-var-bindings #'e)]
    [(* e) (find-parse-var-bindings #'e)]
    [(! e) '()]
    [(bind x e) (list #'x)]
    [(=> pe e) '()]
    [(text t) '()]
    [(token f) '()]
    [(char f) '()]
    [(#%nonterm-ref name:id) '()]
    [(src-span v e) (find-parse-var-bindings #'e)]
    [_ (raise-syntax-error #f "not a core peg form" this-syntax)]))

(define-syntax define-peg-ast
  (lambda (stx)
    (syntax-parse stx
      [(_ peg-name:id ast-name:id p:expr)
       (define/syntax-parse p^ (local-expand-peg #'p))
       (define/syntax-parse (var ...) (find-parse-var-bindings #'p^))
       #'(begin
           (struct ast-name ast [var ...] #:transparent)
           (define-peg peg-name
             (=> (:src-span srcloc p^) (ast-name srcloc var ...))))])))
|#
