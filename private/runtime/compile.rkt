#lang racket/base

(provide #%host-expression
         with-reference-compilers
         (for-syntax setup-default-reference-compilers!
                     binding-as-rkt
                     make-suspension

                     mutable-reference-compiler
                     immutable-reference-compiler

                     make-variable-like-reference-compiler))

(require
  syntax/parse
  (for-syntax
   racket/base
   racket/list
   racket/function
   racket/private/check
   racket/match
   syntax/parse
   "../syntax/env-reps.rkt"
   syntax/id-table
   syntax/transformer
   racket/syntax
   ee-lib)
  (for-meta 2
            racket/base
            syntax/parse
            "../syntax/env-reps.rkt"
            ee-lib))

(define-syntax call-3d-syntax
    (syntax-parser
      [(_ proc arg)
       ((syntax-e #'proc) #'arg)]))

(begin-for-syntax
  ; like make-variable-like-transformer
  (define (make-variable-like-reference-compiler reference-stx [setter-stx #f])
    ; TODO does this need datum->syntax like binding-as-rkt?
    (define transformer
      (make-set!-transformer
       (syntax-parser
         [v:id
          (if (procedure? reference-stx)
              (reference-stx #'v)
              reference-stx)]
         [((~and set! (~literal set!)) v:id new-val)
          (cond
            [(procedure? setter-stx)
             (setter-stx this-syntax)]
            [(syntax? setter-stx)
             #'(setter-stx new-val)]
            [else (raise-syntax-error (syntax-e #'set!) "cannot mutate identifier" this-syntax #'v)])]
         [(v:id . args)
          #`((#%expression (call-3d-syntax #,(lambda (v) ((set!-transformer-procedure transformer) (compile-reference v))) #,(compiled-from #'v))) . args)])))
    transformer)


  (define mutable-reference-compiler
    (make-variable-like-reference-compiler
     (lambda (id) id)
     (lambda (stx) stx)))

  (define immutable-reference-compiler
    (make-variable-like-reference-compiler
     (lambda (id) id)))

  (define suspension-property-key (gensym))

  (define/who (make-suspension stx ctx)
    (check who syntax? stx)
    (check who internal-definition-context? #:or-false ctx)

    ; wrap ctx in a pair because #f is valid as ctx but not as a syntax
    ; property value.
    (syntax-property #`(#%host-expression #,stx) suspension-property-key (list ctx)))

  (define (suspension? stx)
    (not (not (and (syntax? stx)
                   (syntax-property stx suspension-property-key)))))

  (define current-reference-compilers
    (make-parameter (make-immutable-free-id-table)))


  (define (binding-as-rkt bclass-id description)
    (lambda (s stx)
      (define (error-as-rkt)
        (raise-syntax-error
         #f
         (string-append
          description
          " may not be used as a racket expression")
         stx))
      
      (define compile (free-id-table-ref
                      (current-reference-compilers) bclass-id
                      error-as-rkt))

      (define t (if (set!-transformer? compile)
                    (set!-transformer-procedure compile)
                    compile))
      
      (syntax-parse stx
        [v:id
         (t (compile-reference #'v))]
        [((~and set! (~literal set!)) v e)
         (if (set!-transformer? compile)
             (t (datum->syntax this-syntax (list #'set! (compile-reference #'v) #'e) this-syntax this-syntax))
             (raise-syntax-error #f (format "the reference compiler for ~a does not support set!" s) stx #'x))]
        [(v:id . rest)
         (t (datum->syntax this-syntax (cons (compile-reference #'v) #'rest) this-syntax this-syntax))])))

  #;(listof (list identifier? reference-compiler?))
  ; associates binding classes with reference compilers in the default environment.
  ; calling multiple times will add all associations to the default environment.
  ; NOTE: This must only be used before any DSL expansion may occur.
  (define (setup-default-reference-compilers! assocs)
    ; this only works for procedure transformers.
    (define new-reference-compilers
      (for/fold ([env (current-reference-compilers)])
                ([pair assocs])
        (free-id-table-set env (first pair) (second pair))))
   (current-reference-compilers new-reference-compilers)))

(define-syntax with-reference-compilers
  (let ([who 'with-reference-compilers])
    (syntax-parser
      [(_ ([bclass:id t-e:expr] ...) body ...+)
       (define binding-compilers
         (for/fold ([env (current-reference-compilers)])
                   ([k (attribute bclass)]
                    [t-e (attribute t-e)])
           (define v (eval-transformer t-e))
           ; TODO make this something more general once we allow arbitrary structs
           ; as compilers like match expanders.
           (check who (disjoin procedure? set!-transformer?)
                  #:contract "(or/c (-> syntax? syntax?) set!-transformer?)"
                  v)
           (free-id-table-set env k v)))
       (parameterize ([current-reference-compilers binding-compilers])
         (let-values ([(_ v)
                       (syntax-local-expand-expression
                        #'(let () body ...)
                        #t)])
           v))])))

(define-syntax #%host-expression
  (let ([who '#%host-expression])
    (syntax-parser
      [(_ e:expr)
       (unless (suspension? this-syntax)
         (raise-syntax-error who "can only resume a host expansion suspension value" #'e))
       (define ctx (car (syntax-property this-syntax suspension-property-key)))
       (local-expand #'e
                     'expression
                     '()
                     ctx)])))
