#lang racket/base

(require
  racket/base
  racket/private/check
  syntax/id-table
  (for-template racket/base)
  (for-syntax racket/base syntax/parse)
  "flip-intro-scope.rkt"
  "syntax-datum.rkt"
  "binding.rkt"
  "syntax-serializer.rkt")

(provide
 syntax-datum?
 define-persistent-free-id-table
 persistent-free-id-table?
 persistent-free-id-table-context?
 persistent-free-id-table-has-key?
 persistent-free-id-table-set!
 persistent-free-id-table-ref
 wrap-persist

 (struct-out persistent-free-id-table))

(define persistent-free-id-table-context-param (make-parameter #f))

(define (persistent-free-id-table-context?)
  (persistent-free-id-table-context-param))

; Design note: we can't persist via a lift because that'd end up at the end of the module,
; so entries wouldn't be available during module visit until the end of the module
; is reached.

(struct persistent-free-id-table [persisted [transient #:mutable] id])

(define (make-persistent-free-id-table id)
  (persistent-free-id-table
   (make-free-id-table)
   (make-free-id-table)
   id))

(define tables-needing-persist (make-hasheq))

(define/who (persistent-free-id-table-set! t id val)
  (check who persistent-free-id-table? t)
  (check who identifier-with-binding? id)
  (check who (lambda (v) (or (syntax? v) (syntax-datum? v)))
         #:contract "(or/c syntax? syntax-datum?)"
         val)

  (when (and (persistent-free-id-table-has-key? t id)
             ;; Hack: update the table on repeated bindings for top-level ids
             (not (top-binding? id)))
    (error 'persistent-free-id-table-set! "table already has an entry for key"))
  
  (if (module-or-top-binding? id)
      (begin
        (unless (persistent-free-id-table-context?)
          (error 'persistent-free-id-table-set!
                 "not in a persist context"))
  
        (hash-set! tables-needing-persist t #t)
        (free-id-table-set! (persistent-free-id-table-transient t) id val))
      (free-id-table-set! (persistent-free-id-table-persisted t) id val)))

(define/who (persistent-free-id-table-has-key? t id)
  (check who persistent-free-id-table? t)
  (check who identifier-with-binding? id)

  (define unbound-value (gensym))
  (not (eq? unbound-value (persistent-free-id-table-ref t id (lambda () unbound-value)))))

(define (ref-error)
  (error 'persistent-free-id-table-ref "no value found for key"))

(define/who (persistent-free-id-table-ref t id [fail ref-error])
  (check who persistent-free-id-table? t)
  (check who identifier-with-binding? id)

  (define (try-persistent)
    (free-id-table-ref
     (persistent-free-id-table-persisted t)
     id
     fail))
  (free-id-table-ref
   (persistent-free-id-table-transient t) id
   try-persistent))

(define (do-extension! t alist)
  (define p (persistent-free-id-table-persisted t))
  (for ([pair alist])
    (free-id-table-set! p (car pair) (cdr pair))))

(define/who (persist-free-id-table-extensions! t)
  (check who persistent-free-id-table? t)
  
  (define alist
    (for/list ([(k v) (in-free-id-table (persistent-free-id-table-transient t))])
      #`(cons #'#,(flip-intro-scope k) #,(if (syntax? v)
                                             #`(deserialize-syntax-props #'#,(serialize-syntax-props (flip-intro-scope v)))
                                             #`'#,v))))
  
  (set-persistent-free-id-table-transient! t (make-free-id-table))
  
  #`(begin-for-syntax
      (do-extension! #,(persistent-free-id-table-id t)
                     (list . #,alist))))

(define (persist-all-free-id-table-extensions!)
  (define result
    #`(begin
        #,@(for/list ([(t _) tables-needing-persist])
             (persist-free-id-table-extensions! t))))
  (hash-clear! tables-needing-persist)
  result)

(define/who (wrap-persist transformer)
  (check who procedure? transformer)
  
  (lambda (stx)
    (check 'wrap-persist-transformer syntax? stx)

    (if (member (syntax-local-context) '(module top-level))
        (let ([t-result (parameterize ([persistent-free-id-table-context-param #t])
                          (transformer stx))])
          (check 'wrap-persist-transformer syntax? t-result)
          #`(begin
              #,(persist-all-free-id-table-extensions!)
              #,t-result))
        (transformer stx))))

(define-syntax define-persistent-free-id-table
  (syntax-parser
    [(_ name:id)
     (unless (eq? 'module (syntax-local-context))
       (raise-syntax-error #f "only allowed in module context" this-syntax))
     #'(define name (make-persistent-free-id-table (quote-syntax name)))]))

(module* test racket/base
  (require
    rackunit
    (for-syntax
     racket/base
     rackunit
     (submod "..")))

  (begin-for-syntax
    (define-persistent-free-id-table v))

  (define x #f)
  (define y #f)
  
  (define-syntax m1
    (wrap-persist
     (lambda (stx)
       (persistent-free-id-table-set! v #'x 5)
       #'(void))))
  (m1)

  ;; Available during expansion
  (define-syntax m2
    (lambda (stx)
      #`#,(persistent-free-id-table-ref v #'x)))
  (check-equal?
   (m2)
   5)

  ;; Available at visit-time
  (begin-for-syntax
    (check-equal?
     (persistent-free-id-table-ref v #'x)
     5))

  ;; Default value works
  (define-syntax (m5 stx)
    #`#,(persistent-free-id-table-ref v #'y #f))
  (check-equal?
   (m5)
   #f)
  )
