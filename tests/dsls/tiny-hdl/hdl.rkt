#lang racket/base

(provide
 (all-defined-out)
 (for-space hdl (all-defined-out)))

(require "../../../testing.rkt"
         racket/stxparam
         (for-syntax syntax/transformer racket/syntax)

         racket/bool)

(syntax-spec
  (binding-class inst-name #:description "TinyHDL instance name")
  (binding-class arch-name #:description "TinyHDL architecture name")
  (binding-class entity-name #:description "TinyHDL entity name")

  (nonterminal/exporting top-item
    #:description "TinyHDL declaration"
    #:binding-space hdl
    
    (entity name:entity-name (port:port-spec ...))
    #:binding (export name)

    (architecture name:arch-name e:entity-name stmt:statement ...)
    #:binding [(export name) {(recursive stmt)}])
  
  (nonterminal port-spec
    (m:mode name:id))

  (nonterminal mode
    #:binding-space hdl
    input
    output)
  
  (nonterminal/exporting statement
    #:binding-space hdl
    
    (instance name:inst-name arch:arch-name)
    #:binding (export name)
    
    (assign target:port-ref expr:expression))

  (nonterminal port-ref
    n:id
    (n1:inst-name n2:id))

  (nonterminal expression
    #:binding-space hdl
    
    b:boolean
    (not e:expression)
    (xor e1:expression e2:expression)
    (and e:expression ...)
    (or e:expression ...)
    p:port-ref)

  (host-interface/definitions
    (begin-tiny-hdl t:top-item ...)
    #:binding (re-export t)

    (for ([t (attribute t)])
      (register-entities! t))
    
    #'(begin (compile-top-item t) ...)))


(begin-for-syntax
  (struct entity-info [in-ports out-ports] #:prefab)
  
  (define-persistent-symbol-table entities)
  (define-persistent-symbol-table arch-entity)
  (define-local-symbol-table instance-entity)

  (define register-entities!
    (syntax-parser
      #:datum-literals (entity architecture)
      [(entity name:id ((~alt [(~literal input) in-port:id] [(~literal output) out-port:id]) ...))
       (symbol-table-set!
        entities #'name
        (entity-info (map syntax-e (attribute in-port))
                     (map syntax-e (attribute out-port))))]
      [(architecture name:id e:id stmt ...)
       (symbol-table-set! arch-entity #'name #'e)
       (for ([stmt (attribute stmt)])
         (syntax-parse stmt
           #:datum-literals (instance)
           [(instance name:id arch:id)
            (symbol-table-set! instance-entity #'name (symbol-table-ref arch-entity #'arch))]
           [_ (void)]))]))

  (define (check-input-port port entity)
    (unless (member (syntax-e port) (entity-info-in-ports (symbol-table-ref entities entity)))
      (raise-syntax-error #f (format "entity ~a does not have input port ~a" entity (syntax-e port)) port)))

  (define (check-output-port port entity)
    (unless (member (syntax-e port) (entity-info-out-ports (symbol-table-ref entities entity)))
      (raise-syntax-error #f (format "entity ~a does not have output port ~a" entity (syntax-e port)) port))))

(define-syntax-parameter self-inst #f)
(define-syntax-parameter self-entity #f)

(define-syntax compile-top-item
  (syntax-parser
    #:datum-literals (entity architecture) 
    [(_ (entity name:id ([mode port-name:id] ...)))
     #'(struct name ([port-name #:auto] ...) #:mutable)]
    [(_ (architecture name:id e:id stmt ...))
     #'(define (name)
         (define self (e))
         (syntax-parameterize ([self-inst (make-variable-like-transformer #'self)]
                               [self-entity #'e])
           (compile-statement stmt) ...
           self))]))

(define-syntax compile-statement
  (syntax-parser
    #:datum-literals (assign instance) 
    [(_ (assign port:id e:expr))
     #:with entity (syntax-parameter-value #'self-entity)
     (check-output-port #'port #'entity)
     #'(compile-port-set! entity port self-inst e)]
    [(_ (assign (inst:id port:id) e:expr))
     #:with entity (symbol-table-ref instance-entity #'inst)
     (check-input-port #'port #'entity)
     #'(compile-port-set! entity port inst e)]
    [(_ (instance name:id arch:id))
     #'(define name (arch))]))

(define-syntax compile-port-ref
  (syntax-parser
    [(_ entity port instance)
     #:with field-ref (format-id #'entity "~a-~a" #'entity #'port)
     #'((field-ref instance))]))

(define-syntax compile-port-set!
  (syntax-parser
    [(_ entity port instance rhs)
     #:with field-set! (format-id #'entity "set-~a-~a!" #'entity #'port)
     #'(field-set! instance (lambda () (compile-expression rhs)))]))

(define-syntax compile-expression
  (syntax-parser
    #:datum-literals (not xor and or)
    [(_ b:boolean)
     #''b]
    [(_ (not e))
     #'(not (compile-expression e))]
    [(_ (xor e1 e2))
     #'(xor (compile-expression e1) (compile-expression e2))]
    [(_ (and e ...))
     #'(and (compile-expression e) ...)]
    [(_ (or e ...))
     #'(or (compile-expression e) ...)]
    [(_ port:id)
     #:with entity (syntax-parameter-value #'self-entity)
     (check-input-port #'port #'entity)
     #'(compile-port-ref entity port self-inst)]
    [(_ (inst:id port:id))
     #:with entity (symbol-table-ref instance-entity #'inst)
     (check-output-port #'port #'entity)
     #'(compile-port-ref entity port inst)]))

(syntax-spec
  (host-interface/expression
    (print-truth-table a:arch-name (in-port:id ...) (out-port:id ...)
                       (e:expr ...) ...)
    #:with entity (symbol-table-ref arch-entity #'a)
    #'(begin
        (displayln (list 'in-port ... '-> 'out-port ...))
        (let ([inst (a)])
          (compile-port-set! entity in-port inst e)
          ...
          (displayln (list (compile-port-ref entity in-port inst) ... '-> (compile-port-ref entity out-port inst) ...)))
        ...)))