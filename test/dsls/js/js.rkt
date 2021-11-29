#lang racket/base

(require
  "../../../main.rkt"
  json
  racket/list
  racket/system
  racket/string
  ee-lib/define
  (for-syntax
   racket/base
   syntax/stx
   syntax/id-table
   (rename-in syntax/parse [define/syntax-parse def/stx])))

(provide js
         ?
         function
         set!
         +
         *
         -
         /
         <
         <=
         >
         >=
         ==

         let
         let-syntax
         return
         while
         if
         letrec-syntax)

(define-literal-forms js-binops #:syntax-class js-binop
  "Javascript operators cannot be used directly as Racket expressions"
  (+ * - / < <= > >= ==))

(define-hosted-syntaxes
  (binding-class js-var #:description "Javascript variable")

  (extension-class js-macro)

  (nonterminal js-expr
    #:description "Javascript expression"
    #:bind-literal-set js-exprs
    #:allow-extension js-macro

    ; TODO
    ; (letrec-syntax ([x:TODO rhs:TODO]) body ...)
    ; #:binding {(! x) rhs {(rec body)}}
    
    v:js-var
    n:number

    (op:js-binop e1:js-expr e2:js-expr)
               
    (? c:js-expr e1:js-expr e2:js-expr)               
    (set! x:js-var e:js-expr)
    
    (function (x:js-var ...) body:js-stmt ...)
    #:binding {(! x) {(rec body)}}
     
    (e:js-expr e*:js-expr ...))
  
  (two-pass-nonterminal js-stmt
    #:description "Javascript statement"
    #:bind-literal-set js-stmts
    #:allow-extension js-macro
    
    ; TODO
    ; (let-syntax x:TODO e:TODO)
    ; #:binding [(^ x) e]
                        
    (let x:js-var e:js-expr)
    #:binding [(^ x) e]
    
    (return e:js-expr)

    (while c:js-expr body:js-stmt ...)
    #:binding {(rec body)}
                        
    (if c:js-expr (b1:js-stmt ...) (b2:js-stmt ...))
    #:binding [{(rec b1)} {(rec b2)}]
                        
    e:js-expr))


(begin-for-syntax  
  ; Compilation to JS
  (define (extract-js-expression stx)
    (syntax-parse stx
      #:literal-sets (js-binops js-exprs)
      ; expressions
      [n:number
       (hasheq
        'type "Literal"
        'value (syntax->datum #'n))]
      [x:id
       (extract-ref #'x)]
      [(function (x:id ...) body ...)
       (hasheq
        'type "FunctionExpression"
        'params (stx-map (λ (x) (extract-binder x)) #'(x ...))
        'body (extract-block #'(body ...)))]
      [(? c e1 e2)
       (hasheq
        'type "ConditionalExpression"
        'test (extract-js-expression #'c)
        'consequent (extract-js-expression #'e1)
        'alternate (extract-js-expression #'e2))]
      [(set! var:id e)
       (hasheq
        'type "AssignmentExpression"
        'operator "="
        'left (extract-ref #'var)
        'right (extract-js-expression #'e))]
      [(op:js-binop e1 e2)
       (hasheq
        'type "BinaryExpression"
        'operator (symbol->string (syntax->datum #'op))
        'left (extract-js-expression #'e1)
        'right (extract-js-expression #'e2))]
      [(e e* ...)
       (hasheq
        'type "CallExpression"
        'callee (extract-js-expression #'e)
        'arguments (stx-map extract-js-expression #'(e* ...)))]
      ))
  
  (define (extract-js-statement stx)
    (syntax-parse stx
      #:literal-sets (js-stmts)
      ; statements
      [(let x:id e)
       (hasheq
        'type "VariableDeclaration"
        'kind "let"
        'declarations
        (list (hasheq
               'type "VariableDeclarator"
               'id (extract-binder #'x)
               'init (extract-js-expression #'e))))]
      ; TODO
      #;[(let-syntax m:id e)
         (hasheq 'type "EmptyStatement")]
      [(return e)
       (hasheq
        'type "ReturnStatement"
        'argument (extract-js-expression #'e))]
      [(while condition body ...)
       (hasheq
        'type "WhileStatement"
        'test (extract-js-expression #'condition)
        'body (extract-block #'(body ...)))]
      [(if c (b1 ...) (b2 ...))
       (hasheq
        'type "IfStatement"
        'test (extract-js-expression #'c)
        'consequent (extract-block #'(b1 ...))
        'alternate (extract-block #'(b2 ...)))]
      
      [_ ; if it wasn't one of the statements, it should be an expression
       (hash
        'type "ExpressionStatement"
        'expression (extract-js-expression stx))]
      ))

  (struct idmap (table [ctr #:mutable]))
  (define current-idmap (make-parameter #f))
   
  (define (do-extract stx)
    (parameterize ([current-idmap (idmap (make-free-id-table) 0)])
      (extract-js-expression stx)))

  (define (extract-binder id)
    (define map (current-idmap))
    (when (free-id-table-ref! (idmap-table map) id (lambda () #f))
      (raise-syntax-error #f "duplicate binding occurance" id))
    (define name (string-append (symbol->string (syntax->datum id))
                                (number->string (idmap-ctr map))))
    (free-id-table-set! (idmap-table map) id name)
    (set-idmap-ctr! map (+ (idmap-ctr map) 1))
    (hasheq 'type "Identifier" 'name name))
  
  (define (extract-ref id)
    (define name (free-id-table-ref! (idmap-table (current-idmap)) id
                                     (lambda () (raise-syntax-error #f "unbound identifier" id))))
    (hasheq 'type "Identifier" 'name name))

  (define (extract-block body)
    (hasheq
     'type "BlockStatement"
     'body (stx-map (λ (b) (extract-js-statement b))
                    body))))

(define (runjs estree)
  (define out (open-output-string))
  (define err (open-output-string))
  (define f (fifth (process*/ports
                    out
                    (open-input-string (jsexpr->string estree))
                    err
                    (find-executable-path "node")
                    "runjs.js")))
  (f 'wait)
  (displayln (get-output-string err))
  (string-trim (get-output-string out)))

(define-syntax js
  (syntax-parser
    [(_ arg)
     (def/stx expanded-js ((nonterminal-expander js-expr) #'arg))
     (def/stx extracted (do-extract #'expanded-js))
     #'(runjs (hash 'type "ExpressionStatement" 'expression 'extracted))]))

(define-syntax-rule
  (define-js-macro name e)
  (define-syntax name (js-macro e)))

(define-js-macro cond
  (syntax-parser
    #:literals (else)
    [(cond [else e])
     #'e]
    [(cond [c e] clause ...)
     #'(? c e (cond clause ...))]))

(define-js-macro inc!
  (syntax-parser
    [(_ v:id)
     #'(set! v (+ 1 v))]))

(define-js-macro defn
  (syntax-parser
    [(_ (name:id args:id ...) body ...)
     #'(let name (function (args ...) body ...))]))


(module+ test
  (require rackunit)

  (when (not (getenv "PLT_PKG_BUILD_SERVICE"))
    (check-equal?
     (js ((function (n)
                    (return n)) 5))
     "5")

    (check-equal?
     (js ((function ()
                    (let factorial (function (n)
                                             5 ; expressions are allowed in statement position
                                             (if (<= n 1)
                                                 ((return 1))
                                                 ((return (* n (factorial (- n 1))))))))
                    (return (factorial 5)))))
     "120")
  
    ; Thought this was broken due to expander bug, but doesn't seem to be...
    (check-equal?
     (js ((function ()
                    (defn (factorial n)
                      (return (? (<= n 1) 1 (* n (factorial (- n 1))))))
                    (return (factorial 5)))))
     "120")
  
    (check-equal?
     (js ((function ()
                    (let factorial (function (n)
                                             (let i 1)
                                             (let res 1)
                                             (while (<= i n)
                                                    (set! res (* res i))
                                                    (inc! i))
                                             (return res)))
                    (return (factorial 5)))))
     "120")

    (check-equal?
     (js ((function ()
                    (let fib (function (n)
                                       (return
                                        (cond
                                          [(== n 1) 1]
                                          [(== n 2) 1]
                                          [else (+ (fib (- n 1)) (fib (- n 2)))]))))
                    (return (fib 6)))))
     "8")

    ; TODO
    ; A macro defined inside the langauge. Also a use-site binder test.
    #;(check-equal?
       (js ((function ()
                      (let x 5)
                      (let-syntax m (lambda (stx)
                                      (syntax-parse stx
                                        [(_ arg)
                                         #'((function (arg) (return x)) 6)])))
                      (return (m x)))))
       "5")

    ; TODO
    ; Same as previous, but at statement rather than expression position, plus inc!
    #;(check-equal?
       (js ((function ()
                      (let x 5)
                      (let-syntax m (lambda (stx)
                                      (syntax-parse stx
                                        [(_ arg)
                                         #'(return ((function (arg) (return x)) 6))])))
                      (inc! x)
                      (m x))))
       "6")

    ; TODO
    ; use-site scope test for something entirely in an expression context
    #;(check-equal?
       (js ((function ()
                      (let x 5)
                      (return
                       (letrec-syntax ([m (lambda (stx)
                                            (syntax-parse stx
                                              [(_ arg)
                                               #'((function (arg) (return x)) 6)]))])
                         (m x))))))
       "5")

    ; TODO
    #;(check-equal?
       (js ((function ()
                      (let-syntax my-let (syntax-parser
                                           [(_ v rhs)
                                            #'(let v rhs)]))
                      ; I expected surrounding context to be local-expand 'expression
                      ; and not record the use-site scope from this expansion.
                      ; Or... this expansion is occuring within a define/hygienic
                      ; #:definition call, so we're already in a new definition context.
                      ; And the re-expansion is actually in a child of that same context.
                      ; Wouldn't work if we weren't using define/hygienic everywhere.
                      (my-let x 5)
                      (return x))))
       "5")))
