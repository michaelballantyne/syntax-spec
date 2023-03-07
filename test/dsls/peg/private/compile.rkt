#lang racket/base

(provide
 compile-peg
 compile-parse)

(require
  racket/stxparam
  syntax/id-table
  (except-in racket/base => *)
  "forms.rkt"
  "runtime.rkt"
  (for-syntax "compile-alt-str.rkt"
              syntax/parse
              racket/base
              (rename-in syntax/parse [define/syntax-parse def/stx])))

(begin-for-syntax
  (define (bound-vars e)
    (syntax-parse e
      #:literal-sets (peg-literals)
      [(bind v rhs)
       (list #'v)]
      [(seq e1 e2)
       (append (bound-vars #'e1) (bound-vars #'e2))]
      [(? e) (bound-vars #'e)]
      [(* e)
       (bound-vars #'e)]
      [(src-span v e)
       (cons #'v (bound-vars #'e))]
      [_ '()])))

(define v-tmps (make-parameter #f))

(define-syntax generate-plain-alt
  (syntax-parser
    [(_ c1 c2)
     #'(let-values ([(in^ res) c1])
         (if (failure? in^)
             c2
             (values in^ res)))]))

(define-syntax compile-peg
  (syntax-parser
    [(_ stx in)
     (syntax-parse #'stx
       #:literal-sets (peg-literals)
       [eps
        #'(values in (void))]
       [(seq e1 e2)
        #'(let-values ([(in^ res) (compile-peg e1 in)])
            (if (failure? in^)
                (fail)
                (compile-peg e2 in^)))]
       [(plain-alt e1 e2)
        #'(generate-plain-alt (compile-peg e1 in) (compile-peg e2 in))]
       [(alt e1 e2)
        (optimize+compile-alts this-syntax #'in #'compile-peg #'generate-plain-alt)]
       [(? e)
        #'(generate-plain-alt (compile-peg e in) (compile-peg eps in))]
       [(* e)
        (def/stx (v* ...) (bound-vars #'e))
        (def/stx (iter-v* ...) (generate-temporaries (attribute v*)))
        (def/stx (inner-v* ...) (generate-temporaries (attribute v*)))
        #'(let ([iter-v* '()] ...)
            (letrec ([f (lambda (in)
                          (let ([inner-v* #f] ...)
                            (let-values ([(in^ res^)
                                          (syntax-parameterize ([v* (make-rename-transformer #'inner-v*)] ...)
                                            (compile-peg e in))])
                              (if (failure? in^)
                                  (begin
                                    (set! v* (reverse iter-v*)) ...
                                    (values in (void)))
                                  (begin
                                    (set! iter-v* (cons inner-v* iter-v*)) ...
                                    (f in^))))))])
              (f in)))]
       [(! e)
        #'(let-values ([(in^ res) (compile-peg e in)])
            (if (failure? in^)
                (values in (void))
                (fail)))]
       ; TODO use a runtime helper?
       [(bind x e)
        #'(let-values ([(in^ res) (compile-peg e in)])
            (if (failure? in^)
                (fail)
                (if (and (void? res) (not (text-rep? in^)))
                    (error ': "missing semantic value")
                    (begin
                      (set! x
                            (if (not (void? res))
                                res
                                (substring (text-rep-str in) (text-rep-ix in) (text-rep-ix in^))))
                      (values in^ (void))))))]
       [(=> pe e)
        #:with (v* ...) (bound-vars #'pe)
        #:with (v^* ...) (generate-temporaries (attribute v*))
        ;(def/stx (v* ...) (bound-vars #'pe))
        ; these temps are necessary because
        ; (define-syntax-parameter v* (make-rename-transformer #'v*)) ...
        ; would be recursive
        ;(def/stx (x ...) (generate-temporaries (attribute v*)))
        #'(let ([v^* #f] ...)
            (define-syntax-parameter v* (make-rename-transformer #'v^*)) ...
            (let-values ([(in _) (compile-peg pe in)])
              (if (failure? in)
                  (fail)
                  (let ([res e])
                    (values in res)))))]
       [(text s:expr)
        #'(string-rt s in)]
       [(char f)
        #'(char-pred-rt f in)]
       [(token f) ; TODO probably needs a contract check
        #'(token-pred-rt f in)]
       [(#%nonterm-ref name:id) #'(name in)]
       [(src-span v e)
        #'(let-values ([(in^ res tmp) (src-span-rt (lambda (in) (compile-peg e in)) in)])
            (set! v tmp)
            (values in^ res))]
       [_ (raise-syntax-error #f "not a core peg form" this-syntax)])]))

(define-syntax compile-parse
  (syntax-parser
    [(_ f in-e)
     #'(let ([in (wrap-input in-e)])
         (let-values ([(in^ res) (f in)])
           (if (failure? in^)
               (error 'parse "parse failed")
               (parse-result in^ res))))]))
