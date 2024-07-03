#lang typed/racket

(provide
 (rename-out [compile-peg-top compile-peg])
 compile-parse)

(require
  racket/stxparam
  syntax/id-table
  syntax/srcloc
  (except-in racket/base => *)
  "forms.rkt"
  "runtime.rkt"
  (for-syntax syntax/parse
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

#|
you have the PEG and input coming in
you need the result and the updated input out

|#

(define-syntax-rule (compile-peg-top pe in) (let ([in-v in]) (compile-peg pe in-v result in^ (values in^ result) (fail))))

(define-syntax compile-peg
  (syntax-parser
    ; parses `pe` on input stream `in`. If the parse succeeds, binds `result` to the result and `in^` to the updated
    ; input stream in `on-success`. Otherwise, evaluates to `on-fail`.
     ; assume `result` is not the same as `in^`
    [(_ pe in:id result:id in^:id on-success on-fail)
     (syntax-parse #'pe
       #:literal-sets (peg-literals)
       ; start with =>, :, char, seq
       [eps #'(let ([result (void)] [in^ in]) on-success)]
       [(=> pe e)
        #'(compile-peg pe in ignored in^ (let ([result e]) on-success) on-fail)]
       [(bind x:id pe)
        ; assume `x` is not the same as `result`
        #'(compile-peg pe in x in^
                       (if (and (void? x) (not (text-rep? in^)))
                           (error ': "missing semantic value")
                           (let ([x (if (not (void? x))
                                        x
                                        (substring (text-rep-str in) (text-rep-ix in) (text-rep-ix in^)))]
                                 [result (void)])
                             on-success))
                       on-fail)]
       [(seq pe1 pe2)
        #'(compile-peg pe1 in ignored in-tmp
                       (compile-peg pe2 in-tmp result in^ on-success on-fail)
                       on-fail)]
       [(plain-alt pe1 pe2)
        ; TODO deduplicate on-success?
        #'(compile-peg pe1 in result in^ on-success (compile-peg pe2 in result in^ on-success on-fail))]
       [(alt pe1 pe2)
        #'(compile-peg pe1 in result in^ on-success (compile-peg pe2 in result in^ on-success on-fail))]
       [(? pe)
        (def/stx (v* ...) (bound-vars #'pe))
        ; assume result is not the same as any v*
        ; assume in^ is not the same as any v*
        ; TODO could this happen if src-span and ? are used together?
        #'(compile-peg pe in result in^
                       on-success
                       (let ([v* #f] ... [in^ in] [result (void)]) on-success))]
       [(* pe)
        (def/stx (v* ...) (bound-vars #'pe))
        ; bound to the list of values of its corresponding variable
        (def/stx (iter-v* ...) (generate-temporaries (attribute v*)))
        #'(let ([iter-v* '()] ...)
            (let loop ([in in])
              (compile-peg pe in ignored in-tmp
                           (begin (set! iter-v* (cons v* iter-v*)) ...
                                  (loop in-tmp))
                           ; assume result is not the same as any v*
                           ; assume in^ is not the same as any v*
                           ; TODO could this happen if src-span and * are used together?
                           (let ([v* (reverse iter-v*)]
                                 ...
                                 [in^ in]
                                 [result (void)])
                             on-success))))]
       [(src-span v e)
        ; TODO figure out how to pull some of this back into rt
        ; trickier now since bindings in e must escape and we do nesting let style
        ; can't just do a remote set! anymore
        #'(if (text-rep? in)
              (let ([source (text-rep-source in)]
                    [init-pos (text-rep-ix in)]
                    [init-ln (text-rep-ln in)]
                    [init-col (text-rep-col in)])
                (compile-peg e in result in^
                             (let ([v (srcloc source
                                              init-ln init-col
                                              init-pos (- (text-rep-ix in^) init-pos))])
                               on-success)
                             ; TODO what is the desired semantics of e failing?
                             on-fail))
              (parameterize ([first-token-srcloc #f]
                             [last-token-srcloc #f])
                (compile-peg e in result in^
                             (let ([v (build-source-location (first-token-srcloc)
                                                             (last-token-srcloc))])
                               on-success)
                             on-fail)))]
       [(! pe)
        #'(compile-peg pe in ignored-result ignored-in^
                       on-fail
                       (let ([result (void)] [in^ in]) on-success))]
       [(#%nonterm-ref name)
        #'(let-values ([(in^-tmp result-tmp) (name in)])
            (if (failure? in^-tmp)
                on-fail
                (let ([in^ in^-tmp] [result result-tmp])
                  on-success)))]
       [(char f)
        ; TODO this tmp stuff probably isn't necessary
        ; TODO deduplicate. maybe make runtime cps?
        #'(let-values ([(in^-tmp result-tmp) (char-pred-rt f in)])
            (if (failure? in^-tmp)
                on-fail
                (let ([in^ in^-tmp] [result result-tmp]) on-success)))]
       [(text s:expr)
        #'(let-values ([(in^-tmp result-tmp) (string-rt s in)])
            (if (failure? in^-tmp)
                on-fail
                (let ([in^ in^-tmp] [result result-tmp]) on-success)))]
       [(token f) ; TODO probably needs a contract check
        #'(let-values ([(in^-tmp result-tmp) (token-pred-rt f in)])
            (if (failure? in^-tmp)
                on-fail
                (let ([in^ in^-tmp] [result result-tmp]) on-success)))]
       [_ (raise-syntax-error #f "not a core peg form" this-syntax)])]))

(define-syntax compile-parse
  (syntax-parser
    [(_ f in-e)
     #'(let ([in (wrap-input in-e)])
         (let-values ([(in^ res) (f in)])
           (if (failure? in^)
               (error 'parse "parse failed")
               (parse-result in^ res))))]))
