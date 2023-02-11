#lang racket/base

(require
  syntax/parse
  racket/list
  racket/dict
  racket/match
  (for-template "forms.rkt")
  (for-template (except-in racket/base => *) "runtime.rkt"))

(provide optimize+compile-alts)

(define (flatten-alternatives stx)
  (define alts '())
  (define (add-alt! alt)
    (set! alts (cons alt alts)))
  (let rec ([stx stx])
    (syntax-parse stx #:literal-sets (peg-literals)
      [(alt e1 e2)
       (rec #'e1)
       (rec #'e2)]
      [_ (add-alt! stx)]))
  (reverse alts))

(define (generate-right-associative-plain-alt alternatives)
  (match alternatives
    [(list a) a]
    [(list-rest head rest)
     #`(plain-alt #,head #,(generate-right-associative-plain-alt rest))]))

(define (split-text-alternatives alts)
  (splitf-at
    alts
    (lambda (stx)
      (syntax-parse stx #:literal-sets (peg-literals)
        [(text s) #t]
        [_ #f]))))

(define (optimize+compile-alts stx in-id compile generate-plain-alt)
  (define-values (text-alternatives other-alternatives) (split-text-alternatives (flatten-alternatives stx)))

  (define text-peg
    (and (not (null? text-alternatives))
         (compile-alt-str (map (syntax-parser #:literal-sets (peg-literals) [(text s) (syntax-e #'s)]) text-alternatives) in-id)))

  (define other-peg
    (and (not (null? other-alternatives))
         (compile (generate-right-associative-plain-alt other-alternatives) in-id)))

  (cond
    [(and text-peg other-peg)
     (generate-plain-alt text-peg other-peg)]
    [text-peg text-peg]
    [else other-peg]))

(define (compile-alt-str alternatives in-id)
  #`(let ([s (text-rep-str #,in-id)]
          [init-ix (text-rep-ix #,in-id)])
      (let ([tail-len (- (string-length s) init-ix)])
        (let ([has-c (lambda (offset) (< offset tail-len))]
              [c (lambda (offset) (string-ref s (+ init-ix offset)))]
              [succeed (lambda (offset) (values (step-input* #,in-id offset) (void)))])
          #,(gen-body alternatives 0 in-id)))))

(define (except-last l)
  (take l (- (length l) 1)))

(define (derive alternatives)
  (for/fold ([h (hash)])
            ([a alternatives])
    (hash-update h (string-ref a 0) (lambda (l) (append l (list (substring a 1)))) '())))

(define (gen-body alternatives offset in-id)
  (match alternatives
    [(list "")
     #`(succeed #,offset)]
    [(list s)
     #`(string-rt #,s (step-input* #,in-id #,offset))]
    [else
     (define has-empty? (ormap (lambda (s) (equal? s "")) alternatives))
     (define non-empty (filter (lambda (s) (not (equal? s ""))) alternatives))
     (when (member "" (except-last alternatives))
       (error 'gen-body "unreachable alternative"))
     (define else-k
       (if has-empty?
           #`(succeed #,offset)
           #`(fail)))
     (define cases
       (for/list ([(c c-alts) (in-dict (sort (hash->list (derive non-empty)) char<? #:key car))])
         #`[#,c #,(gen-body c-alts (+ 1 offset) in-id)]))

     #`(if (has-c #,offset)
           (char-case (c #,offset)
                      #,@cases
                      [else #,else-k])
           #,else-k)]))

; Example:
#;(alt-str "ab" "a" "ba" "bb" "")
#;=>
#;(ief (has-c 0)
      (char-case (c 0)
                 [#\a (if (has-c 1)
                          (char-case (c 1)
                                     [#\b (succeed 2)]
                                     [else (succeed 1)])
                          (succeed 1))]
                 [#\b
                  (if (has-c 1)
                      (char-case (c 1)
                        [#\a (succeed 2)]
                        [#\b (succeed 2)]
                        [else (fail)])
                      (fail))]
                 [else
                  (succeed 0)])
      (succeed 0))

(module case racket/base
  (require (for-syntax racket/base racket/list racket/match syntax/parse))

  (provide int-case char-case)
  
  (define-syntax char-case
    (syntax-parser
      #:datum-literals [else]
      [(_ char-e:expr
          [c:char rhs]
          ...+
          [else fail-e])
       (define/syntax-parse (int ...) (map char->integer (syntax->datum #'(c ...))))
       #`(let ([c-int (char->integer char-e)])
           (int-case c-int
                     [int rhs] ...
                     [else fail-e]))]))

  (begin-for-syntax
    (struct option [int rhs])
    (define (int-case-generate options ref fail-e)
      (match options
        ['() (error 'int-case-generate "shouldn't happen")]
        ; 1
        [(list (option int rhs))
         (define option (first options))
         #`(if (eq? #,ref #,int)
               #,rhs
               #,fail-e)]
        ; 2 or 3
        [(list-rest (option int rhs) (and rest (or (list _) (list _ _))))
         #`(if (eq? #,ref #,int)
               #,rhs
               #,(int-case-generate rest ref fail-e))]
        ; > 3
        [_
         (define-values (l1 l2) (split-at options (quotient (length options) 2)))
         #`(if (< #,ref #,(option-int (first l2)))
               #,(int-case-generate l1 ref fail-e)
               #,(int-case-generate l2 ref fail-e))])))
  
  (define-syntax int-case
    (syntax-parser
      #:datum-literals [else]
      [(_ ref:id [i:integer rhs] ...+ [else fail-e])
       (define options (map (lambda (item)
                              (option (syntax->datum (first item)) (second item)))
                            (map syntax->list (syntax->list #'([i rhs] ...)))))
       (int-case-generate options #'ref #'fail-e)])))

(require (for-template 'case))
  

