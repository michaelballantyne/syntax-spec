#lang racket/base

(require "../main.rkt"
         rackunit
         (for-syntax racket/base syntax/parse racket/pretty))

(begin-for-syntax
  (define-syntax-class string
    (pattern val #:when (string? (syntax-e #'val)))))

(define-hosted-syntaxes
  (binding-class var "peg variable")
  (binding-class nonterm "peg nonterminal")
  (extension-class peg-macro)

  (nonterminal expr
               #:description "action expression"
               v:var
               s:string
               (list e:expr ...))
  
  (nonterminal peg
               #:description "peg expression"
               #:allow-extension peg-macro

               n:nonterm           ; confusing! Probably shouldn't use pegs as the paper example...
               (eps)               ; can't just be `eps` yet.

               (seq e1:peg e2:peg) ; relies on left-to-right order for effectful binds
               
               (alt e1:peg e2:peg)
               (repeat e:peg)      ; *
               (not e:peg)         ; !
    
               (bind x:var e:peg)  ; :
               #:binding (! x)
    
               (=> pe:peg e:expr)
               ; Seems like we want an `apply` here, if we want left-to-right scope.
               ; or, generalize `nest` treat a single item as a list of length 1.
               #:binding {pe e} 
    
               (text e:expr) ; right now these are referring to the expr syntax class. Need escape to Racket...
               (char e:expr)
               (token e:expr)
               (src-span v:var e:peg)

               ;; can't do implicit #%peg-datum yet.
    
               ))

;; simulated interface macro
(define-syntax peg-expr
  (syntax-parser
    [(_ e) #`'#,((nonterminal-expander peg) #'e)]))


(check-equal?
 (peg-expr
  (=> (seq (bind a (text "a")) (bind b (=> (bind c (text "b"))
                                           (list a c))))
      b))
 '(=> (seq (bind a (text "a")) (bind b (=> (bind c (text "b"))
                                           (list a c))))
      b))