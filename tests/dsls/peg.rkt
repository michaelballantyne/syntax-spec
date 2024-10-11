#lang racket/base

(require "../../testing.rkt")

(begin-for-syntax
  (define-syntax-class string
    (pattern val #:when (string? (syntax-e #'val)))))

(syntax-spec
  (binding-class var #:description "PEG variable")
  (binding-class nonterm #:description "PEG nonterminal")
  (extension-class peg-macro #:description "PEG macro")

  (nonterminal expr
    #:description "PEG action expression"
    v:var
    s:string
    (list e:expr ...))

  (nonterminal peg-top
    n:peg #:binding (nest n []))
  
  (nonterminal/nesting peg (tail)
    #:description "PEG expression"
    #:allow-extension peg-macro

    n:nonterm           ; confusing! Probably shouldn't use pegs as the paper example...
    (eps)               ; can't just be `eps` yet.
    
    (seq e1:peg e2:peg)
    #:binding (nest e1 (nest e2 tail))
    
    (alt e1:peg e2:peg)
    #:binding [(nest e1 []) (nest e2 [])]
    
    (repeat e:peg)      ; *
    #:binding (nest e [])
    
    (not e:peg)         ; !
    #:binding (nest e [])
    
    (bind x:var e:peg)  ; :
    #:binding (scope (bind x) (nest e []) tail)
    
    (=> pe:peg e:expr)
    #:binding (nest pe e)
    
    (text e:expr) ; right now these are referring to the expr syntax class. Need escape to Racket...
    
    (char e:expr)
    (token e:expr)
    (src-span v:var e:peg)
    #:binding (scope (nest e []))
    
    ;; can't do implicit #%peg-datum yet.
    
    ))

(check-equal?
 (expand-nonterminal/datum peg-top
   (=> (seq (bind a (text "a")) (bind b (=> (bind c (text "b"))
                                            (list a c))))
       (list a b)))
 '(=> (seq (bind a (text "a")) (bind b (=> (bind c (text "b"))
                                           (list a c))))
      (list a b)))
