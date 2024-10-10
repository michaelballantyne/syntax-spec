#lang racket/base

(require "../../testing.rkt")

(syntax-spec
  (binding-class var #:description "PEG variable")
  (binding-class nonterm #:description "PEG nonterminal")
  (extension-class peg-macro #:description "PEG macro")

  (nonterminal peg-el
    #:description "PEG expression"
    #:allow-extension peg-macro

    n:nonterm
    (eps)
    (char e:expr)
    (token e:expr)
    (alt e1:peg e2:peg)
    (not e:peg)

    (text e:racket-expr)
    
    (=> ps:peg-seq e:racket-expr)
    #:binding (nest ps e))

  (nonterminal/nesting peg-seq (tail)
    #:description "PEG expression"
    #:allow-extension peg-macro
    
    (bind v:var ps:peg-seq)
    #:binding (scope (bind v) (nest ps tail))
    
    (seq ps1:peg-seq ps2:peg-seq)
    #:binding (nest ps1 (nest ps2 tail))

    (repeat ps:peg-seq)
    #:binding (nest ps tail)

    (src-span v:var ps:peg-seq)
    #:binding (scope (bind v) (nest ps tail))

    pe:peg-el)

  (nonterminal peg   
    ps:peg-seq
    #:binding (nest ps [])))

(require racket/match)

(check-true
 (match (expand-nonterminal/datum peg
          (=> (seq (bind a (text "a")) (bind b (=> (bind c (text "b"))
                                                   (list a c))))
              (list a b)))
   [`(=> (seq (bind a (text ,_)) (bind b (=> (bind c (text ,_))
                                             ,_)))
         ,_)
    #t]))
