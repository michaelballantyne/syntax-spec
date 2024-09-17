#lang racket

; like peg, but no parser results. just builds recognizers, not parsers.

(provide define-pegs
         match?
         text
         repeat
         alt
         seq)
(module+ test)
(require "../../testing.rkt")

; a Stream is a
(struct stream [text index] #:transparent)
; represents an input stream
; text is a String
; index is a Natural representing the index into the string

(syntax-spec
  (binding-class nonterm #:description "PEG nonterminal")
  (extension-class peg-macro #:description "PEG macro")

  (nonterminal peg
    #:description "PEG expression"
    #:allow-extension peg-macro
    (~> n:id
        #'(#%nonterm-ref n))
    (#%nonterm-ref n:nonterm)
    (~> s:string
        #'(text s))
    (text s:text-expr)
    (repeat p:peg)
    (alt2 p1:peg p2:peg)
    (seq2 p1:peg p2:peg))

  (nonterminal text-expr
    s:string)

  (host-interface/definitions
   (define-pegs [name:nonterm p:peg] ...)
   #:binding [(export name) ...]
   #'(begin (define name (lambda (s) (compile-peg p s)))
            ...))

  (host-interface/expression
   (match? p:peg s:expr)
   #'(match?-rt (compile-peg p (stream s 0)))))

; (compile-peg p s) compiles to code that matches p against the stream.
; it evaluates to #f if p doesn't match, or a new stream that starts after the matched text.
; where
; p is a peg expression
; s is a Racket expression that evaluates to a Stream
(define-syntax compile-peg
  (syntax-parser
    [(_ p s)
     (syntax-parse #'p
       #:datum-literals (#%nonterm-ref text repeat alt2 seq2)
       [(#%nonterm-ref n:id) #'(n s)]
       [(text str:string) #'(string-rt str s)]
       [(repeat p)
        #'(let ()
            (define (do-repeat-p s-p)
              (define s^ (compile-peg p s-p))
              (if s^
                  (do-repeat-p s^)
                  s-p))
            (do-repeat-p s))]
       [(alt2 p1 p2)
        #'(or (compile-peg p1 s)
              (compile-peg p2 s))]
       [(seq2 p1 p2)
        #'(let ([s^ (compile-peg p1 s)])
            (and s^ (compile-peg p2 s^)))])]))

; String Stream -> (Union #f Stream)
; match str to the current text in strm
(define (string-rt str strm)
  (match strm
    [(stream text index)
     (define index^ (+ index (string-length str)))
     (and (<= index^ (string-length text))
          (string=? str (substring text index index^))
          (stream text index^))]))

; Stream -> Boolean
; does the result of the matching indicate a full match?
(define (match?-rt s)
  (and s (stream-empty? s)))

; Stream -> Boolean
; Does the input stream not have any text left?
(define (stream-empty? s)
  (>= (stream-index s)
      (string-length (stream-text s))))

(define-dsl-syntax alt peg-macro
  (syntax-parser
    [(alt p) #'p]
    [(alt p0 p ...) #'(alt2 p0 (alt p ...))]))

(define-dsl-syntax seq peg-macro
  (syntax-parser
    [(seq p) #'p]
    [(seq p0 p ...) #'(seq2 p0 (seq p ...))]))

(module+ test
  (require rackunit)
  (check-true (match? "" ""))
  (check-false (match? "" "a"))
  (check-true (match? "foo" "foo"))
  (check-false (match? "foo" "fo"))
  (check-false (match? "foo" "foobar"))
  (check-true (match? (alt "a" "b")
                      "a"))
  (check-true (match? (alt "a" "b")
                      "b"))
  (check-false (match? (alt "a" "b")
                       "c"))
  (check-true (match? (seq "a" "b")
                      "ab"))
  (check-false (match? (seq "a" "b")
                       "aab"))
  (check-false (match? (seq "a" "b")
                       "abb"))
  (check-true (match? (seq "a" "b" "c")
                      "abc"))
  (check-true (match? (repeat "a") ""))
  (check-true (match? (repeat "a") "a"))
  (check-true (match? (repeat "a") "aa"))
  (check-true (match? (repeat "a") "aaaaaaaaa"))
  (check-false (match? (repeat "a") "aaaaaaaaab"))
  (check-false (match? (repeat "a") "b"))
  (check-true (match? (seq (repeat "a") "b")
                      "b"))
  (check-true (match? (seq (repeat "a") "b")
                      "aaab"))
  (define-pegs [foo "foo"])
  (check-true (match? foo "foo"))
  ; recursive
  (define-pegs [balanced-parens (seq "(" (repeat balanced-parens) ")")])
  (check-true (match? balanced-parens "()"))
  (check-true (match? balanced-parens "(()((())))"))
  (check-false (match? balanced-parens "("))
  (check-false (match? balanced-parens "(()()")))
