#lang racket/base

(provide serialize-syntax-props deserialize-syntax-props)

(require racket/match "datum-map.rkt")

;; Serialization walks through the syntax and syntax properties. Whenever a syntax object
;; appears within a property value, the syntax object is lifted into the table
;; (after being recursively serialized) under a gensym key and replaced with an identifier
;; containing the gensym.
;;
;; Such identifiers are the only syntax left in property values. They are only used for
;; their datum content, so the expander's failure to manipulate their scopes is okay.
;;
;; Memoization is used to retain the graph structure wrt. references to syntax objects.
;; Graph structure within the datums contained in syntax object properties is unfolded to a tree
;; because the expander's syntax serialization does not retain this graph structure anyway.
;; Datums within syntax object contents never contain graph structure because datum->syntax unfolds it.

(struct serialized-syntax
  (contents  ;; syntax?
   table)    ;; (hash/c symbol? syntax?)
  #:prefab)

(define (serialize-syntax-props stx)
  (define gensym (make-counting-gensym))
  
  (define table (hasheq))
  (define (lift! el)
    (define tag-sym (gensym))
    (set! table (hash-set table tag-sym el))
    (datum->syntax #f tag-sym))
  
  (define recur
    (make-memoized
     (lambda (stx) 
       (syntax-preserved-properties-map
        stx
        recur
        (lambda (stx) (lift! (recur stx)))))))

  (define top-s (recur stx))
  (datum->syntax #f (serialized-syntax top-s table)))

(define (deserialize-syntax-props ser)
  (match-define (serialized-syntax contents table-stx) (syntax-e ser))
  (define table (syntax-e table-stx))

  (define recur
    (make-memoized
     (lambda (stx) 
       (syntax-preserved-properties-map
        stx
        recur
        (lambda (stx) (recur (hash-ref table (syntax-e stx))))))))

  (recur contents))

(define (syntax-preserved-properties-map stx content-f prop-f)
  (define new-content
    (syntax-content-map
     (syntax-e stx)
     content-f))

  (for/fold ([stx^ (datum->syntax stx new-content stx #f)])
            ([k (syntax-property-symbol-keys stx)])
    (define orig-prop-val (syntax-property stx k))
    (define new-prop-val
      (syntax-content-map
       orig-prop-val
       prop-f))
    (syntax-property stx^ k new-prop-val #t)))

(define (syntax-content-map s f)
  (datum-map s
             (lambda (tail? v) v)
             (lambda (tail? v)
               (cond
                 [(syntax? v) (f v)]
                 [else v]))
             (hasheq 'cycle-fail (lambda (v) (error 'serialize-syntax-props "cannot serialize cyclic data")))
             #f))

(define (make-counting-gensym)
  (let ([ctr 0])
    (lambda ()
      (set! ctr (+ ctr 1))
      (string->uninterned-symbol (string-append "s" (number->string ctr))))))

(define (make-memoized f)
  (define memo-table (make-hasheq))
  (lambda (v)
    (hash-ref!
     memo-table v
     (lambda () (f v)))))


(module+ test
  (require rackunit)

  (define type
    (syntax-property
      (syntax-property #'Int ':: #'Type #t)
      'orig (list #'Int) #t))
  (define term (syntax-property #`(1 #,(syntax-property #'2 ': type #t)) ': #'Type #t))
  (define s (serialize-syntax-props term))
  (define d (deserialize-syntax-props s))

  (check-true
   (bound-identifier=?
    (syntax-property d ':)
    #'Type))

  ;; Syntax with properties inside outer syntax with properties
  (check-true
   (bound-identifier=?
    (syntax-property (syntax-property (cadr (syntax-e d)) ':) '::)
    #'Type))

  (check-true
   (bound-identifier=?
    (syntax-property (cadr (syntax-e d)) ':)
    #'Int))

  (check-equal?
   (syntax-position term)
   (syntax-position d))
  
  (check-equal?
   (syntax-position (syntax-property (cadr (syntax-e term)) ':))
   (syntax-position (syntax-property (cadr (syntax-e d)) ':)))

  (check-equal?
   (syntax-position (car (syntax-e term)))
   (syntax-position (car (syntax-e d))))

  ;; Syntax in datum in properties
  (check-true
   (bound-identifier=?
    (car (syntax-property (syntax-property (cadr (syntax-e d)) ':) 'orig))
    #'Int))

  ;; Serialziation should preserve structure sharing
  (let ()
    ;; Sharing within content
    (define s1 #'Foo)
    (define s2 #`(#,s1 #,s1))
    (syntax-case (deserialize-syntax-props (serialize-syntax-props s2)) ()
      [(a b)
       (check-eq? #'a #'b)])

    ;; Sharing within a property
    (define s4 (syntax-property #'y 'foo #`(#,s1 #,s1) #t))
    (syntax-case (syntax-property (deserialize-syntax-props (serialize-syntax-props s4)) 'foo) ()
      [(a b)
       (check-eq? #'a #'b)])

    ;; Sharing between content and properties
    (define s3 (syntax-property #`(#,s1) 'foo s1 #t))
    (define s3-d (deserialize-syntax-props (serialize-syntax-props s3)))
    (check-eq? (syntax-case s3-d () [(a) #'a]) (syntax-property s3-d 'foo)))

  ;; Serialization should be performant for syntax that would explode in size if
  ;; its graph structure were unfolded into a tree.
  (let ()
    (define (make-shared-stx depth)
      (if (= 0 depth)
          #'x
          (let ([v (make-shared-stx (- depth 1))])
            (syntax-property #`(#,v #,v) ':: v #t))))

    (void (deserialize-syntax-props (serialize-syntax-props (make-shared-stx 1000))))))