#lang racket/base

(provide identifier-has-binding?
         identifier-with-binding?
         same-binding?
         top-binding?
         module-or-top-binding?
         maybe-raise-ambiguity-error)

(require racket/private/check
         "flip-intro-scope.rkt")

(define/who (identifier-has-binding? id)
  (check who identifier? id)
  
  (not (not (identifier-binding id (syntax-local-phase-level) #t))))

(define (identifier-with-binding? val)
  (and (identifier? val) (identifier-has-binding? val)))

(define/who (same-binding? id1 id2)
  (check who identifier? id1)
  (check who identifier? id2)

  (maybe-raise-ambiguity-error id1)
  (maybe-raise-ambiguity-error id2)
  
  (let ([id1-ext (if (syntax-transforming?) (flip-intro-scope id1) id1)]
        [id2-ext (if (syntax-transforming?) (flip-intro-scope id2) id2)])

    (and (identifier-has-binding? id1-ext)
         (identifier-has-binding? id2-ext)
         (free-identifier=? id1-ext id2-ext))))

(define/who (top-binding? id)
  (check who identifier-with-binding? id)

  (define binding
    (identifier-binding id (syntax-local-phase-level) #t))
    
  (and (list? binding) (= 1 (length binding))))

(define/who (module-or-top-binding? id)
  (check who identifier-with-binding? id)

  (define binding
    (identifier-binding id (syntax-local-phase-level) #t))
  (list? binding))

;; situation         ; identifier-binding  ; syntax-local-value  ; syntax-debug-info
;; bound as syntax   ; non-#f value        ; environment value   ; has matching binding(s)

;; note: the following two cases can't be easily distinguished; this is why `lookup`
;;   never tells you a name is unbound or out-of-context, and instead can only tell you
;;   that it isn't bound to the particular kind of syntax you check via the predicate.
;; bound as racket   ; non-#f value        ; fails               ; has matching binding(s)
;; out of context    ; non-#f value        ; fails               ; has matching binding(s)

;; unbound           ; #f                  ; fails               ; no matching bindings (I hope!)
;; ambiguous         ; #f                  ; fails               ; has matching binding(s)
(define (maybe-raise-ambiguity-error id)
  (define (has-matching-bindings? id)
    (define debug-info (syntax-debug-info (if (syntax-transforming?) (flip-intro-scope id) id)))
    (and
     (hash-has-key? debug-info 'bindings)
     (let* ([bindings (hash-ref debug-info 'bindings)]
            [matching-bindings (filter (lambda (b) (hash-ref b 'match?)) bindings)])
         (not (null? matching-bindings)))))

  (when (and (not (identifier-binding id (syntax-local-phase-level) #t))
             (has-matching-bindings? id))
    ;; have racket raise the error by local-expanding
    (if (syntax-transforming?)
        (local-expand id 'expression '())
        (error 'maybe-raise-ambiguity-error "internal error: don't know how to raise ambiguity error"))))