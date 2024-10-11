#lang racket/base

;; This module is responsible for compiling the surface syntax of a binding specification
;; to syntax that generates a runtime representation of the binding specification.
;; Surface syntax is elaborated into structures, which are then validated
;; and then compiled into syntax which generates structures that the runtime can interpret.

(provide compile-bspec)

(require racket/match
         syntax/parse
         racket/syntax
         syntax/parse/class/paren-shape
         racket/list
         "../../ee-lib/main.rkt"
         "../env-reps.rkt"
         "../syntax-classes.rkt"
         (for-template racket/base
                       "../../runtime/binding-spec.rkt")
         (for-syntax
          racket/base
          syntax/parse))

#;((or/c syntax? #f) (listof pvar?) syntax? -> syntax?)
;; Given a bindingspec's syntax (or lack thereof), bound pvars, and the variant
;; of the bspec (#:simple, #:pass1, etc.),
;; generate syntax that generates a runtime representation of the binding spec
(define (compile-bspec maybe-bspec bound-pvars variant)
  (define bspec-stx (or maybe-bspec #'[]))

  (define bspec-elaborated (elaborate-bspec bspec-stx))
  (check-affine-pvar-use! bspec-elaborated)
  (check-ellipsis-depth! bspec-elaborated)
  (check-ellipsis-homogeneity! bspec-elaborated)
  (define bspec-with-implicits (add-implicit-pvar-refs bspec-elaborated bound-pvars))
  (define bspec-flattened (bspec-flatten-groups bspec-with-implicits))
  (define bspec-combined-imports (bspec-combine-imports bspec-flattened))

  (syntax-parse variant
    [(~or #:simple (#:nesting _))
     (check-order/unscoped-expression bspec-combined-imports)
     (compile-bspec-term/single-pass bspec-combined-imports)]
    [#:pass1
     (check-order/exporting bspec-combined-imports)
     (compile-bspec-term/pass1 bspec-combined-imports)]
    [#:pass2
     #`(fresh-env-expr-ctx #,(compile-bspec-term/pass2 bspec-combined-imports))]))

;; Elaborated representation; variables are associated with expander-environment information

(struct pvar [id info])

(struct with-stx [stx])

;; A BSpec is one of
(struct ref with-stx [pvar] #:transparent)
(struct bind with-stx [pvar] #:transparent)
(struct bind-syntax with-stx [pvar transformer-pvar] #:transparent)
(struct bind-syntaxes with-stx [depth pvar transformer-pvar] #:transparent)
(struct import with-stx [pvar] #:transparent)
; no surface syntax, just a mechanism to combine imports
; something like [(import x) (import y)] ~> (imports (list (import x) (import y)))
; list may contain groups and ellipses of imports, not necessarily just imports
(struct imports with-stx [specs] #:transparent)
(struct re-export with-stx [pvar] #:transparent)
(struct export with-stx [pvar] #:transparent)
(struct export-syntax with-stx [pvar transformer-pvar] #:transparent)
(struct export-syntaxes with-stx [depth pvar transformer-pvar] #:transparent)
(struct nest with-stx [depth pvar spec] #:transparent)
(struct nest-one with-stx [pvar spec] #:transparent) 
(struct suspend with-stx [pvar] #:transparent)
(struct scope with-stx [spec] #:transparent)
(struct ellipsis with-stx [spec] #:transparent)
(struct group with-stx [specs] #:transparent)

(define-match-expander s*
  (syntax-parser
    [(_ sname:id [fname:id p] ...)
     #'(struct* sname ([fname p] ...))]))

#;((BSpec -> BSpec) BSpec -> BSpec)
; bottom-up mapping of spec
(define (map-bspec f spec)
  (match spec
    [(nest stx depth pv s)
     (let ([s^ (map-bspec f s)])
       (f (nest stx depth pv s^)))]
    [(nest-one stx pv s)
     (let ([s^ (map-bspec f s)])
       (f (nest-one stx pv s^)))]
    [(scope stx s)
     (let ([s^ (map-bspec f s)])
       (f (scope stx s^)))]
    [(group stx ss)
     (let ([ss^ (map (lambda (s) (map-bspec f s)) ss)])
       (f (group stx ss^)))]
    [(imports stx ss)
     (let ([ss^ (map (lambda (s) (map-bspec f s)) ss)])
       (f (imports stx ss^)))]
    [(ellipsis stx s)
     (f (ellipsis stx (map-bspec f s)))]
    [_ (f spec)]))

#;(∀ A ((BSpec (listof A) -> A) BSpec -> A))
; bottom-up fold of spec
(define (fold-bspec f spec)
  (match spec
    [(or (s* nest [spec s])
         (s* nest-one [spec s])
         (s* scope [spec s])
         (s* ellipsis [spec s]))
     (let ([s^ (fold-bspec f s)])
       (f spec (list s^)))]
    [(or (imports _ ss) (group _ ss))
     (let ([ss^ (map (lambda (s) (fold-bspec f s)) ss)])
       (f spec ss^))]
    [_ (f spec '())]))


;; Elaborate

#;(syntax? -> BSpec)
; convert surface syntax for a bspec to a structure representation.
(define elaborate-bspec
  (syntax-parser
    #:datum-literals (scope bind bind-syntax bind-syntaxes import export export-syntax export-syntaxes re-export nest nest-one host)
    [v:nonref-id
     (elaborate-ref (attribute v))]
    [(bind ~! v:nonref-id)
     (bind
      this-syntax
      (elaborate-pvar (attribute v)
                      (s* bindclass-rep)
                      "binding class"))]
    [(bind-syntax ~! v:nonref-id v-transformer:nonref-id)
     (bind-syntax
      this-syntax
      (elaborate-pvar (attribute v)
                      (s* extclass-rep)
                      "extension class")
      (elaborate-pvar (attribute v-transformer)
                      (? stxclass-rep?)
                      "syntax class"))]
    [(bind-syntaxes ~! v:nonref-id (~and ooo (~literal ...)) ... v-transformer:nonref-id)
     (bind-syntaxes
      this-syntax
      (length (attribute ooo))
      (elaborate-pvar (attribute v)
                      (s* extclass-rep)
                      "extension class")
      (elaborate-pvar (attribute v-transformer)
                      (? stxclass-rep?)
                      "syntax class"))]
    [(import ~! v:nonref-id)
     (import
      this-syntax
      (elaborate-pvar (attribute v)
                      (s* nonterm-rep [variant-info (s* exporting-nonterm-info)])
                      "exporting nonterminal"))]
    [(re-export ~! v:nonref-id)
     (re-export
      this-syntax
      (elaborate-pvar (attribute v)
                      (s* nonterm-rep [variant-info (s* exporting-nonterm-info)])
                      "exporting nonterminal"))]
    [(export ~! v:nonref-id)
     (export
      this-syntax
      (elaborate-pvar (attribute v)
                      (s* bindclass-rep)
                      "binding class"))]
    [(export-syntax ~! v:nonref-id v-transformer:nonref-id)
     (export-syntax
      this-syntax
      (elaborate-pvar (attribute v)
                      (s* extclass-rep)
                      "extension class")
      (elaborate-pvar (attribute v-transformer)
                      (? stxclass-rep?)
                      "syntax class"))]
    [(export-syntaxes ~! v:nonref-id (~and ooo (~literal ...)) ... v-transformer:nonref-id)
     (export-syntaxes
      this-syntax
      (length (attribute ooo))
      (elaborate-pvar (attribute v)
                      (s* extclass-rep)
                      "extension class")
      (elaborate-pvar (attribute v-transformer)
                      (? stxclass-rep?)
                      "syntax class"))]
    [(nest v:nonref-id spec:bspec-term)
     (nest-one
      this-syntax
      (elaborate-pvar (attribute v)
                      (s* nonterm-rep [variant-info (s* nesting-nonterm-info)])
                      "nesting nonterminal")
      (elaborate-bspec (attribute spec)))]
    [(nest ~! v:nonref-id (~and (~literal ...) ooo) ...+ spec:bspec-term)
     (define depth (length (attribute ooo)))
     (when (> depth 1)
       (wrong-syntax/orig this-syntax "nest cannot contain more than one ellipsis"))
     (nest
      this-syntax
      depth
      (elaborate-pvar (attribute v)
                      (s* nonterm-rep [variant-info (s* nesting-nonterm-info)])
                      "nesting nonterminal")
      (elaborate-bspec (attribute spec)))]
    [(host ~! v:nonref-id)
     (suspend
      this-syntax
      (pvar (attribute v) (lookup-pvar (attribute v))))]
    [(scope ~! spec ...)
     (scope
      #'(spec ...)
      (group this-syntax (elaborate-group (attribute spec))))]
    [(spec ...)
     (group this-syntax (elaborate-group (attribute spec)))]))

; (Listof Syntax) -> (Listof BSpec)
; handles ellipses
(define elaborate-group
  (syntax-parser
    #:datum-literals (import)
    [(spec (~and ooo (~literal ...)) ... . specs)
     ; however many ellipses follow the pattern, wrap the elaborated spec with
     ; the ellipses struct that many times.
     (cons (for/fold ([spec (elaborate-bspec (attribute spec))])
                     ([ooo (attribute ooo)])
             (ellipsis ooo spec))
           (elaborate-group (attribute specs)))]
    [() '()]))

;; Elaborator helpers

#;(identifier? -> BSpec)
(define (elaborate-ref v)
  (ref v (elaborate-pvar v
                         (or (s* bindclass-rep) (s* nonterm-rep) (? stxclass-rep?) (s* nested-binding)
                             (s* special-syntax-class-binding))
                         "binding class, syntax class, or nonterminal")))

(define-syntax-rule
  (elaborate-pvar v-e pattern expected-str-e)
  (elaborate-pvar-rt
   v-e
   (match-lambda
     [pattern #t]
     [_ #f])
   expected-str-e))

#;(pvar? (Any -> boolean?) string? -> Any)
; ensure the pvar is bound to a value that matches info-pred.
; get the value if it is, throw an error with expected-str if it is not.
(define (elaborate-pvar-rt v info-pred expected-str)
  (let ([info (lookup-pvar v)])
    (if (info-pred info)
        (pvar v info)
        (wrong-syntax v (string-append "expected pattern variable associated with a " expected-str)))))

(define (lookup-pvar v)
  (define binding (lookup v pvar-rep?))
  (when (not binding)
    (if (identifier? (current-syntax-context))
        (wrong-syntax/orig v "binding spec expected a reference to a pattern variable")
        (wrong-syntax v "expected a reference to a pattern variable")))
  (pvar-rep-var-info binding))

(define (lookup-pvar-depth v)
  (define binding (lookup v pvar-rep?))
  (when (not binding)
    (if (identifier? (current-syntax-context))
        (wrong-syntax/orig v "binding spec expected a reference to a pattern variable")
        (wrong-syntax v "expected a reference to a pattern variable")))
  (pvar-rep-depth binding))

(define (check-affine-pvar-use! bspec)
  (define pvars (bspec-referenced-pvars bspec))
  (define maybe-dup (check-duplicates pvars free-identifier=?))

  (when maybe-dup
    (wrong-syntax/orig maybe-dup "each pattern variable must occur in the binding spec at most once")))

(define (check-ellipsis-depth! bspec)
  (let loop ([bspec bspec] [depth 0])
    (match bspec
      [(ref _ (pvar v _))
       (check-ellipsis-depth/pvar depth v)]
      [(or (export stx (pvar v _))
           (re-export stx (pvar v _))
           (bind stx (pvar v _))
           (suspend stx (pvar v _)))
       (check-ellipsis-depth/pvar depth v stx)]
      [(or (export-syntax stx (pvar v _) (pvar tv _))
           (bind-syntax stx (pvar v _) (pvar tv _)))
       (check-ellipsis-depth/pvar depth v stx)
       (check-ellipsis-depth/pvar depth tv stx)]
      [(or (export-syntaxes stx v-depth (pvar v _) (pvar tv _))
           (bind-syntaxes stx v-depth (pvar v _) (pvar tv _)))
       (check-ellipsis-depth/pvar (+ v-depth depth) v stx)
       (check-ellipsis-depth/pvar depth tv stx)]
      [(import stx (pvar v _))
       (check-ellipsis-depth/pvar depth v stx)]
      [(or (group _ ss) (imports _ ss))
       (for ([s ss])
         (loop s depth))]
      [(nest stx nest-depth (pvar v _) s)
       (check-ellipsis-depth/pvar (+ nest-depth depth) v stx)
       (loop s depth)]
      [(nest-one stx (pvar v _) s)
       (check-ellipsis-depth/pvar depth v stx)
       (loop s depth)]
      [(scope _ s)
       (loop s depth)]
      [(ellipsis _ s)
       (loop s (add1 depth))])))

(define (check-ellipsis-depth/pvar bs-depth v [stx #f])
  (define ss-depth (lookup-pvar-depth v))
  (cond
    [(< ss-depth bs-depth)
     (wrong-syntax/orig v "too many ellipses for pattern variable in binding spec")]
    [(< bs-depth ss-depth)
     (wrong-syntax/orig v "missing ellipses with pattern variable in binding spec")]))

; makes sure you don't mix categories like refs+subexps and binds in the same ellipsis
(define (check-ellipsis-homogeneity! bspec)
  (map-bspec
   (lambda (bspec)
     (match bspec
       [(ellipsis stx spec)
        (define ref+subexp (find-ref+subexp spec))
        (define bind (find-bind spec))
        (define import (find-import spec))
        (define export (find-export spec))
        (define representatives (filter values (list ref+subexp bind import export)))
        (when (< 1 (length representatives))
            (wrong-syntax/orig stx "cannot mix different binding spec categories inside of ellipses"))
        bspec]
       [_ bspec]))
   bspec))

; BSpec -> (or/c #f BSpec)
; finds a ref or a subexp. doesn't recur into scopes
(define (find-ref+subexp bspec)
  (match bspec
    [(or (s* ref)
         (s* suspend)
         (s* scope)
         (s* nest)
         (s* nest-one))
     bspec]
    [(group _ specs)
     (findf find-ref+subexp specs)]
    [(ellipsis _ spec) (find-ref+subexp spec)]
    [_ #f]))

; BSpec -> (or/c #f BSpec)
; finds a bind. doesn't recur into scopes
(define (find-bind bspec)
  (match bspec
    [(or (s* bind)
         (s* bind-syntax)
         (s* bind-syntaxes))
     bspec]
    [(group _ specs)
     (findf find-bind specs)]
    [(ellipsis _ spec) (find-bind spec)]
    [_ #f]))

; BSpec -> (or/c #f BSpec)
; finds an import. doesn't recur into scopes
(define (find-import bspec)
  (match bspec
    [(or (s* import)
         (s* imports))
     bspec]
    [(group _ specs)
     (findf find-import specs)]
    [(ellipsis _ spec) (find-import spec)]
    [_ #f]))

; BSpec -> (or/c #f BSpec)
; finds an export. doesn't recur into scopes
(define (find-export bspec)
  (match bspec
    [(or (s* export)
         (s* export-syntax)
         (s* export-syntaxes))
     bspec]
    [(group _ specs)
     (findf find-export specs)]
    [(ellipsis _ spec) (find-export spec)]
    [_ #f]))

;; Infer implicit pvar refs

(define (add-implicit-pvar-refs bspec bound-pvars)
  (define unreferenced-pvars
    (remove*
     (bspec-referenced-pvars bspec)
     bound-pvars
     bound-identifier=?))

  (group (with-stx-stx bspec)
         (cons bspec (for/list ([v unreferenced-pvars])
                       (elaborate-ref v)))))

(define (bspec-referenced-pvars spec)
  (fold-bspec
   (lambda (spec children)
     (define node-vars
       (match spec
         [(or (s* ref [pvar (pvar v _)])
              (s* bind [pvar (pvar v _)])
              (s* export [pvar (pvar v _)])
              (s* nest [pvar (pvar v _)])
              (s* nest-one [pvar (pvar v _)])
              (s* suspend [pvar (pvar v _)])
              (s* import [pvar (pvar v _)])
              (s* re-export [pvar (pvar v _)]))
          (list v)]
         [(or (s* bind-syntax [pvar (pvar v1 _)] [transformer-pvar (pvar v2 _)])
              (s* bind-syntaxes [pvar (pvar v1 _)] [transformer-pvar (pvar v2 _)])
              (s* export-syntax [pvar (pvar v1 _)] [transformer-pvar (pvar v2 _)])
              (s* export-syntaxes [pvar (pvar v1 _)] [transformer-pvar (pvar v2 _)]))
          (list v1 v2)]
         [_ '()]))
     (append* node-vars children))
   spec))

;; Flatten groups for easier order analysis

(define (bspec-flatten-groups bspec)
  (map-bspec
   (lambda (spec)
     (match spec
       [(group stx l) (group stx (append* (map flat-bspec-top-elements l)))]
       [_ spec]))
   bspec))

(define (flat-bspec-top-elements el)
  (match el
    [(group _ l) l]
    [_ (list el)]))

; combine consecutive imports in a group
(define (bspec-combine-imports bspec)
  (map-bspec
   (lambda (spec)
     (match spec
       [(group stx l)
        (group stx (let loop ([l l])
                     (match l
                       [(list (and imps (? find-import)) ..1 ss ...)
                        ; TODO combine srclocs of imps instead of using group's stx
                        (cons (imports stx imps) (loop ss))]
                       [(cons s ss)
                        (cons s (loop ss))]
                       [(list) (list)])))]
       [_ spec]))
   bspec))

;; Static checks

; Concerns:
;   - (bind x) must occur within a scope.
;       - (nest a [(bind x) x]) shouldn't be legal as we have
;         no static guarantee about the length of a or whether its
;         associated non-terminal installs any scopes.
;       - (scope (nest a [(bind x) x)])) shouldn't be legal as it would imply binding
;         in a scope created by the nesting non-terminal, and the binding might
;         come after a reference created in the nesting non-terminal.
;   - Bindings should come before import and references within a scope
;   - Exports may only occur at the top-level of a exporting non-terminal,
;     and appear before import and references
;   - Imports can appear after bindings and before
;     references.
;
; Resulting contexts:
;   - Unscoped expression context; references only.
;   - Scoped expression context
;       - Bindings
;       - Then imports
;       - Then references
;   - Exporting context
;       - Exports
;       - Then re-exports
;       - Then references
; The body of a nest is an unscoped expression context, but ideally should have
; special error messages related to nest.
;
; As a grammar:
;
; one-pass-spec: unscoped-spec
; exporting-spec: (seq (* (or (export _) (export-syntax _ _) (export-syntaxes _ _))) (* (re-export _)) refs+subexps)
; unscoped-spec: refs+subexps
; refs+subexps: (* (or (ref _) (nest _ unscoped-spec) (nest-one _ unscoped-spec) (scope scoped-spec)))
; scoped-spec:   (seq (* (or (bind-syntax _ _) (bind-syntaxes _ _) (bind _))) (? (imports _)) refs+subexps)
;
; The implementation below separately implements refs+subexps for each context in which it occurs to
; provide specific error messages.
;

#;((BSpec (listof BSpec) -> A) (listof BSpec) -> A)
; Apply f to the car and cdr of specs or do nothing if it's null.
(define (check-sequence f specs)
  (if (null? specs)
      (void)
      (f (car specs) (cdr specs))))

(define (binding-scope-error stx)
  (wrong-syntax/orig stx "binding must occur within a scope"))

(define (export-context-error stx)
  (wrong-syntax/orig stx "exports may only occur at the top-level of an exporting binding spec"))

(define (re-export-context-error stx)
  (wrong-syntax/orig stx "re-exports may only occur at the top-level of an exporting binding spec"))

; spec -> (void) or raised syntax error
; enforces the above grammar for an unscoped expression
(define (check-order/unscoped-expression spec)
  (let refs+subexps ([spec spec])
    (match spec
      [(group _ specs) (map refs+subexps specs)]
      [(s* ellipsis [spec s])
       (refs+subexps s)]
      [(or (s* ref) (s* suspend)) (void)]
      [(and (or (s* bind) (s* bind-syntax) (s* bind-syntaxes)) (with-stx stx))
       (binding-scope-error stx)]
      [(or (and (s* import) (with-stx stx))
           (imports _ (cons (with-stx stx) _)))
       ; TODO use imports stx once it's sorce location is more refined.
       (wrong-syntax/orig stx "import binding groups must occur within a scope")]
      [(imports _ (list))
       ; impossible
       (void)]
      [(and (or (s* export) (s* export-syntax) (s* export-syntaxes)) (with-stx stx))
       (export-context-error stx)]
      [(and (s* re-export) (with-stx stx))
       (re-export-context-error stx)]
      [(or (s* nest [spec s])
           (s* nest-one [spec s]))
       (check-order/unscoped-expression s)]
      [(s* scope [spec s])
       (check-order/scoped-expression s)])))

#;(BSpec -> void?)
; enforces the above grammar for a scoped expression
(define (check-order/scoped-expression spec)
  (define (bindings spec specs)
    (match spec
      [(s* ellipsis [spec s])
       (bindings s specs)]
      [(group _ (cons group-spec group-specs))
       (bindings group-spec (append group-specs specs))]
      [(group _ (list)) (check-sequence bindings specs)]
      [(or (s* bind) (s* bind-syntax) (s* bind-syntaxes))
       (check-sequence bindings specs)]
      [(and (or (s* export) (s* export-syntax) (s* export-syntaxes)) (with-stx stx))
       (export-context-error stx)]
      [(and (s* re-export) (with-stx stx))
       (re-export-context-error stx)]
      [(or (s* import) (s* imports))
      ; assumes imports only contains imports, groups, and ellipses of imports by now
       (check-sequence refs+subexps specs)]
      [_ (check-sequence refs+subexps (cons spec specs))]))

  (define (refs+subexps spec specs)
    (match spec
      [(s* ellipsis [spec s])
       (refs+subexps s specs)]
      [(group _ (cons group-spec group-specs))
       ; inline flatten
       (refs+subexps group-spec (append group-specs specs))]
      [(group _ (list)) (check-sequence refs+subexps specs)]
      [(and (or (s* bind) (s* bind-syntax) (s* bind-syntaxes)) (with-stx stx))
       (wrong-syntax/orig stx "bindings must appear first within a scope")]
      [(and (or (s* export) (s* export-syntax) (s* export-syntaxes)) (with-stx stx))
       (export-context-error stx)]
      [(and (s* re-export) (with-stx stx))
       (re-export-context-error stx)]
      [(or (and (s* import) (with-stx stx))
           (imports _ (cons (with-stx stx) _)))
       (wrong-syntax/orig stx "an import binding group must appear before references and subexpressions")]
      [(imports _ (list))
       ; impossible
       (void)]
      [(or (s* ref) (s* suspend))
       (check-sequence refs+subexps specs)]
      [(or (s* nest [spec s])
           (s* nest-one [spec s]))
       (check-order/unscoped-expression s)
       (check-sequence refs+subexps specs)]
      [(s* scope [spec s])
       (check-order/scoped-expression s)
       (check-sequence refs+subexps specs)]))

  (check-sequence bindings (flat-bspec-top-elements spec)))

(define (check-order/exporting spec)
  (define (exports spec specs)
    (match spec
      [(s* ellipsis [spec s])
       (exports s specs)]
      [(group _ (cons group-spec group-specs))
       (exports group-spec (append group-specs specs))]
      [(group _ (list)) (check-sequence exports specs)]
      [(or (s* export) (s* export-syntax) (s* export-syntaxes))
       (check-sequence exports specs)]
      [_ (check-sequence re-exports (cons spec specs))]))

  (define (re-exports spec specs)
    (match spec
      [(s* ellipsis [spec s])
       (re-exports s specs)]
      [(group _ (cons group-spec group-specs))
       (re-exports group-spec (append group-specs specs))]
      [(group _ (list)) (check-sequence re-exports specs)]
      [(s* re-export)
       (check-sequence re-exports specs)]
      [_
       (check-sequence refs+subexps (cons spec specs))]))
  
  (define (refs+subexps spec specs)
    (match spec
      [(s* ellipsis [spec s])
       (refs+subexps s specs)]
      [(group _ (cons group-spec group-specs))
       (refs+subexps group-spec (append group-specs specs))]
      [(group _ (list)) (check-sequence refs+subexps specs)]
      [(and (or (s* bind) (s* bind-syntax) (s* bind-syntaxes)) (with-stx stx))
       (binding-scope-error stx)]
      [(and (or (s* export) (s* export-syntax) (s* export-syntaxes)) (with-stx stx))
       (wrong-syntax/orig stx "exports must appear first in a exporting spec")]
      [(and (s* re-export) (with-stx stx))
       (wrong-syntax/orig stx "re-exports must occur before references and subexpressions")]
      [(or (and (s* import) (with-stx stx))
           (imports _ (cons (and (s* import) (with-stx stx)) _)))
       (wrong-syntax/orig stx "import must occur within a scope")]
      [(imports _ (list))
       ; impossible
       (void)]
      [(or (s* ref) (s* suspend))
       (check-sequence refs+subexps specs)]
      [(or (s* nest [spec s])
           (s* nest-one [spec s]))
       (check-order/unscoped-expression s)
       (check-sequence refs+subexps specs)]
      [(s* scope [spec s])
       (check-order/scoped-expression s)
       (check-sequence refs+subexps specs)]))

  (check-sequence exports (flat-bspec-top-elements spec)))

(define (invariant-error who)
  (error who "should be caught by check-order functions"))

(define (compile-bspec-term/single-pass spec)
  (match spec
    [(ref _ (pvar v info))
     (match info
       [(nonterm-rep (simple-nonterm-info exp-proc))
        #`(subexp '#,v #,exp-proc)]
       [(bindclass-rep description _ pred space)
        #`(group (list (ref '#,v '#,space #,pred #,(string-append "not bound as " description)) (rename-ref '#,v '#,space)))]
       [(nested-binding)
        #`(nested)]
       [(nonterm-rep (nesting-nonterm-info _))
        (wrong-syntax/orig v "nesting nonterminals may only be used with `nest`")]
       [(nonterm-rep (exporting-nonterm-info _ _))
        (wrong-syntax/orig v "exporting nonterminals may only be used with `import` and `re-export`")]
       [(or (? stxclass-rep?) (? special-syntax-class-binding?))
        #`(group (list))])]
    [(suspend _ (pvar v info))
     #`(suspend '#,v)]
    [(bind _ (pvar v (bindclass-rep _ constr _ space)))
     #`(group (list (bind '#,v '#,space #'#,constr) (rename-bind '#,v '#,space)))]
    [(bind-syntax _ (pvar v (extclass-rep constr _ _ space)) (pvar v-transformer _))
     #`(group (list (bind-syntax '#,v '#,space #'#,constr '#,v-transformer) (rename-bind '#,v '#,space)))]
    [(bind-syntaxes _ depth (pvar v (extclass-rep constr _ _ space)) (pvar v-transformer _))
     #`(group (list (bind-syntaxes '#,v '#,space #'#,constr '#,v-transformer) (rename-bind '#,v '#,space)))]
    [(imports _ ss)
     (define/syntax-parse (s-cp1 ...) (map compile-import/pass1 ss))
     (define/syntax-parse (s-cp2 ...) (map compile-import/pass2 ss))
     #'(group (list s-cp1 ... s-cp2 ...))]
    [(import _ _)
     (error "shouldn't encounter import")]
    [(or (s* export) (s* export-syntax) (s* export-syntaxes))
     (invariant-error 'compile-bspec-term/single-pass)]
    [(re-export _ (pvar v _))
     (invariant-error 'compile-bspec-term/single-pass)]
    [(nest _ depth (pvar v info) spec)
     (match info
       [(nonterm-rep (nesting-nonterm-info expander))
        (with-syntax ([spec-c (compile-bspec-term/single-pass spec)])
          #`(nest '#,v #,expander spec-c))])]
    [(nest-one _ (pvar v info) spec)
     (match info
       [(nonterm-rep (nesting-nonterm-info expander))
        (with-syntax ([spec-c (compile-bspec-term/single-pass spec)])
          #`(nest-one '#,v #,expander spec-c))])]
    [(scope _ spec)
     (with-syntax ([spec-c (compile-bspec-term/single-pass spec)])
       #'(scope spec-c))]
    [(group _ specs)
     (with-syntax ([(spec-c ...) (map compile-bspec-term/single-pass specs)])
       #'(group (list spec-c ...)))]
    [(ellipsis _ spec)
     (define vs (bspec-referenced-pvars spec))
     (with-syntax ([spec-c (compile-bspec-term/single-pass spec)])
       #`(ellipsis '#,vs spec-c))]))

; BSpec -> Syntax
; spec can be groups, ellipses, and imports
(define (compile-import/pass1 spec)
  (match spec
    [(import _ pv)
     (match pv
       [(pvar v (nonterm-rep (exporting-nonterm-info pass1-expander _)))
        #`(subexp '#,v #,pass1-expander)])]
    [(ellipsis _ spec)
     (define vs (bspec-referenced-pvars spec))
     (with-syntax ([spec-c (compile-import/pass1 spec)])
       #`(ellipsis '#,vs spec-c))]
    [(or (imports _ specs) (group _ specs))
     (with-syntax ([(spec-c ...) (map compile-import/pass1 specs)])
       #'(group (list spec-c ...)))]
    [_ (error "unexpected specs in imports")]))

; BSpec -> Syntax
; spec can be groups, ellipses, and imports
(define (compile-import/pass2 spec)
  (match spec
    [(import _ pv)
     (match pv
       [(pvar v (nonterm-rep (exporting-nonterm-info _ pass2-expander)))
        ;; avoid adding the local-scopes to syntax moved in by first pass expansion
        #`(subexp/no-scope '#,v #,pass2-expander)])]
    [(ellipsis _ spec)
     (define vs (bspec-referenced-pvars spec))
     (with-syntax ([spec-c (compile-import/pass2 spec)])
       #`(ellipsis '#,vs spec-c))]
    [(or (imports _ specs) (group _ specs))
     (with-syntax ([(spec-c ...) (map compile-import/pass2 specs)])
       #'(group (list spec-c ...)))]
    [_ (error "unexpected specs in imports")]))

(define no-op #'(group (list)))

(define (compile-bspec-term/pass1 spec)
  (match spec
    [(or (s* bind) (s* bind-syntax) (s* bind-syntaxes))
     (invariant-error 'compile-bspec-term/pass1)]
    [(group _ specs)
     (with-syntax ([(spec-c ...) (map compile-bspec-term/pass1 specs)])
       #'(group (list spec-c ...)))]
    
    [(or (ref _ _)
         (nest _ _ _ _)
         (nest-one _ _ _)
         (scope _ _)
         (suspend _ _))
     no-op]
    
    [(export _ (pvar v (bindclass-rep _ constr _ space)))
     #`(group (list (bind '#,v '#,space #'#,constr) (rename-bind '#,v '#,space)))]
    [(export-syntax _ (pvar v (extclass-rep constr _ _ space)) (pvar v-transformer _))
     #`(group (list (bind-syntax '#,v '#,space #'#,constr '#,v-transformer) (rename-bind '#,v '#,space)))]
    [(export-syntaxes _ depth (pvar v (extclass-rep constr _ _ space)) (pvar v-transformer _))
     #`(group (list (bind-syntaxes '#,v '#,space #'#,constr '#,v-transformer) (rename-bind '#,v '#,space)))]
    [(re-export _ pv)
     (match-define (pvar v (nonterm-rep (exporting-nonterm-info pass1-expander _))) pv)
     #`(subexp '#,v #,pass1-expander)]
    [(ellipsis _ spec)
     (define vs (bspec-referenced-pvars spec))
     (with-syntax ([spec-c (compile-bspec-term/pass1 spec)])
       #`(ellipsis '#,vs spec-c))]))

(define (compile-bspec-term/pass2 spec)
  (match spec
    [(or (s* bind) (s* bind-syntax) (s* bind-syntaxes))
     (invariant-error 'compile-bspec-term/pass2)]
    [(group _ specs)
     (with-syntax ([(spec-c ...) (map compile-bspec-term/pass2 specs)])
       #'(group (list spec-c ...)))]

    [(or (ref _ _)
         (nest _ _ _ _)
         (nest-one _ _ _)
         (scope _ _)
         (suspend _ _))
     (compile-bspec-term/single-pass spec)]

    [(or (s* export) (s* export-syntax) (s* export-syntaxes))
     no-op]
    [(re-export _ pv)
     (match-define (pvar v (nonterm-rep (exporting-nonterm-info _ pass2-expander))) pv)
     ;; avoid adding the local-scopes to syntax moved in by first pass expansion
     #`(subexp/no-scope '#,v #,pass2-expander)]
    [(ellipsis _ spec)
     (define vs (bspec-referenced-pvars spec))
     (with-syntax ([spec-c (compile-bspec-term/pass2 spec)])
       #`(ellipsis '#,vs spec-c))]))
