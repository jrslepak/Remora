#lang racket

(require rackunit
         redex
         "language.rkt"
         "typed-lang.rkt")

(define-extended-language Dependent Arrays
  ; need to put dependent λ somewhere
  (expr ....
        (expr idx ...))
  
  ; for array syntax, already-present shape vector works as constructor index
  ; allow arrays of types and isorts? or are arrays strictly value-level?
  
  ; have to include type annotations in λ syntax
  (fun (λ [(var num type) ...] expr)
       op
       c-op)
  
  (E ....
     (E idx ...))
  
  ; type-level pieces
  (type (∏ [(var isort) ...] type)
        (∑ [(var isort) ...] type)
        ; in Dependent ML's style of typing, functions are not dependent products
        (type ... -> type)
        (× type ...) ; product types may be needed later
        (Array idx elt-type)
        var)
  (elt-type btype type)
  ; base types
  (btype Num Bool)
  
  ; sort-level pieces
  ; constraint domain would probably be something like nat lists
  (isort Nat
         Shape)
  (idx natural
       (S idx ...)
       ; sort-level computation to determine output shape
       ; handled via metafunction once actual indices known
       (frame [idx idx] ...)
       var)
  
  ; extra machinery for type checking
  (t-env (t-bind ...))
  (t-bind [var type])
  (i-env (i-bind ...))
  (i-bind [var isort]))

; type check an expression in the dependently-typed version of the language
(define-judgment-form Dependent
  #:contract (type-of i-env t-env expr type)
  #:mode (type-of I I I O)
  ; array: find element type, check for correct number of elements
  [(type-of/elt i-env t-env (el-expr ...) elt-type)
   (side-condition (size-check (A (natural ...) (el-expr ...))))
   --- array
   (type-of i-env t-env
            (A (natural ...) (el-expr ...))
            (Array (S natural ...) elt-type))]
  ; variable: grab from environment (not there -> ill-typed)
  [; should probably change this to have premise which calls type env lookup
   --- variable
   (type-of i-env ([var_0 type_0] ... [var_1 type_1] [var_2 type_2] ...)
            var_1 type_1)]
  ; primitive operator: call out to metafunction?
  [(where type (primop-type op))
   --- operator
   (type-of i-env t-env op type)]
  ; λ abstraction: add inputs to type environment, check body
  ; function app: check that args are equivalent to what function expects
  ; index abstraction: 
  ; index app: 
  )

; separate judgment needed for checking array element types
; they must all have equivalent types (this will require that equivalent
; types be reducible to some canonical form)
(define-judgment-form Dependent
  #:contract (type-of/elt i-env t-env (el-expr ...) elt-type)
  #:mode (type-of/elt I I I O)
  ; simple cases are base types
  ; TODO: does it ever make sense to have vars of type Num or Bool?
  ; maybe not, but it might make sense to have vars in an array, so have to
  ; handle arbitrary types on the inside anyway
  [--- num-elt
       (type-of/elt i-env t-env (num ...) Num)]
  [--- bool-elt
       (type-of/elt i-env t-env (bool ...) Bool)]
  ; odd case is array-of-arrays, then look at elements as exprs
  )
  

; check whether an array has as many elements as its shape says it should have
(define-metafunction Dependent
  size-check : arr -> bool
  [(size-check (A (num ...) (el-expr ...))) ,(= (foldr * 1 (term (num ...)))
                                                (length (term (el-expr ...))))])

(define-metafunction Dependent
  primop-type : fun -> type
  [(primop-type +) (∏ [(s1 Shape) (s2 Shape)]
                      ((Array s1 Num)
                       (Array s2 Num)
                       -> (Array (frame [s1 0] [s2 0]) Num)))])




(module+
 test
 
 ; array of numbers
 (check-equal? (judgment-holds (type-of () () (A (3 2) (4 1 6 2 3 5)) type)
                               [type])
               (term (((Array (S 3 2) Num)))))
 ; array of booleans
 (check-equal? (judgment-holds (type-of () () (A (2) (#f #t)) type)
                               [type])
               (term (((Array (S 2) Bool)))))
 
 
 ; array with wrong number of elements
 (check-equal? (judgment-holds (type-of () () (A (3 2) (4 1 6 2 3)) type)
                               [type])
               '()))
