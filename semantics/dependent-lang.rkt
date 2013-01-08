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
  ; allow arrays of types and sorts? or are arrays strictly value-level?
  
  ; have to include type annotations in λ syntax
  (fun (λ [(var num type) ...] expr)
       op
       c-op)
  
  (E ....
     (E idx ...))
  
  ; type-level pieces
  (type (∏ [(var sort) ...] type)
        (∑ [(var sort) ...] type)
        ; in DML's style of typing, functions are not dependent products
        (type ... -> type)
        (× type ...) ; product types may be needed later
        (Array idx elt-type)
        var)
  (elt-type btype type)
  ; base types
  (btype Num Bool)
  
  ; sort-level pieces
  ; constraint domain would probably be something like nat lists
  (sort Nat
         Shape)
  (idx natural
       (S idx ...)
       ; sort-level computation to determine output shape
       ; handled via metafunction once actual indices known
       (frame [idx idx] ...)
       var)
  
  ; extra machinery for type checking
  (type-env (t-bind ...))
  (t-bind [var type])
  (sort-env (s-bind ...))
  (s-bind [var sort]))

; type check an expression (or single element expression) in the
; dependently-typed version of the language
(define-judgment-form Dependent
  #:contract (type-of sort-env type-env expr type)
  #:mode (type-of I I I O)
  ; array: find element type, check for correct number of elements
  [(type-of/elts sort-env type-env (el-expr ...) elt-type)
   (side-condition (size-check (A (natural ...) (el-expr ...))))
   --- array
   (type-of sort-env type-env
            (A (natural ...) (el-expr ...))
            (Array (S natural ...) elt-type))]
  ; variable: grab from environment (not there -> ill-typed)
  [; should probably change this to have premise which calls type env lookup
   --- variable
   (type-of sort-env ([var_0 type_0] ... [var_1 type_1] [var_2 type_2] ...)
            var_1 type_1)]
  ; primitive operator: call out to metafunction?
  [(where type (primop-type op))
   --- operator
   (type-of sort-env type-env op type)]
  ; λ abstraction: add inputs to type environment, check body
  ; function app: check that args are equivalent to what function expects
  ; index abstraction: 
  ; index app: 
  )

; separate judgment needed for checking array element types
; they must all have equivalent types (this will require that equivalent
; types be reducible to some canonical form)
(define-judgment-form Dependent
  #:contract (type-of/elts sort-env type-env (el-expr ...) elt-type)
  #:mode (type-of/elts I I I O)
  ; simple cases are base types
  ; TODO: does it ever make sense to have vars of type Num or Bool?
  ; maybe not, but it might make sense to have vars in an array, so have to
  ; handle arbitrary types on the inside anyway
  [--- num-elt
       (type-of/elts sort-env type-env (num ...) Num)]
  [--- bool-elt
       (type-of/elts sort-env type-env (bool ...) Bool)]
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


; type/sort manipulation metafunctions
(define-metafunction Dependent
  type-env-lookup : var type-env -> type or #f
  [(type-env-lookup var (t-bind_0 ... [var type] t-bind_1 ...))
   type
   (side-condition (not (term (type-env-lookup var (t-bind_0 ...)))))]
  [(type-env-lookup var type-env) #f])

(define-metafunction Dependent
  sort-env-lookup : var sort-env -> sort or #f
  [(sort-env-lookup var (s-bind_0 ... [var sort] s-bind_1 ...))
   sort
   (side-condition (not (term (type-env-lookup var (s-bind_0 ...)))))]
  [(sort-env-lookup var sort-env) #f])

(define-metafunction Dependent
  type-env-update : t-bind type-env -> type-env
  ; eliminate any already-present occurrences of the variable in the environment
  [(type-env-update [var type_new] (t-bind_0 ... [var type_old] t-bind_1 ...))
   (type-env-update [var type_new] (t-bind_0 ... t-bind_1 ...))]
  ; once they're all gone, put in the new entry
  [(type-env-update [var_new type_new] ([var_old type_old] ...))
   ([var_new type_new] [var_old type_old] ...)])

(define-metafunction Dependent
  sort-env-update : s-bind sort-env -> sort-env
  ; eliminate any already-present occurrences of the variable in the environment
  [(sort-env-update [var sort_new] (s-bind_0 ... [var_1 sort_old] s-bind_1 ...))
   (sort-env-update [var sort_new] (s-bind_0 ... s-bind_1 ...))]
  ; once they're all gone, put in the new entry
  [(sort-env-update [var_new sort_new] ([var_old sort_old] ...))
   ([var_new sort_new] [var_old sort_old] ...)])




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
               '())
 
 (check-equal?
  (term (type-env-lookup y ([y (Array (S 3) Num)]
                            [y (Array (S 3) Bool)])))
  (term (Array (S 3) Num)))
 (check-equal?
  (term (type-env-lookup y ([y (Array (S 3) Num)]
                            [y (Array (S 3) Bool)]
                            [y (Array (S 5 2) Bool)])))
  (term (Array (S 3) Num)))
 (check-equal?
  (term (type-env-lookup y ([x (Array (S 3) Num)]
                            [y (Array (S 3) Bool)]
                            [y (Array (S 5 2) Bool)])))
  (term (Array (S 3) Bool)))
 (check-equal?
  (term (type-env-update [y (Array (S) Num)]
                         ([x (Array (S 3) Num)]
                          [y (Array (S 3) Bool)]
                          [y (Array (S 5 2) Bool)])))
  (term ([y (Array (S) Num)] [x (Array (S 3) Num)]))))
