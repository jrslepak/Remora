#lang racket

(require rackunit
         redex
         "language.rkt"
         "typed-lang.rkt")

(define-extended-language Dependent Arrays
  ; need to put dependent λ and syntax for ∑-related things somewhere
  ; may also want to add a type annotation to array syntax
  ; (A type (num ...) (el-expr ...)), etc.
  (expr ....
        (INDEX expr idx ...))
  
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
        (Array idx type)
        var
        base-type)
  (base-type Num Bool)
  
  ; sort-level pieces
  ; constraint domain would probably be something like nat lists
  (sort Nat
        Shape)
  (idx natural
       (S idx ...)
       ; sort-level computation to determine output shape
       ; handled via metafunction once actual indices known
       ; we require [shape naturalized-rank] pairs
       ; TODO: probably going to need more operations on shapes eventually
       (frame [idx idx] ...)
       var)
  
  ; this feels hackish, but the type/index substitution seems to need it
  (type/idx type idx)
  
  ; extra machinery for type checking and substitution purposes
  (expr-env (e-bind ...))
  (e-bind [var expr])
  (idx-env (i-bind ...))
  (i-bind [var idx])
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
  [(type-of/elts sort-env type-env (el-expr ...) type)
   (side-condition (size-check (A (natural ...) (el-expr ...))))
   --- array
   (type-of sort-env type-env
            (A (natural ...) (el-expr ...))
            (Array (S natural ...) type))]
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
  [(type-of sort-env (type-env-update [var type_arg] ... type-env)
            expr type_body)
   --- lambda
   (type-of sort-env type-env
            (λ [(var num type_arg) ...] expr)
            (type_arg ... -> type_body))]
  ; function app: check that args are equivalent to what function expects
  [; first, identify function's and args' types (need a handle on their shapes)
   ; TODO: what about nested array of functions?
   (type-of sort-env type-env expr_fun type_fun)
   (type-of sort-env type-env expr_arg type_arg)
   ...
   ; get canonical types for the function & argument arrays
   (where (Array idx_fun-shape (type_input0 ... -> type_output))
          (canonicalize-type type_fun))
   (where [(Array idx_arg-shape type_arg-elt) ...]
          [(canonicalize-type type_arg) ...])
   ; extract expected rank of function array's elements
   (where (natural_arg-rank ...)
          (type->arg-rank (type_input0 ... -> type_output)))
   ; determine the frame of the implicit `apply'
   ; it's the approrpriate-sized prefix of the shape with the highest overrank
   (where (natural_app-rank ...) (0 natural_arg-rank ...))
   (where (natural_term-rank ...)
          ((shape->rank idx_fun-shape) (shape->rank idx_arg-shape) ...))
   (where (S natural_app-frame ...)
          (frame-shape (natural_app-rank ...)
                       (idx_fun-shape idx_arg-shape ...)))
   ; make sure all terms can lift into this frame, i.e. each term's individual
   ; frame shape is a prefix of apply's frame shape
   ; this requires dropping the cell parts of all terms' shapes
   (side-condition
    (prefix-agree? (S natural_app-frame ...)
                   ; non-cell parts of terms' shapes
                   ((exclude-cell 0 idx_fun-shape)
                    (exclude-cell natural_arg-rank idx_arg-shape) ...)))
   --- fun-app
   (type-of sort-env type-env
            (expr_fun expr_arg ...)
            (Array (S natural_app-frame ...) type_output)
            #;(× (Array (S natural_app-frame ...) type_output)
               
               (Array (S natural_app-rank ...) Bool)
               (Array idx_fun-shape Bool)
               (Array idx_arg-shape (Array (S natural_arg-rank) Bool)) ...))]
  #;[(type-of sort-env type-env expr_fun (type_input0 ... -> type_output))
   (type-of sort-env type-env expr_arg type_input1) ...
   (equiv-type sort-env type-env type_input0 type_input1) ...
   --- fun-app
   (type-of sort-env type-env
            (expr_fun expr_arg ...)
            type_output)]
  ; index abstraction: 
  ; index app: check that indices have proper sort, substitute indices into type
  [(type-of sort-env type-env expr (∏ ([var sort] ...) type))
   (sort-of sort-env type-env idx sort) ...
   --- idx-app
   (type-of sort-env type-env (INDEX expr idx ...)
            (index-sub ([var idx] ...) type))]
  )

(define-judgment-form Dependent
  #:contract (sort-of sort-env type-env idx sort)
  #:mode (sort-of I I I O)
  [#;???
   --- sort-nat
   (sort-of sort-env type-env natural Nat)]
  [(sort-of sort-env type-env idx Nat) ...
   --- sort-shape
   (sort-of sort-env type-env (S idx ...) Shape)]
  [(sort-of sort-env type-env idx_shape Shape) ...
   (sort-of sort-env type-env idx_rank Nat) ...
   ; TODO: will need a separate metafunction for this
   ; may also need to track actual index values
   ; or is that left to constraint generation?
   #;(side-condition (valid-shape (frame [idx_shape idx_rank] ...)))
   --- sort-frame
   (sort-of sort-env type-env (frame [idx_shape idx_rank] ...) Shape)]
  )

; separate judgment needed for checking array element types
; they must all have equivalent types (this will require that equivalent
; types be reducible to some canonical form)
(define-judgment-form Dependent
  #:contract (type-of/elts sort-env type-env (el-expr ...) type)
  #:mode (type-of/elts I I I O)
  ; empty arrays will need either annotation or special casing
  [(type-of sort-env type-env expr_0 type_0)
   (type-of sort-env type-env expr_1 type_1) ...
   ; TODO: metafunction or judgment form?
   ; judgment form will probably need auxiliary metafunctions anyway
   ; - evaluate (frame _) forms
   ; - collapse nested arrays
   (side-condition (equivalent-types type_0 type_1 ...))
   --- ???
   (type-of/elts sort-env type-env (expr_0 expr_1 ...) type_0)]
  ; simple cases are base types
  ; does it ever make sense to have vars of type Num or Bool?
  ; maybe not, but it might make sense to have vars in an array, so have to
  ; handle arbitrary types on the inside anyway
  [--- num-elt
       (type-of/elts sort-env type-env (num ...) Num)]
  [--- bool-elt
       (type-of/elts sort-env type-env (bool ...) Bool)]
  ; odd case is array-of-arrays, then look at elements as exprs
  )

; judgment forms for checking type equivalence
; instead of a general term equivalence, the restricted dependent typing allows
; a much simpler "index equivalence" judgment
(define-judgment-form Dependent
  #:contract (equiv-type sort-env type-env type type)
  #:mode (equiv-type I I I I)
  [#;(well-formed sort-env type-env type)
   ; is a well-formedness check needed?
   --- teq-refl
   (equiv-type sort-env type-env type type)]
  ; basics of congruence
  [(equiv-type sort-env type-env type_0 type_1)
   (equiv-idx sort-env type-env idx_0 idx_1)
   --- teq-array
   (equiv-type sort-env type-env
               (Array idx_0 type_0)
               (Array idx_1 type_1))]
  [(equiv-type sort-env type-env type_arg0 type_arg1) ...
   (equiv-type sort-env type-env type_res0 type_res1)
   --- teq-fun
   (equiv-type sort-env type-env
               (type_arg0 ... -> type_res0)
               (type_arg1 ... -> type_res1))]
  [(equiv-type sort-env type-env type_0 type_1) ...
   --- teq-tuple
   (equiv-type sort-env type-env
               (× type_0 ...)
               (× type_1 ...))]
  
  ; canonicalization rules
  ; collapse nested arrays
  [--- equiv-nest1
   (equiv-type sort-env type-env
               (Array (S idx_0 ...) (Array (S idx_1 ...) type))
               (Array (S idx_0 ... idx_1 ...) type))]
  [--- equiv-nest2
   (equiv-type sort-env type-env
               (Array (S idx_0 ... idx_1 ...) type)
               (Array (S idx_0 ...) (Array (S idx_1 ...) type)))]
  )

(define-judgment-form Dependent
  #:contract (equiv-idx sort-env type-env idx idx)
  #:mode (equiv-idx I I I I)
  [--- ieq-refl
   (equiv-idx sort-env type-env idx idx)]
  ; TODO: frame
  )


; check whether an array has as many elements as its shape says it should have
(define-metafunction Dependent
  size-check : arr -> bool
  [(size-check (A (num ...) (el-expr ...))) ,(= (foldr * 1 (term (num ...)))
                                                (length (term (el-expr ...))))])

; determine the frame shape associated with a lifting instance
; TODO: check for prefix agreement
;  should reject (term (frame-shape (frame [0 (S 3)] [0 (S 2)])))
(define-metafunction Dependent
  frame-shape : (natural ...) (idx ...) -> idx
  [(frame-shape (natural_rank ...) (idx ...))
   (S natural_frame-dim ...)
   ; how overranked is each term?
   (where (natural_over ...)
          ((-/m (shape->rank idx) natural_rank) ...))
   ; position of highest overrank?
   (where (natural_overrank (S natural_dim ...))
          ,(argmax first (term ([natural_over idx] ...))))
   ; extract prefix
   (where (natural_frame-dim ...) (take/m (natural_dim ...) natural_overrank))
   ; check that extracted prefix is a prefix of the other shapes
   ;(side-condition (term (prefix-agree? 
   ])

; determine whether a shape is a prefix of other shapes
(define-metafunction Dependent
  prefix-agree? : idx (idx ...) -> bool
  [(prefix-agree? idx ()) #t]
  [(prefix-agree? (S natural_prefix ...) ((S natural_shape ...) idx ...))
   (prefix-agree? (S natural_prefix ...) (idx ...))
   (side-condition (term (prefix? (natural_shape ...) (natural_prefix ...))))]
  ; if side-condition not met, we have a mismatch
  [(prefix-agree? (S natural_prefix ...) ((S natural_shape ...) idx ...)) #f])

; find the arg rank associated with a function type
(define-metafunction Dependent
  type->arg-rank : type -> (natural ...)
  [(type->arg-rank (type_arg ... -> type_result)) ((type->rank type_arg) ...)])
; find the rank associated with an array type
; TODO: is var lookup capability needed here?
(define-metafunction Dependent
  type->rank : type -> natural
  ; collapse nested array types
  #;[(type->rank (Array (S idx_outer ...) (Array (S idx_inner ...) type)))
   ,(+ (length (term (idx_outer ...)))
       (length (term (idx_inner ...)))
       (term (type->rank type)))]
  [(type->rank (Array (S idx ...) type))
   ,(+ (length (term (idx ...))) (term (type->rank type)))]
  ; for now, pretending all other possible element types are scalar
  ; more detailed handling of ∑ types might be awkward/impossible
  [(type->rank type) 0])

; find the rank associated with a given shape
; TODO: add clauses for index-level computation forms
(define-metafunction Dependent
  shape->rank : idx -> natural
  [(shape->rank (S natural ...)) ,(length (term (natural ...)))])

; drop the part of a shape designated as the cell
(define-metafunction Dependent
  exclude-cell : natural idx -> idx
  [(exclude-cell natural_rank (S natural_dim ...))
   (S natural_frame-dim ...)
   (where (natural_frame-dim ...)
          (drop-right/m (natural_dim ...) natural_rank))])

(define-metafunction Dependent
  primop-type : fun -> type
  [(primop-type +) (∏ [(s1 Shape) (s2 Shape)]
                      ((Array s1 Num)
                       (Array s2 Num)
                       -> (Array (frame [s1 0] [s2 0]) Num)))])


; substitute indices into a type
(define-metafunction Dependent
  index-sub : idx-env type/idx -> type/idx
  [(index-sub ([var_0 idx_0] ... [var_1 idx_1] [var_2 idx_2] ...) var_1)
   idx_1
   (side-condition (not (member (term var_1) (term (var_0 ...)))))]
  ; substituting in types
  [(index-sub idx-env base-type) base-type]
  [(index-sub idx-env (Array idx type))
   (Array (index-sub idx-env idx) (index-sub idx-env type))]
  [(index-sub idx-env (type_arg ... -> type_result))
   ((index-sub idx-env type_arg) ... -> (index-sub idx-env type_result))]
  [(index-sub idx-env (× type ...))
   (× (index-sub idx-env type) ...)]
  ; TODO: shadowing for ∏ and ∑ types
  [(index-sub idx-env (∏ [(var sort) ...] type))
   (∏ [(var sort) ...] (index-sub idx-env_shadowed type))
   (where idx-env_shadowed (shadow (var ...) idx-env))]
  ; substituting in indices
  [(index-sub idx-env natural) natural]
  [(index-sub idx-env (S idx ...)) (S (index-sub idx-env idx) ...)]
  [(index-sub idx-env (frame [idx_shape idx_rank] ...))
   (frame [(index-sub idx-env idx_shape) (index-sub idx-env idx_rank)] ...)])


; reduce a type to canonical form:
; base types, dependent products, dependent sums, tuples of canonical types,
; functions on canonical types, arrays of canonical non-array types
(define-metafunction Dependent
  canonicalize-type : type -> type
  ; some types are already in canonical form
  [(canonicalize-type base-type) base-type]
  ; no reducing inside the body (TODO: actually, it's probably safe to do so)
  [(canonicalize-type (∏ [(var sort) ...] type)) (∏ [(var sort) ...] type)]
  [(canonicalize-type (∑ [(var sort) ...] type)) (∑ [(var sort) ...] type)]
  ; simple structural recursion for most other cases
  [(canonicalize-type (× type ...)) (× (canonicalize-type type) ...)]
  [(canonicalize-type (type_arg ... -> type_result))
   ((canonicalize-type type_arg) ... -> (canonicalize-type type_result))]
  ; the interesting cases are for nested arrays
  ; make sure the element type isn't another array
  [(canonicalize-type (Array idx type))
   (Array (canonicalize-index idx) (canonicalize-type type))
   ; sometimes, Redex has funny binding structure
   (side-condition (not (redex-match Dependent (Array idx type) (term type))))]
  ; if it is another array, we need to collapse the nested array type into
  ; a flat array type
  [(canonicalize-type (Array idx type))
   ; may need multiple steps of reduction
   (canonicalize-type (Array (nest-shape idx idx_cell) type_elt))
   (where (Array idx_cell type_elt) type)]
  )
; reduce an index to canonical form
(define-metafunction Dependent
  canonicalize-index : idx -> idx
  ; naturals and shape literals are already in canonical form
  [(canonicalize-index natural) natural]
  [(canonicalize-index (S natural ...)) (S natural ...)]
  ; index-level computation
  [(canonicalize-index (frame [idx_rank idx_shape] ...))
   (frame-shape [(canonicalize-index idx_rank) ...]
                [(canonicalize-index idx_shape) ...])])
; find the shape that comes from nesting one shape inside another
(define-metafunction Dependent
  nest-shape : idx idx ... -> idx
  [(nest-shape idx) idx]
  [(nest-shape (S natural_frame ...) (S natural_cell ...) idx ...)
   (nest-shape (S natural_frame ... natural_cell ...) idx ...)])
   
  
  
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
   (side-condition (not (term (sort-env-lookup var (s-bind_0 ...)))))]
  [(sort-env-lookup var sort-env) #f])

(define-metafunction Dependent
  type-env-update : t-bind ... type-env -> type-env
  ; if no changes, return the environment as-is
  [(type-env-update type-env) type-env]
  ; if multiple changes, apply each individually
  [(type-env-update t-bind_0 t-bind_1 t-bind_2 ... type-env)
   (type-env-update t-bind_1 t-bind_2 ...
                    (type-env-update t-bind_0 type-env))]
  ; eliminate any already-present occurrences of the variable in the environment
  [(type-env-update [var type_new] (t-bind_0 ... [var type_old] t-bind_1 ...))
   (type-env-update [var type_new] (t-bind_0 ... t-bind_1 ...))]
  ; once they're all gone, put in the new entry
  [(type-env-update [var_new type_new] ([var_old type_old] ...))
   ([var_new type_new] [var_old type_old] ...)])

(define-metafunction Dependent
  sort-env-update : s-bind ... sort-env -> sort-env
  ; if no changes, return the environment as-is
  [(sort-env-update sort-env) sort-env]
  ; if multiple changes, apply each individually
  [(sort-env-update s-bind_0 s-bind_1 s-bind_2 ... sort-env)
   (sort-env-update s-bind_1 s-bind_2 ...
                    (sort-env-update s-bind_0 sort-env))]
  ; eliminate any already-present occurrences of the variable in the environment
  [(sort-env-update [var sort_new] (s-bind_0 ... [var sort_old] s-bind_1 ...))
   (sort-env-update [var sort_new] (s-bind_0 ... s-bind_1 ...))]
  ; once they're all gone, put in the new entry
  [(sort-env-update [var_new sort_new] ([var_old sort_old] ...))
   ([var_new sort_new] [var_old sort_old] ...)])

; remove the named entries from an environment
; var bound to any so this can be used on any form of binding list
(define-metafunction Dependent
  shadow : (var ...) [(var any) ...] -> [(var any) ...]
  [(shadow () [(var any) ...]) [(var any) ...]]
  [(shadow (var ...) []) []]
  [(shadow (var_0 var_1 ...)
                       [(var_2 sort_2) ... (var_0 sort_0) (var_3 sort_3) ...])
   (shadow (var_1 ...)
                       [(var_2 sort_2) ... (var_3 sort_3) ...])])


(define-metafunction Dependent
  -/m : number ... -> number
  [(-/m number ...) ,(apply - (term (number ...)))])
(define-metafunction Dependent
  argmax/m : (number ...) -> number
  [(argmax/m (number ...)) ,(argmax (λ (x) x) (term (number ...)))])


(module+
 test
 
 ;-------------------
 ; expression typing judgment
 ;-------------------
 ; array of numbers
 (check-equal? (judgment-holds (type-of () () (A (3 2) (4 1 6 2 3 5)) type)
                               type)
               (term ((Array (S 3 2) Num))))
 
 ; array of booleans
 (check-equal? (judgment-holds (type-of () () (A (2) (#f #t)) type)
                               type)
               (term ((Array (S 2) Bool))))
 
 ; array with wrong number of elements
 (check-equal? (judgment-holds (type-of () () (A (3 2) (4 1 6 2 3)) type)
                               type)
               '())
 
 ; array of arrays
 (check-equal? (judgment-holds (type-of () ()
                                        (A (2) ((A (3) (1 2 3))
                                                (A (3) (4 5 6))))
                                        type)
                               type)
               (term ((Array (S 2) (Array (S 3) Num)))))
 
 ; simple λ term
 (check-equal?
  (judgment-holds
   (type-of () () (λ ([x 0 (Array (S) Num)]) (A () (3))) type)
   type)
  (term (((Array (S) Num) -> (Array (S) Num)))))
 
 ; multiargument λ term
 (check-equal?
  (judgment-holds
   (type-of () () (λ ([x 0 (Array (S 3) Num)]
                      [y 0 (Array (S 1) Bool)]) (A () (3))) type)
   type)
  (term (((Array (S 3) Num) (Array (S 1) Bool) -> (Array (S) Num)))))
 
 ; applying the simple λ term
 (check-equal?
  (judgment-holds
   (type-of () () ((A () ((λ ([x 0 (Array (S) Num)]) (A () (3)))))
                   (A () (4)))
            type)
   type)
  (term ((Array (S) (Array (S) Num)))))
 
 ; applying the multiargument λ term
 (check-equal?
  (judgment-holds
   (type-of () () ((A () ((λ ([x 0 (Array (S 3) Num)]
                              [y 0 (Array (S 1) Bool)])
                            (A () (3)))))
                   (A (3) (1 2 3)) (A (1) (#f))) type)
   type)
  (term ((Array (S) (Array (S) Num)))))
 
 ; applying to arg with equivalent-but-not-identical type
 (check-equal?
  (judgment-holds
   (type-of () () ((A () ((λ ([x 2 (Array (S 2 3) Num)]) x)))
                   (A (2) ((A (3) (1 2 3))
                           (A (3) (4 5 6)))))
            type)
   type)
  (term ((Array (S) (Array (S 2 3) Num)))))
 
 
 ; index application
 ; TODO: find some other function to use, as +'s type is now scalar->scalar
 #;(check-equal?
  (judgment-holds (type-of () () (INDEX + (S 3) (S)) type) type)
  (term (((Array (S 3) Num)
          (Array (S) Num)
          -> (Array (frame [(S 3) 0] [(S) 0]) Num)))))
 
 ; index application followed by function application
 #;(check-equal?
  (judgment-holds (type-of () () ((INDEX + (S 3) (S))
                                  (A (3) (1 2 3)) (A () (10))) type) type)
  (term ((Array (frame [(S 3) 0] [(S) 0]) Num))))
 
 ;-------------------
 ; utility metafunctions
 ;-------------------
 ; type environment manipulation
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
  (term ([y (Array (S) Num)] [x (Array (S 3) Num)])))
 (check-equal?
  (term (type-env-update [y (Array (S) Num)]
                         [z (Array (S 5) ((Array (S) Num) -> (Array (S) Num)))]
                         ([x (Array (S 3) Num)]
                          [y (Array (S 3) Bool)]
                          [y (Array (S 5 2) Bool)])))
  (term ([z (Array (S 5) ((Array (S) Num) -> (Array (S) Num)))]
         [y (Array (S) Num)]
         [x (Array (S 3) Num)])))
 
 ; sort environment manipulation
 (check-equal?
  (term (sort-env-lookup k ([k Shape]
                            [l Nat])))
  (term Shape))
 (check-equal?
  (term (sort-env-lookup k ([l Shape]
                            [k Nat])))
  (term Nat))
 (check-equal?
  (term (sort-env-lookup k ([k Shape]
                            [k Nat])))
  (term Shape))
 
 (check-equal?
  (term (sort-env-update [y Shape]
                         ([x Nat]
                          [y Nat]
                          [y Shape])))
  (term ([y Shape] [x Nat])))
 (check-equal?
  (term (sort-env-update [y Shape]
                         [z Nat]
                         ([x Nat]
                          [y Nat]
                          [y Shape])))
  (term ([z Nat] [y Shape] [x Nat]))))
