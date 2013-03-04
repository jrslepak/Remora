#lang racket

; TODO: make sure typing doesn't require things that are valid top-level
; exprs to be scalar-wrapped

(require rackunit
         redex
         "language.rkt"
         "typed-lang.rkt")

(define-extended-language Dependent Arrays
  ; need to put dependent λ and syntax for Σ-related things somewhere
  ; may also want to add a type annotation to array syntax
  ; (A type (num ...) (el-expr ...)), etc.
  (expr ....
        ; type abstraction (or should this be at `fun' level?)
        (Λ [var ...] expr)
        ; type application
        (TYPE expr type ...)
        ; construction of dependent sum
        (SUM idx ... expr type)
        ; can only get the abstracted thing -- the witness is an index
        (Σ-PROJ expr)
        ; index abstraction
        (ל [(var sort) ...] expr)
        ; index application
        (INDEX expr idx ...))
  
  ; for array syntax, already-present shape vector works as constructor index
  ; allow arrays of types and sorts? or are arrays strictly value-level?
  
  ; have to include type annotations in λ syntax
  ; TODO: with parametric polymorphism, is rank annotation still needed?
  (fun (λ [(var num type) ...] expr)
       op
       c-op)
  
  ; type-level pieces
  (type (Π [(var sort) ...] type)
        (Σ [(var sort) ...] type)
        ; in DML's style of typing, functions are not dependent products
        (type ... -> type)
        (× type ...) ; product types may be needed later
        (Array idx type)
        (∀ [var ...] type) ; no type-level computation -> there is only one kind
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
       ; addition, multiplication
       (frame [idx idx] ...)
       ; extract the `var' witness from dependent sum
       (Σ-WITNESS var expr)
       var)
  
  ; extra machinery for type checking and substitution purposes
  (expr-env (e-bind ...)) (e-bind [var expr])
  (idx-env (i-bind ...)) (i-bind [var idx])
  (type-env (t-bind ...)) (t-bind [var type])
  (kind-env (k-bind ...)) (k-bind [var ★]) ; may later need [var kind]
  (sort-env (s-bind ...)) (s-bind [var sort]))


; type check an expression (or single element expression) in the
; dependently-typed version of the language
; need a kind-env ::= (var ...), and have to check that type variables are bound
; before they are used (they can appear in λ, Λ, ל, and TYPE forms)
(define-judgment-form Dependent
  #:contract (type-of sort-env kind-env type-env el-expr type)
  #:mode (type-of I I I I O)
  ; array: find element type, check for correct number of elements
  [(side-condition (display "checking array"))
   (type-of/elts sort-env kind-env type-env (el-expr ...) type)
   (side-condition (size-check (A (natural ...) (el-expr ...))))
   --- array
   (type-of sort-env kind-env type-env
            (A (natural ...) (el-expr ...))
            (Array (S natural ...) type))]
  ; variable: grab from environment (not there -> ill-typed)
  [; should probably change this to have premise which calls type env lookup
   (side-condition (display "checking variable"))
   --- variable
   (type-of sort-env kind-env
            ([var_0 type_0] ... [var_1 type_1] [var_2 type_2] ...)
            var_1 type_1)]
  ; primitive operator: call out to metafunction
  [(side-condition (display "checking primop"))
   (where type (primop-type op))
   --- operator
   (type-of sort-env kind-env type-env op type)]
  ; λ abstraction: add inputs to type environment, check body
  [(side-condition (display "checking λ abstraction"))
   (kind-of sort-env kind-env type-env type_arg) ...
   (type-of sort-env kind-env (type-env-update [var type_arg] ... type-env)
            expr type_body)
   --- lambda
   (type-of sort-env kind-env type-env
            (λ [(var num type_arg) ...] expr)
            (type_arg ... -> type_body))]
  ; function app: check that args are equivalent to what function expects
  [; first, identify function's and args' types (need a handle on their shapes)
   (side-condition (display "checking function application"))
   (type-of sort-env kind-env type-env expr_fun type_fun)
   (type-of sort-env kind-env type-env expr_arg type_arg) ...
   ; get canonical types for the function & argument arrays
   (where (Array idx_fun-shape (type_input0 ... -> type_output))
          (canonicalize-type type_fun))
   (where [(Array idx_arg-shape type_arg-elt) ...]
          [(canonicalize-type type_arg) ...])
   ; extract expected rank of function array's elements
   (where (natural_arg-rank ...)
          (type->arg-rank (type_input0 ... -> type_output)))
   (side-condition (display (term expr_fun)))
   ; determine the frame of the implicit `apply'
   ; it's the approrpriate-sized prefix of the shape with the highest overrank
   (where (natural_app-rank ...) (0 natural_arg-rank ...))
   (where (natural_term-rank ...)
          ((shape->rank idx_fun-shape) (shape->rank idx_arg-shape) ...))
   ; originally had natural_app-frame, but axes might be variables too
   (where (S idx_app-frame ...)
          (frame-shape (natural_app-rank ...)
                       (idx_fun-shape idx_arg-shape ...)))
   ; if they are, they'd better be have sort Nat
   (sort-of sort-env kind-env type-env idx_app-frame Nat) ...
   ; make sure all terms can lift into this frame, i.e. each term's individual
   ; frame shape is a prefix of apply's frame shape
   ; this requires dropping the cell parts of all terms' shapes
   (side-condition
    (prefix-agree? (S idx_app-frame ...)
                   ; non-cell parts of terms' shapes
                   ((exclude-cell 0 idx_fun-shape)
                    (exclude-cell natural_arg-rank idx_arg-shape) ...)))
   --- fun-app
   (type-of sort-env kind-env type-env
            (expr_fun expr_arg ...)
            (canonicalize-type (Array (S idx_app-frame ...) type_output)))]
  
  ; type abstraction: 
  [; here is where we need the kind-env (extend here, check in other rules)
   (side-condition (display "checking type abstraction"))
   (type-of sort-env (kind-env-update [var ★] ... kind-env) type-env
            expr_body type)
   ---
   (type-of sort-env kind-env type-env
            (Λ [var ...] expr_body) (∀ [var ...] type))]
  
  ; type application
  [(side-condition (display "checking type application"))
   (type-of sort-env kind-env type-env expr (∀ (var ...) type))
   (kind-of sort-env kind-env type-env type_arg) ...
   ; make sure array types never get bound -- this restriction is needed for
   ; making function's input types fully determine its expected argument rank
   (side-condition (all-non-array type_arg ...))
   ;(side-condition ((λ (x) (x x)) (λ (x) (x x))))
   ---
   (type-of sort-env kind-env type-env
            (TYPE expr type_arg ...)
            (type/type-sub [(var type_arg) ...] type))]
  
  ; index abstraction: extend sort environment, make sure body has correct type
  [(side-condition (display "checking index abstraction"))
   (type-of (sort-env-update [var sort] ... sort-env)
            kind-env type-env expr type)
   ---
   (type-of sort-env kind-env type-env
            (ל [(var sort) ...] expr)
            (Π [(var sort) ...] type))]
  
  ; index app: check that indices have proper sort, substitute indices into type
  [(side-condition (display "checking index application"))
   (type-of sort-env kind-env type-env expr
            (Array idx_prod (Π ([var sort] ...) type)))
   (sort-of sort-env kind-env type-env idx_prod Shape)
   (sort-of sort-env kind-env type-env idx_arg sort) ...
   --- idx-app
   (type-of sort-env kind-env type-env (INDEX expr idx_arg ...)
            (Array idx_prod (index/type-sub ([var idx_arg] ...) type)))]
  
  ; projection from dependent sum
  [(type-of sort-env kind-env type-env expr (Σ [(var sort) ...] type))
   ---
   (type-of sort-env kind-env type-env (Σ-PROJ expr)
            (index/type-sub [(var (Σ-WITNESS var expr))...] type))]
  
  ; creation of dependent sum
  [(sort-of sort-env kind-env type-env idx sort) ...
   ; type_subbed in the type-of premise is in a position where metafunctions
   ; don't get evaluated, so we have to evaluate it in a `where' clause
   (where type_subbed (index/type-sub [(var idx) ...] type))
   (type-of sort-env kind-env type-env expr type_subbed)
   ;(where type_0 (canonicalize-type type))
   ---
   (type-of sort-env kind-env type-env
            (SUM idx ... expr (Σ [(var sort) ...] type))
            (Σ [(var sort) ...] type))]
  )

(define-judgment-form Dependent
  #:contract (sort-of sort-env kind-env type-env idx sort)
  #:mode (sort-of I I I I O)
  [#;???
   --- sort-nat
   (sort-of sort-env kind-env type-env natural Nat)]
  [(sort-of sort-env kind-env type-env idx Nat) ...
   --- sort-shape
   (sort-of sort-env kind-env type-env (S idx ...) Shape)]
  [---
   (sort-of ([var_0 sort_0] ... [var sort] [var_1 sort_1] ...)
            kind-env type-env var sort)]
  [(sort-of sort-env kind-env type-env idx_shape Shape) ...
   (sort-of sort-env kind-env type-env idx_rank Nat) ...
   ; TODO: will need a separate metafunction for this
   ; may also need to track actual index values
   ; or is that left to constraint generation?
   #;(side-condition (valid-shape (frame [idx_shape idx_rank] ...)))
   --- sort-frame
   (sort-of sort-env kind-env type-env (frame [idx_shape idx_rank] ...) Shape)]
  [(type-of sort-env kind-env type-env expr
            (Σ [(var_0 sort_0) ... (var sort) (var_1 sort_1) ...] type))
   ---
   (sort-of sort-env kind-env type-env (Σ-WITNESS var expr) sort)]
  )

; Determine whether a type is well-formed (does not use free variables for
; types or indices).
(define-judgment-form Dependent
  #:contract (kind-of sort-env kind-env type-env type)
  #:mode (kind-of I I I I)
  [---
   (kind-of sort-env kind-env type-env base-type)]
  [---
   (kind-of sort-env ([var_0 ★] ... [var ★] [var_1 ★] ...) type-env var)]
  [(kind-of (sort-env-update [var sort] ... sort-env) kind-env type-env type)
   ---
   (kind-of sort-env kind-env type-env (Π [(var sort) ...] type))]
  [(kind-of (sort-env-update [var sort] ... sort-env) kind-env type-env type)
   ---
   (kind-of sort-env kind-env type-env (Σ [(var sort) ...] type))]
  [(kind-of sort-env kind-env type-env type_arg) ...
   (kind-of sort-env kind-env type-env type_result)
   ---
   (kind-of sort-env kind-env type-env (type_arg ... -> type_result))]
  [(kind-of sort-env kind-env type-env type) ...
   ---
   (kind-of sort-env kind-env type-env (× type ...))]
  [(kind-of sort-env kind-env type-env type)
   (sort-of sort-env kind-env type-env idx Shape)
   ---
   (kind-of sort-env kind-env type-env (Array idx type))]
  [(kind-of sort-env (kind-env-update [var ★] ... kind-env) type-env type)
   ---
   (kind-of sort-env kind-env type-env (∀ [var ...] type))])



; separate judgment needed for checking array element types
; they must all have equivalent types (this will require that equivalent
; types be reducible to some canonical form)
(define-judgment-form Dependent
  #:contract (type-of/elts sort-env kind-env type-env (el-expr ...) type)
  #:mode (type-of/elts I I I I O)
  ; TODO: empty arrays will need either annotation or special casing
  ; as it stands now, they check as empty arrays of base type
  [(type-of sort-env kind-env type-env el-expr_0 type_0)
   (type-of sort-env kind-env type-env el-expr_1 type_1) ...
   (side-condition (term (all-equal? [(canonicalize-type type_0)
                                      (canonicalize-type type_1) ...])))
   --- non-base-elt
   (type-of/elts sort-env kind-env type-env (el-expr_0 el-expr_1 ...) type_0)]
  ; simple cases are base types (this is the only place where raw base-typed
  ; values can appear)
  [--- num-elt
       (type-of/elts sort-env kind-env type-env (num ...) Num)]
  [--- bool-elt
       (type-of/elts sort-env kind-env type-env (bool ...) Bool)])


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
   (S idx_frame-dim ...)
   ; how overranked is each term?
   (where (natural_over ...)
          ((-/m (shape->rank idx) natural_rank) ...))
   ;(side-condition (displayln (term (natural_over ...))))
   ; position of highest overrank?
   ;(side-condition (displayln (argmax first (term ([natural_over idx] ...)))))
   (where (natural_overrank (S idx_dim ...))
          ,(argmax first (term ([natural_over idx] ...))))
   ;(side-condition (displayln (term (natural_overrank (S idx_dim ...)))))
   ; extract prefix
   (where (idx_frame-dim ...) (take/m (idx_dim ...) natural_overrank))
   
   ;(side-condition (displayln ""))
   ; check that extracted prefix is a prefix of the other shapes
   ;(side-condition (term (prefix-agree? 
   ])

; determine whether a shape is a prefix of other shapes
(define-metafunction Dependent
  prefix-agree? : idx (idx ...) -> bool
  [(prefix-agree? idx ()) #t]
  [(prefix-agree? (S idx_prefix ...) ((S idx_shape ...) idx ...))
   (prefix-agree? (S idx_prefix ...) (idx ...))
   (side-condition (term (prefix? (idx_shape ...) (idx_prefix ...))))]
  ; if side-condition not met, we have a mismatch
  [(prefix-agree? (S idx_prefix ...) ((S idx_shape ...) idx ...)) #f])

; find the arg rank associated with a function type
(define-metafunction Dependent
  type->arg-rank : type -> (number ...)
  [(type->arg-rank (type_arg ... -> type_result)) ((type->rank type_arg) ...)])
; find the rank associated with an array type
; not for use with an open type
(define-metafunction Dependent
  type->rank : type -> number
  ; collapse nested array types
  #;[(type->rank (Array (S idx_outer ...) (Array (S idx_inner ...) type)))
     ,(+ (length (term (idx_outer ...)))
         (length (term (idx_inner ...)))
         (term (type->rank type)))]
  [(type->rank (Array (S idx ...) type))
   ,(+ (length (term (idx ...))) (term (type->rank type)))]
  [(type->rank (Array var type)) +inf.0]
  ; for now, pretending all other possible element types are scalar
  ; more detailed handling of Σ types might be awkward/impossible
  [(type->rank type) 0])

; find the rank associated with a given shape
; TODO: add clauses for index-level computation forms
(define-metafunction Dependent
  shape->rank : idx -> natural
  [(shape->rank (S idx ...)) ,(length (term (idx ...)))])

; drop the part of a shape designated as the cell
(define-metafunction Dependent
  exclude-cell : natural idx -> idx
  [(exclude-cell natural_rank (S idx_dim ...))
   (S idx_frame-dim ...)
   (where (idx_frame-dim ...)
          (drop-right/m (idx_dim ...) natural_rank))])

; make sure that none of the types are of the form (Array ___ ___)
; used in type application rule
(define-metafunction Dependent
  all-non-array : type ... -> bool
  [(all-non-array type_arg ...)
   ,(for/and ([t (term (type_arg ...))])
      ;(displayln t)
      (not (redex-match Dependent (Array idx type) t)))])

(define-metafunction Dependent
  primop-type : fun -> type
  [(primop-type +) ((Array (S) Num)
                    (Array (S) Num)
                    -> (Array (S) Num))]
  #;[(primop-type +) (Π [(s1 Shape) (s2 Shape)]
                        ((Array s1 Num)
                         (Array s2 Num)
                         -> (Array (frame [s1 0] [s2 0]) Num)))])

; determine the argument ranks a function expects
(define-metafunction Dependent
  fun-rank : expr -> (num ...)
  [(fun-rank expr)
   (fun-rank/type type_f)
   (where (type_f type_other ...)
          ,(judgment-holds (type-of [] [] [] expr type) type))])
; determine the argument ranks a function of a given type would expect
(define-metafunction Dependent
  fun-rank/type : type -> (num ...)
  [(fun-rank/type (type_a ... -> type_r))
   ((fun-rank/single (canonicalize-type type_a)) ...)]
  [(fun-rank/type (Π ([var sort] ...) type))
   (fun-rank/type type)]
  [(fun-rank/type (Σ ([var sort] ...) type))
   (fun-rank/type type)]
  [(fun-rank/type (∀ ([var] ...) type))
   (fun-rank/type type)]
  [(fun-rank/type (Array idx type))
   (fun-rank/type type)])
; determine the expected argument rank associated with an argument type
(define-metafunction Dependent
  fun-rank/single : type -> num
  [(fun-rank/single (Array var (Array idx type))) +inf.0]
  [(fun-rank/single (Array (S idx ...) (Array var type)))
   ,(- (length (term (idx ...))))]
  [(fun-rank/single (Array var type)) +inf.0]
  [(fun-rank/single (Array (S idx ...) type)) ,(length (term (idx ...)))])



;--------------------------------
; substitution metafunctions:
; metafunction name says whether vars represent exprs, types, or idxs
; and whether they're being substituted into an expr, type, or idx
; (they cannot be distinguished because a var might be any of them, and
; making sure exprs become exprs, etc. would require case-> contract anyway)
;--------------------------------
; substitute indices into an (element) expression
(define-metafunction Dependent
  index/expr-sub : idx-env el-expr -> el-expr
  [(index/expr-sub idx-env var) var]
  [(index/expr-sub idx-env base) base]
  [(index/expr-sub idx-env (A (idx ...) (el-expr ...)))
   (A ((index/index-sub idx-env idx) ...)
      ((index/expr-sub idx-env el-expr) ...))]
  ; function-like forms
  [(index/expr-sub idx-env (λ [(var num type) ...] expr))
   (λ [(var num (index/type-sub idx-env type)) ...]
     (index/expr-sub idx-env expr))]
  [(index/expr-sub idx-env fun) fun]
  [(index/expr-sub idx-env (Λ [var ...] expr))
   (Λ [var ...] (index/expr-sub idx-env expr))]
  [(index/expr-sub idx-env (ל [(var sort) ...] expr))
   ; index vars are shadowed by the ל-binder
   (ל [(var sort) ...] (index/expr-sub (shadow (var ...) idx-env) expr))]
  ; application forms
  [(index/expr-sub idx-env (expr_fun expr_arg ...))
   ((index/expr-sub idx-env expr_fun)
    (index/expr-sub idx-env expr_arg) ...)]
  [(index/expr-sub idx-env (TYPE expr_fun type_arg ...))
   ((index/expr-sub idx-env expr_fun)
    (index/type-sub idx-env type_arg) ...)]
  [(index/expr-sub idx-env (INDEX expr_fun idx_arg ...))
   ((index/expr-sub idx-env expr_fun)
    (index/expr-sub idx-env idx_arg) ...)]
  ; dependent sums
  [(index/expr-sub idx-env (SUM idx ... expr type))
   (SUM (index/index-sub idx-env idx) ...
        (index/expr-sub idx-env expr)
        (index/type-sub idx-env type))]
  [(index/expr-sub idx-env (Σ-PROJ expr))
   (Σ-PROJ (index/expr-sub idx-env expr))])

; substitute indices into a type
(define-metafunction Dependent
  index/type-sub : idx-env type -> type
  [(index/type-sub idx-env var) var]
  [(index/type-sub idx-env base-type) base-type]
  [(index/type-sub idx-env (Array idx type))
   (Array (index/index-sub idx-env idx) (index/type-sub idx-env type))]
  [(index/type-sub idx-env (type_arg ... -> type_result))
   ((index/type-sub idx-env type_arg) ...
    -> (index/type-sub idx-env type_result))]
  [(index/type-sub idx-env (× type ...))
   (× (index/type-sub idx-env type) ...)]
  [(index/type-sub idx-env (Π [(var sort) ...] type))
   (Π [(var sort) ...] (index/type-sub idx-env_shadowed type))
   (where idx-env_shadowed (shadow (var ...) idx-env))]
  [(index/type-sub idx-env (Σ [(var sort) ...] type))
   (Σ [(var sort) ...] (index/type-sub idx-env_shadowed type))
   (where idx-env_shadowed (shadow (var ...) idx-env))]
  [(index/type-sub idx-env (∀ [var ...] type))
   (∀ [var ...] (index/type-sub idx-env type))])

; substitute indices into an index
(define-metafunction Dependent
  index/index-sub : idx-env idx -> idx
  [(index/index-sub ([var_0 idx_0] ... [var_1 idx_1] [var_2 idx_2] ...) var_1)
   idx_1
   (side-condition (not (member (term var_1) (term (var_0 ...)))))]
  [(index/index-sub idx-env var) var]
  [(index/index-sub idx-env natural) natural]
  [(index/index-sub idx-env (S idx ...)) (S (index/index-sub idx-env idx) ...)]
  [(index/index-sub idx-env (frame [idx_shape idx_rank] ...))
   (frame [(index-sub idx-env idx_shape) (index-sub idx-env idx_rank)] ...)]
  [(index/index-sub idx-env (Σ-WITNESS var expr))
   (Σ-WITNESS (index/index-sub idx-env var) (index/index-sub idx-env expr))])

; substitute types into an (element) expression
(define-metafunction Dependent
  type/expr-sub : type-env el-expr -> el-expr
  [(type/expr-sub type-env var) var]
  [(type/expr-sub type-env base) base]
  [(type/expr-sub type-env (A (idx ...) (el-expr ...)))
   (A ((type/idx-sub type-env idx) ...)
      ((type/expr-sub type-env el-expr) ...))]
  ; function-like forms
  [(type/expr-sub type-env (λ [(var num type) ...] expr))
   (λ [(var num (type/type-sub type-env type)) ...]
     (type/expr-sub type-env expr))]
  [(type/expr-sub type-env fun) fun]
  [(type/expr-sub type-env (Λ [var ...] expr))
   ; shadow type vars
   (Λ [var ...] (type/expr-sub (shadow (var ...) type-env) expr))]
  [(type/expr-sub type-env (ל [(var sort) ...] expr))
   (ל [(var sort) ...] (type/expr-sub type-env expr))]
  ; application forms
  [(type/expr-sub type-env (expr_fun expr_arg ...))
   ((type/expr-sub type-env expr_fun)
    (type/expr-sub type-env expr_arg) ...)]
  [(type/expr-sub type-env (TYPE expr type ...))
   (TYPE (type/expr-sub type-env expr)
         (type/type-sub type-env type) ...)]
  [(type/expr-sub type-env (INDEX expr idx ...))
   (INDEX (type/expr-sub type-env expr)
          (type/index-sub type-env idx) ...)]
  ; dependent sums
  [(type/expr-sub type-env (SUM idx ... expr type))
   (SUM (type/index-sub type-env idx) ...
        (type/expr-sub type-env expr)
        (type/type-sub type-env type))]
  [(type/expr-sub type-env (Σ-PROJ expr))
   (Σ-PROJ (type/expr-sub type-env expr))])

; substitute types into a type
(define-metafunction Dependent
  type/type-sub : type-env type -> type
  [(type/type-sub ([var_0 type_0] ... [var type] [var_1 type_1] ...) var)
   type
   (side-condition (not (member (term var) (term (var_0 ...)))))]
  [(type/type-sub ([var type] ...) var_free) var_free]
  [(type/type-sub ([var type_s] ...) base-type) base-type]
  [(type/type-sub type-env (Π ([var_i sort] ...) type))
   (Π ([var_i sort] ...) (type/type-sub type-env type))]
  [(type/type-sub type-env (Σ ([var_i sort] ...) type))
   (Σ ([var_i sort] ...) (type/type-sub type-env type))]
  [(type/type-sub type-env (× type ...))
   (× (type/type-sub type-env type) ...)]
  [(type/type-sub type-env (type_a ... -> type_r))
   ((type/type-sub type-env type_a) ...
    -> (type/type-sub type-env type_r))]
  [(type/type-sub type-env (Array idx type_e))
   (Array idx (type/type-sub type-env type_e))]
  [(type/type-sub type-env (∀ (var_b ...) type_q))
   (∀ (var_b ...)
      (type/type-sub (shadow (var_b ...) type-env) type_q))])

; substitute types into an index
(define-metafunction Dependent
  type/index-sub : type-env idx -> idx
  [(type/index-sub type-env var) var]
  [(type/index-sub type-env natural) natural]
  [(type/index-sub type-env (S idx ...))
   (S (type/index-sub type-env idx) ...)]
  [(type/index-sub type-env (frame [idx_s idx_r] ...))
   (frame [(type/index-sub type-env idx_s)
           (type/index-sub type-env idx_r)] ...)]
  [(type/index-sub type-env (Σ-WITNESS var expr))
   (Σ-WITNESS var (type/expr-sub type-env expr))])

; substitute (element) expressions into an (element) expression
(define-metafunction Dependent
  expr/expr-sub : expr-env el-expr -> el-expr
  [(expr/expr-sub ([var_0 expr_0] ... [var expr] [var_1 expr_1] ...) var)
   expr
   (side-condition (not (member (term var) (term (var_0 ...)))))]
  [(expr/expr-sub expr-env var) var]
  [(expr/expr-sub expr-env (A (idx ...) (expr_elt ...)))
   (A (idx ...) ((expr/expr-sub expr-env expr_elt) ...))]
  ; function-like forms
  [(expr/expr-sub expr-env (λ ([var num type] ...) expr_body))
   (λ ([var num type] ...)
     (expr/expr-sub (shadow (var ...) expr-env) expr-body))]
  ; covers op and c-op
  [(expr/expr-sub expr-env fun) fun]
  [(expr/expr-sub expr-env (Λ ([var] ...) expr_body))
   ; type variables do not shadow term variables (whether a variable represents
   ; a term, type, or index is evident from is position)
   (Λ ([var] ...) (expr/expr-sub expr-env expr-body))]
  [(expr/expr-sub expr-env (ל ([var sort] ...) expr_body))
   ; index variables do not shadow term variables
   (ל ([var sort] ...) (expr/expr-sub expr-env expr-body))]
  ; application forms
  [(expr/expr-sub expr-env (expr_fun expr_arg ...))
   ((expr/expr-sub expr-env expr_fun) (expr/expr-sub expr-env expr_arg) ...)]
  [(expr/expr-sub expr-env (TYPE expr_fun type_arg ...))
   (TYPE (expr/expr-sub expr-env expr_fun)
         (expr/type-sub expr-env type_arg) ...)]
  [(expr/expr-sub expr-env (INDEX expr_fun idx_arg ...))
   (INDEX (expr/expr-sub expr-env expr_fun)
          (expr/idx-sub expr-env idx_arg) ...)]
  ; dependent sums
  [(expr/expr-sub expr-env (SUM idx ... expr type))
   (SUM (expr/idx-sub expr-env idx) ...
        (expr/expr-sub expr-env expr)
        (expr/type-sub expr-env type))]
  [(expr/expr-sub expr-env (Σ-PROJ expr))
   (Σ-PROJ (expr/expr-sub expr-env expr))])

; substitute expressions into a type
(define-metafunction Dependent
  expr/type-sub : expr-env type -> type
  [(expr/type-sub expr-env var) var]
  [(expr/type-sub expr-env base-type) base-type]
  [(expr/type-sub expr-env (Array idx type))
   (Array (expr/idx-sub expr-env idx) (expr/type-sub expr-env type))]
  [(expr/type-sub expr-env (× type ...))
   (× (expr/type-sub expr-env type) ...)]
  [(expr/type-sub expr-env (type_a ... -> type_r))
   ((expr/type-sub expr-env type_a) ...
    -> (expr/type-sub expr-env type_r))]
  [(expr/type-sub expr-env (∀ [var ...] type))
   (∀ [var ...] (expr/type-sub expr-env type))]
  [(expr/type-sub expr-env (Π [(var sort) ...] type))
   (Π [(var sort) ...] (expr/type-sub expr-env type))]
  [(expr/type-sub expr-env (Σ [(var sort) ...] type))
   (Σ [(var sort) ...] (expr/type-sub expr-env type))])

; substitute expressions into an index
(define-metafunction Dependent
  expr/index-sub : expr-env type -> type
  [(expr/index-sub expr-env var) var]
  [(expr/index-sub expr-env natural) natural]
  [(expr/index-sub expr-env (S idx ...))
   (S (expr/index-sub expr-env idx) ...)]
  [(expr/index-sub expr-env (frame [idx_s idx_r] ...))
   (frame [((expr/index-sub expr-env idx_s)
            (expr/index-sub expr-env idx_r)) ...])]
  [(expr/index-sub expr-env (Σ-WITNESS var expr))
   (Σ-WITNESS var (expr/index-sub expr-env expr))])


; reduce a type to canonical form:
; base types, dependent products, dependent sums, tuples of canonical types,
; functions on canonical types, arrays of canonical non-array types
(define-metafunction Dependent
  canonicalize-type : type -> type
  ; some types are already in canonical form
  [(canonicalize-type base-type) base-type]
  [(canonicalize-type var) var]
  ; no reducing inside the body (actually, it's probably safe to do so)
  [(canonicalize-type (Π [(var sort) ...] type)) (Π [(var sort) ...] type)]
  [(canonicalize-type (Σ [(var sort) ...] type)) (Σ [(var sort) ...] type)]
  [(canonicalize-type (∀ [var ...] type)) (∀ [var ...] type)]
  #;[(canonicalize-type (∃ [var ...] type)) (∃ [var ...] type)]
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
  ; TODO: need to check whether inner or outer index is a variable instead of
  ; a shape literal -- if it is a variable, canonical form is still nested
  [(canonicalize-type (Array (S idx_outer ...) type))
   ; may need multiple steps of reduction
   (canonicalize-type (Array (S idx_outer ... idx_inner ...) type_elt))
   (where (Array (S idx_inner ...) type_elt) type)]
  [(canonicalize-type (Array (S idx_outer ...) type))
   (Array (S idx_outer ...) (canonicalize-type type))]
  [(canonicalize-type (Array var type))
   (Array var (canonicalize-type type))])

; reduce an index to canonical form
(define-metafunction Dependent
  canonicalize-index : idx -> idx
  ; naturals and variables are already in canonical form
  [(canonicalize-index natural) natural]
  [(canonicalize-index var) var]
  ; for shapes, recur on their axes
  [(canonicalize-index (S idx ...)) (S (canonicalize-index idx) ...)]
  ; index-level computation
  [(canonicalize-index (frame [idx_rank idx_shape] ...))
   (frame-shape [(canonicalize-index idx_rank) ...]
                [(canonicalize-index idx_shape) ...])])

; find the shape that comes from nesting one shape inside another
(define-metafunction Dependent
  nest-shape : idx idx ... -> idx
  [(nest-shape idx) idx]
  [(nest-shape (S idx_frame ...) (S idx_cell ...) idx ...)
   (nest-shape (S idx_frame ... idx_cell ...) idx ...)])



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

; For now, all we can check in a kind environment is whether a certain type
; variable is there. If type-level computation is to be added later, this will
; have to become a lookup function like in other environments.
(define-metafunction Dependent
  kind-env-contains? : var kind-env -> bool
  [(kind-env-contains? var ([var_0] ... [var] [var_1] ...)) #t]
  [(kind-env-contains? var kind-env) #f])
(define-metafunction Dependent
  kind-env-update : k-bind ... kind-env -> kind-env
  ; if no changes, return unmodified environment
  [(kind-env-update kind-env) kind-env]
  ; without any actual kinds to bind them to, just look for preexisting bindings
  [(kind-env-update k-bind_0 k-bind_1 ... (k-bind_2 ... k-bind_0 k-bind_3 ...))
   (kind-env-update k-bind_1 ... (k-bind_2 ... k-bind_0 k-bind_3 ...))]
  [(kind-env-update k-bind_0 k-bind_1 ... (k-bind_2 ...))
   (kind-env-update k-bind_1 ... (k-bind_0 k-bind_2 ...))
   #;(side-condition (not (term (kind-env-contains? k-bind_0
                                                    (k-bind_2 ...)))))])

; remove the named entries from an environment
; var bound to any so this can be used on any form of binding list
(define-metafunction Dependent
  shadow : (var ...) [(var any) ...] -> [(var any) ...]
  [(shadow () [(var any) ...]) [(var any) ...]]
  ;[(shadow (var ...) []) []]
  [(shadow (var_0 var_1 ...)
           [(var_2 any_2) ... (var_0 any_0) (var_3 any_3) ...])
   (shadow (var_1 ...)
           [(var_2 any_2) ... (var_3 any_3) ...])]
  ;if this is reached, no var appears in both shadow list and environment
  [(shadow (var_s ...) ([var_e any] ...))  ([var_e any] ...)])


(define-metafunction Dependent
  -/m : number ... -> number
  [(-/m number ...) ,(apply - (term (number ...)))])
(define-metafunction Dependent
  argmax/m : (number ...) -> number
  [(argmax/m (number ...)) ,(argmax (λ (x) x) (term (number ...)))])


(define-metafunction Dependent
  all-equal? : (any ...) -> bool
  [(all-equal? ()) #t]
  [(all-equal? (any)) #t]
  [(all-equal? (any_1 any_1 any_2 ...))
   ,(and (term (all-equal? (any_1 any_2 ...))))]
  [(all-equal? any) #f])

(module+
 test
 
 ;-------------------
 ; expression typing judgment
 ;-------------------
 ; array of numbers
 (check-equal? (judgment-holds (type-of () () () (A (3 2) (4 1 6 2 3 5)) type)
                               type)
               (term ((Array (S 3 2) Num))))
 
 ; array of booleans
 (check-equal? (judgment-holds (type-of () () () (A (2) (#f #t)) type)
                               type)
               (term ((Array (S 2) Bool))))
 
 ; array with wrong number of elements
 (check-equal? (judgment-holds (type-of () () () (A (3 2) (4 1 6 2 3)) type)
                               type)
               '())
 
 ; array of arrays
 (check-equal? (judgment-holds (type-of () () ()
                                        (A (2) ((A (3) (1 2 3))
                                                (A (3) (4 5 6))))
                                        type)
                               type)
               (term ((Array (S 2) (Array (S 3) Num)))))
 
 ; simple λ term
 (check-equal?
  (judgment-holds
   (type-of () () () (λ ([x 0 (Array (S) Num)]) (A () (3))) type)
   type)
  (term (((Array (S) Num) -> (Array (S) Num)))))
 
 ; multiargument λ term
 (check-equal?
  (judgment-holds
   (type-of () () () (λ ([x 0 (Array (S 3) Num)]
                         [y 0 (Array (S 1) Bool)]) (A () (3))) type)
   type)
  (term (((Array (S 3) Num) (Array (S 1) Bool) -> (Array (S) Num)))))
 
 ; applying the simple λ term
 (check-equal?
  (judgment-holds
   (type-of () () () ((A () ((λ ([x 0 (Array (S) Num)]) (A () (3)))))
                      (A () (4)))
            type)
   type)
  (term ((Array (S) Num))))
 
 ; applying the multiargument λ term
 (check-equal?
  (judgment-holds
   (type-of () () () ((A () ((λ ([x 0 (Array (S 3) Num)]
                                 [y 0 (Array (S 1) Bool)])
                               (A () (3)))))
                      (A (3) (1 2 3)) (A (1) (#f))) type)
   type)
  (term ((Array (S) Num))))
 
 ; applying to arg with equivalent-but-not-identical type
 (check-equal?
  (judgment-holds
   (type-of () () () ((A () ((λ ([x 2 (Array (S 2 3) Num)]) x)))
                      (A (2) ((A (3) (1 2 3))
                              (A (3) (4 5 6)))))
            type)
   type)
  (term ((Array (S 2 3) Num))))
 
 ; applying a nested array of functions
 (check-equal?
  (judgment-holds
   (type-of [] [] [] ((A (3) [(A (2) [(λ ([x 0 (Array (S) Num)]) (A () (1)))
                                      (λ ([x 0 (Array (S) Num)]) (A () (2)))])
                              (A (2) [(λ ([x 0 (Array (S) Num)]) (A () (3)))
                                      (λ ([x 0 (Array (S) Num)]) (A () (4)))])
                              (A (2) [(λ ([x 0 (Array (S) Num)]) (A () (5)))
                                      (λ ([x 0 (Array (S) Num)]) (A () (6)))])])
                      (A (3) (7 23523 245)))
            type)
   type)
  (term ((Array (S 3 2) Num))))
 
 ; type abstraction, and use the bound type variable
 (check-equal?
  (judgment-holds
   (type-of [] [] []
            (Λ [elt] (A [] [(λ ([x 0 (Array (S) elt)]) x)]))
            type) type)
  (term ((∀ [elt] (Array (S)
                         ((Array (S) elt) -> (Array (S) elt)))))))
 ; type abstraction, but use a free type variable (makes an ill-formed type)
 (check-equal?
  (judgment-holds
   (type-of [] [] []
            (Λ [elt] (A [] [(λ ([x 0 (Array (S) foo)]) x)]))
            type) type)
  '())
 
 ; type application
 (check-equal?
  (judgment-holds
   (type-of [] [] []
            (TYPE (Λ [elt] (A [] [(λ ([x 0 (Array (S) elt)]) x)]))
                  Bool)
            type)
   type)
  (term ((Array (S) ((Array (S) Bool) -> (Array (S) Bool))))))
 
 ; index application
 ; TODO: find some other function to use, as +'s type is now scalar->scalar
 ; append is probably a good one (for now, just stick something in environment)
 (check-equal?
  (judgment-holds
   (type-of [] [] [(op (Π ([d1 Nat]) ((Array (S d1) Num)
                                      -> (Array (S 1 d1) Num))))]
            (INDEX (A () [op]) 3)
            type)
   type)
  (term ((Array (S) [(Array (S 3) Num) -> (Array (S 1 3) Num)]))))
 
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
 
 ; index abstraction
 (check-equal?
  (judgment-holds (type-of [][][]
                           (ל [(d Nat)]
                              (A [] [(λ [(l 1 (Array (S d) Num))] l)]))
                           type) type)
  (term ((Π [(d Nat)]
            (Array (S)
                   ((Array (S d) Num)
                    -> (Array (S d) Num)))))))
 
 ; make sure abstracted index vars are available for use
 (check-equal?
  (judgment-holds
   (type-of [(d1 Nat) (d2 Nat)]
            []
            [(a1 (Array (S d1 d2) Num)) (a2 (Array (S d1 d2) Num))]
            ([A () (+)] a1 a2)
            type)
   type)
  (term ((Array (S d1 d2) Num))))
 
 ; projection from a dependent sum
 (check-equal?
  (judgment-holds
   (type-of [] [] [(x (Σ [(d1 Nat) (d2 Nat)] (Array (S d1 3 d2) Num)))]
            (Σ-PROJ x) type) type)
  (term ((Array (S (Σ-WITNESS d1 x) 3 (Σ-WITNESS d2 x)) Num))))
 
 ; creation of a dependent sum
 (check-equal?
  (judgment-holds
   (type-of [][][]
            (SUM 3 (A (3) (0 1 2)) (Σ [(d Nat)] (Array (S d) Num)))
            type)
   type)
  (term ((Σ [(d Nat)] (Array (S d) Num)))))
 ; (projection from a hand-made dependent sum is really a run-time issue)
 
 
 ;-------------------
 ; type and index well-formedness
 ;-------------------
 (check-true
  (judgment-holds (kind-of [] [] [] (Array (S 4 3) Num))))
 (check-true
  (judgment-holds (kind-of [] [] [] ((Array (S 4 3) Num) -> Bool))))
 (check-true
  (judgment-holds (kind-of ([sh Shape]) [] [] ((Array sh Num) -> Bool))))
 (check-true
  (judgment-holds (kind-of ([sh Shape]) [] [] (× (Array sh Num) Bool))))
 (check-true
  (judgment-holds (kind-of ([sh Shape]) [] [] (Array sh (× Num Bool)))))
 (check-true
  (judgment-holds (kind-of [] [] [] (Π ([sh Shape]) ((Array sh Num) -> Bool)))))
 (check-true
  (judgment-holds (kind-of [] [] []
                           (Π ([sh Shape]) ((Array sh Num) -> Bool)))))
 (check-true
  (judgment-holds (kind-of [] [] []
                           (Π ([len Nat]) ((Array (S 3 len 2) Num) -> Bool)))))
 (check-true
  (judgment-holds (kind-of [] [] []
                           (∀ (elt)
                              (Π ([fr Shape])
                                 ((Array fr elt) -> (Array fr elt)))))))
 ; can tell by position whether a variable indicates a type or an index
 (check-true
  (judgment-holds (kind-of [(x Shape)] [(x ★)] []
                           (Array x x))))
 ; free index variable
 (check-false
  (judgment-holds (kind-of [] [] [] (Array sh Num))))
 ; free type variable
 (check-false
  (judgment-holds (kind-of [] [] [] (Array (S) elt))))
 
 
 
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
  (term ([z Nat] [y Shape] [x Nat])))
 
 ; TODO: need more substitution tests
 ; exercising the type substitution metafunction
 (check-equal?
  (term (type/type-sub
         ([elt1 Num] [elt2 (Num -> Bool)])
         (∀ (elt3) (Array (S) [(Array (S) elt1)
                               -> (Array (S 3) (elt2 -> elt3))]))))
  (term (∀ (elt3) (Array (S) ((Array (S) Num)
                              -> (Array (S 3) ((Num -> Bool) -> elt3))))))))
