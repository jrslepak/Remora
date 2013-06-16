#lang racket

(require rackunit
         redex
         "language.rkt")
(provide Dependent
         type-of kind-of sort-of)

(define-extended-language Dependent Arrays
  ; may also want to add a type annotation to array syntax
  ; (A type (num ...) (el-expr ...)), etc.
  (expr (expr expr ...) ; include bits from untyped language, but not boxing
        arr
        var
        ; type abstraction
        (T-λ [var ...] expr)
        ; type application
        (T-APP expr type ...)
        ; construction of dependent sum
        (PACK idx ... expr type)
        ; let-like binding to destruct a dependent sum
        (UNPACK ([var ... var] ⇐ expr) expr)
        ; index abstraction
        (I-λ [(var sort) ...] expr)
        ; index application
        (I-APP expr idx ...))
  
  (arr ....
       ; allow a type annotation
       (A type (num ...) (el-expr ...)))
  
  ; for array syntax, already-present shape vector works as constructor index
  ; allow arrays of types and sorts? or are arrays strictly value-level?
  
  ; have to include type annotations in λ syntax
  (fun (λ [(var type) ...] expr)
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
  ; an array type, but with a hole where the element type should be
  (frame-struct (Array idx frame-struct)
                (Array idx hole))
  
  ; sort-level pieces
  ; constraint domain would probably be something like nat lists
  (sort Nat
        Shape)
  (idx natural
       (S idx ...)
       (PLUS idx idx)
       var)
  
  ; extra machinery for type checking and substitution purposes
  (expr-env (e-bind ...)) (e-bind [var expr])
  (idx-env (i-bind ...)) (i-bind [var idx])
  (type-env (t-bind ...)) (t-bind [var type])
  (kind-env (k-bind ...)) (k-bind [var ★]) ; may later need [var kind]
  (sort-env (s-bind ...)) (s-bind [var sort]))

(caching-enabled? #f) ; useful for testing purposes
(define-metafunction Dependent
  displayln/m : any -> any
  [(displayln/m any)
   #t
   (side-condition (displayln (term any)))])



; type check an expression (or single element expression) in the
; dependently-typed version of the language
; need a kind-env ::= (var ...), and have to check that type variables are bound
; before they are used (they can appear in λ, T-λ, I-λ, and T-APP forms)
(define-judgment-form Dependent
  #:contract (type-of sort-env kind-env type-env el-expr type)
  #:mode (type-of I I I I O)
  ; base data
  [(type-of sort-env kind-env type-env num Num)]
  [(type-of sort-env kind-env type-env bool Bool)]
  ; array: find element type, check for correct number of elements
  [(type-of/elts sort-env kind-env type-env (el-expr ...) type)
   ; would prefer `(type-of sort-env kind-env type-env el-expr type) ...' here,
   ; but Redex doesn't realize there must be exactly one `type' (causing an
   ; ellipsis mismatch)
   (side-condition (size-check (A (natural ...) (el-expr ...))))
   --- array
   (type-of sort-env kind-env type-env
            (A (natural ...) (el-expr ...))
            (Array (S natural ...) type))]
  [(check-elts sort-env kind-env type-env (el-expr ...) type)
   (side-condition (size-check (A (natural ...) (el-expr ...))))
   --- array-annotated
   (type-of sort-env kind-env type-env
            (A type (natural ...) (el-expr ...))
            (Array (S natural ...) type))]
  ; variable: grab from environment (not there -> ill-typed)
  [; should probably change this to have premise which calls type env lookup
   --- variable
   (type-of sort-env kind-env
            ([var_0 type_0] ... [var_1 type_1] [var_2 type_2] ...)
            var_1 type_1)]
  ; primitive operator: call out to metafunction
  [(where type (primop-type op))
   --- operator
   (type-of sort-env kind-env type-env op type)]
  ; λ abstraction: add inputs to type environment, check body
  [(kind-of sort-env kind-env type_arg) ...
   (type-of sort-env kind-env (type-env-update [var type_arg] ... type-env)
            expr type_body)
   --- lambda
   (type-of sort-env kind-env type-env
            (λ [(var type_arg) ...] expr)
            (type_arg ... -> type_body))]
  ; function app: check that args are equivalent to what function expects
  [; first, identify function's and args' types (need a handle on their shapes)
   (type-of sort-env kind-env type-env expr_fun type_fun)
   (type-of sort-env kind-env type-env expr_arg type_arg) ...
   ; get canonical types for the function & argument arrays
   (where (Array idx_fun-shape (type_expected ... -> type_output))
          (canonicalize-type type_fun))
   (where [(Array idx_arg-shape type_arg-elt) ...]
          [(canonicalize-type type_arg) ...])
   ; determine the frame of the implicit `apply':
   ; - find what each function/arg array contributes to the frame
   (where (frame-struct_pieces ...)
          ; the part for the function is the shape of the function array
          ((Array idx_fun-shape hole)
           ; the part for the args is based on the expected and actual types
           (frame-contribution type_expected
                               (Array idx_arg-shape type_arg-elt)) ...))
   ; - the largest contribution (by prefix ordering) is the frame
   (where frame-struct_max (largest-frame (frame-struct_pieces ...)))
   --- fun-app
   (type-of sort-env kind-env type-env
            (expr_fun expr_arg ...)
            (canonicalize-type (in-hole frame-struct_max type_output)))]
  ; type abstraction:
  [; here is where we need the kind-env (extend here, check in other rules)
   (type-of sort-env (kind-env-update [var ★] ... kind-env) type-env
            expr_body type)
   ---
   (type-of sort-env kind-env type-env
            (T-λ [var ...] expr_body) (∀ [var ...] type))]
  ; type application
  [(type-of sort-env kind-env type-env expr (∀ (var ...) type))
   (kind-of sort-env kind-env type_arg) ...
   ; make sure array types never get bound -- this restriction is needed for
   ; making function's input types fully determine its expected argument rank
   (side-condition (all-non-array type_arg ...))
   ---
   (type-of sort-env kind-env type-env
            (T-APP expr type_arg ...)
            (type/type-sub [(var type_arg) ...] type))]
  
  ; index abstraction: extend sort environment, make sure body has correct type
  [(type-of (sort-env-update [var sort] ... sort-env)
            kind-env type-env expr type)
   ---
   (type-of sort-env kind-env type-env
            (I-λ [(var sort) ...] expr)
            (Π [(var sort) ...] type))]
  
  ; index app: check that indices have proper sort, substitute indices into type
  [(type-of sort-env kind-env type-env expr
            (Array idx_prod (Π ([var sort] ...) type)))
   (sort-of sort-env idx_prod Shape)
   (sort-of sort-env idx_arg sort) ...
   --- idx-app
   (type-of sort-env kind-env type-env (I-APP expr idx_arg ...)
            (Array idx_prod (index/type-sub ([var idx_arg] ...) type)))]
  
  ; projection from dependent sum
  [(type-of sort-env kind-env type-env
            expr_sum (Σ [(var_witness sort_witness) ...] type_contents))
   (type-of (sort-env-update [var_witness sort_witness] ... sort-env)
            kind-env
            (type-env-update [var_contents type_contents] type-env)
            expr_body type_body)
   (kind-of sort-env kind-env type_body)
   ---
   (type-of sort-env kind-env type-env
            (UNPACK ([var_witness ... var_contents] ⇐ expr_sum) expr_body)
            type_body)]
  
  ; creation of dependent sum
  [(sort-of sort-env idx sort) ...
   ; type_subbed in the type-of premise is in a position where metafunctions
   ; don't get evaluated, so we have to evaluate it in a `where' clause
   (where type_subbed (index/type-sub [(var idx) ...] type))
   (type-of sort-env kind-env type-env expr type_subbed)
   ;(where type_0 (canonicalize-type type))
   ---
   (type-of sort-env kind-env type-env
            (PACK idx ... expr (Σ [(var sort) ...] type))
            (Σ [(var sort) ...] type))])

(define-judgment-form Dependent
  #:contract (sort-of sort-env idx sort)
  #:mode (sort-of I I O)
  [#;???
   --- sort-nat
   (sort-of sort-env natural Nat)]
  [(sort-of sort-env idx Nat) ...
   --- sort-shape
   (sort-of sort-env (S idx ...) Shape)]
  [---
   (sort-of ([var_0 sort_0] ... [var sort] [var_1 sort_1] ...)
            var sort)]
  [(sort-of sort-env idx_1 Nat)
   (sort-of sort-env idx_2 Nat)
   ---
   (sort-of sort-env (PLUS idx_1 idx_2) Nat)])

; Determine whether a type is well-formed (does not use free variables for
; types or indices).
(define-judgment-form Dependent
  #:contract (kind-of sort-env kind-env type)
  #:mode (kind-of I I I)
  [---
   (kind-of sort-env kind-env base-type)]
  [---
   (kind-of sort-env ([var_0 ★] ... [var ★] [var_1 ★] ...) var)]
  [(kind-of (sort-env-update [var sort] ... sort-env) kind-env type)
   ---
   (kind-of sort-env kind-env (Π [(var sort) ...] type))]
  [(kind-of (sort-env-update [var sort] ... sort-env) kind-env type)
   ---
   (kind-of sort-env kind-env (Σ [(var sort) ...] type))]
  [(kind-of sort-env kind-env type_arg) ...
   (kind-of sort-env kind-env type_result)
   ---
   (kind-of sort-env kind-env (type_arg ... -> type_result))]
  [(kind-of sort-env kind-env type) ...
   ---
   (kind-of sort-env kind-env (× type ...))]
  [(kind-of sort-env kind-env type)
   (sort-of sort-env idx Shape)
   ---
   (kind-of sort-env kind-env (Array idx type))]
  [(kind-of sort-env (kind-env-update [var ★] ... kind-env) type)
   ---
   (kind-of sort-env kind-env (∀ [var ...] type))])



; separate judgment needed for checking array element types
; they must all have equivalent types (this will require that equivalent
; types be reducible to some canonical form)
(define-judgment-form Dependent
  #:contract (type-of/elts sort-env kind-env type-env (el-expr ...) type)
  #:mode (type-of/elts I I I I O)
  [(type-of sort-env kind-env type-env el-expr_0 type_0)
   (type-of sort-env kind-env type-env el-expr_1 type_1) ...
   (side-condition (term (all-equal? [(canonicalize-type type_0)
                                      (canonicalize-type type_1) ...])))
   --- non-base-elt
   (type-of/elts sort-env kind-env type-env (el-expr_0 el-expr_1 ...) type_0)]
  ; simple cases are base types (this is the only place where raw base-typed
  ; values can appear)
  [--- num-elt
       (type-of/elts sort-env kind-env type-env (num_0 num_1 ...) Num)]
  [--- bool-elt
       (type-of/elts sort-env kind-env type-env (bool_0 bool_1 ...) Bool)])

; separate judgment needed for checking array element types
; they must all have a type equivalent to the specified type
(define-judgment-form Dependent
  #:contract (check-elts sort-env kind-env type-env (el-expr ...) type)
  #:mode (check-elts I I I I I)
  [(type-of sort-env kind-env type-env el-expr type_derived) ...
   (side-condition (term (all-equal? [(canonicalize-type type_annotated)
                                      (canonicalize-type type_derived) ...])))
   --- non-base-elt
   (check-elts sort-env kind-env type-env (el-expr ...) type_annotated)])

; check whether an array has as many elements as its shape says it should have
(define-metafunction Dependent
  size-check : arr -> bool
  [(size-check (A (num ...) (el-expr ...))) ,(= (foldr * 1 (term (num ...)))
                                                (length (term (el-expr ...))))])


; identify what an argument's expected and actual types require
; as a prefix of the function application frame
; TODO: there are probably other cases to consider
(define-metafunction Dependent
  frame-contribution : type type -> frame-struct
  [(frame-contribution (Array var_i type_cell)
                       (Array var_i type_cell)) (Array (S) hole)]
  [(frame-contribution (Array var_i type_cell)
                       (Array var_j type_cell)) #f]
  [(frame-contribution (Array var_i type_cell) type_actual) (Array var_i hole)]
  [(frame-contribution (Array (S) type) (Array idx type)) (Array idx hole)]
  [(frame-contribution (Array (S) type) (Array idx_1 (Array idx_2 type)))
   (Array idx_1 (Array idx_2 hole))]
  [(frame-contribution (Array (S idx_0 ...) type)
                       (Array (S idx_0 ... idx_1 ...) type))
   (Array (S idx_1 ...) hole)])

; find the frame-struct that is prefixed by all other frame-structs in the list
; this is essentially a left fold with larger-frame
(define-metafunction Dependent
  largest-frame : (frame-struct ...) -> frame-struct or #f
  [(largest-frame ()) #f]
  [(largest-frame (frame-struct)) frame-struct]
  [(largest-frame (frame-struct_0 frame-struct_1 frame-struct_rest ...))
   (largest-frame (frame-struct_l frame-struct_rest ...))
   (where frame-struct_l (larger-frame frame-struct_0 frame-struct_1))]
  [(largest-frame (frame-struct_0 frame-struct_1 frame-struct_rest ...))
   #f
   (side-condition (printf "incomparable frame-structs: ~a and ~a\n"
                           (term frame-struct_0)
                           (term frame-struct_1)))])
; select the frame-struct which is guaranteed to have the other as its prefix
; TODO: there are probably other cases to consider
(define-metafunction Dependent
  larger-frame : frame-struct frame-struct -> frame-struct or #f
  ; given same argument twice, return it
  [(larger-frame frame-struct frame-struct) frame-struct]
  ; if one frame is scalar, return the other
  [(larger-frame (Array (S) hole) frame-struct) frame-struct]
  [(larger-frame frame-struct (Array (S) hole)) frame-struct]
  [(larger-frame (Array (S idx_0 ...) hole)
                 (Array (S idx_0 ... idx_1 ...) hole))
   (Array (S idx_0 ... idx_1 ...) hole)]
  [(larger-frame (Array (S idx_0 ... idx_1 ...) hole)
                 (Array (S idx_0 ...) hole))
   (Array (S idx_0 ... idx_1 ...) hole)]
  ; catch-all case for those that failed to match any of the above
  [(larger-frame frame-struct_1 frame-struct_2) #f])

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
; TODO: or make sure this metafunction is no longer needed
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
                    -> (Array (S) Num))])

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
   (λ [(var (index/type-sub idx-env type)) ...]
     (index/expr-sub idx-env expr))]
  [(index/expr-sub idx-env fun) fun]
  [(index/expr-sub idx-env (T-λ [var ...] expr))
   (T-λ [var ...] (index/expr-sub idx-env expr))]
  [(index/expr-sub idx-env (I-λ [(var sort) ...] expr))
   ; index vars are shadowed by the I-λ-binder
   (I-λ [(var sort) ...] (index/expr-sub (shadow (var ...) idx-env) expr))]
  ; application forms
  [(index/expr-sub idx-env (expr_fun expr_arg ...))
   ((index/expr-sub idx-env expr_fun)
    (index/expr-sub idx-env expr_arg) ...)]
  [(index/expr-sub idx-env (T-APP expr_fun type_arg ...))
   ((index/expr-sub idx-env expr_fun)
    (index/type-sub idx-env type_arg) ...)]
  [(index/expr-sub idx-env (I-APP expr_fun idx_arg ...))
   ((index/expr-sub idx-env expr_fun)
    (index/expr-sub idx-env idx_arg) ...)]
  ; dependent sums
  [(index/expr-sub idx-env (PACK idx ... expr type))
   (PACK (index/index-sub idx-env idx) ...
         (index/expr-sub idx-env expr)
         (index/type-sub idx-env type))]
  [(index/expr-sub idx-env (UNPACK ([var_wit ... var_cont] ⇐ expr_sum) expr_body))
   (UNPACK ([var_wit ... var_cont] ⇐ (index/expr-sub idx-env expr_sum))
           (index/expr-sub idx-env_shadowed expr_body))
   (where idx-env_shadowed (shadow (var_wit ...) idx-env))])

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
  [(index/index-sub idx-env (PLUS idx_0 idx_1))
   (PLUS (index-sub idx-env idx_0) (index-sub idx-env idx_1))])

; substitute types into an (element) expression
(define-metafunction Dependent
  type/expr-sub : type-env el-expr -> el-expr
  [(type/expr-sub type-env var) var]
  [(type/expr-sub type-env base) base]
  [(type/expr-sub type-env (A (idx ...) (el-expr ...)))
   (A ((type/idx-sub type-env idx) ...)
      ((type/expr-sub type-env el-expr) ...))]
  ; function-like forms
  [(type/expr-sub type-env (λ [(var type) ...] expr))
   (λ [(var (type/type-sub type-env type)) ...]
     (type/expr-sub type-env expr))]
  [(type/expr-sub type-env fun) fun]
  [(type/expr-sub type-env (T-λ [var ...] expr))
   ; shadow type vars
   (T-λ [var ...] (type/expr-sub (shadow (var ...) type-env) expr))]
  [(type/expr-sub type-env (I-λ [(var sort) ...] expr))
   (I-λ [(var sort) ...] (type/expr-sub type-env expr))]
  ; application forms
  [(type/expr-sub type-env (expr_fun expr_arg ...))
   ((type/expr-sub type-env expr_fun)
    (type/expr-sub type-env expr_arg) ...)]
  [(type/expr-sub type-env (T-APP expr type ...))
   (T-APP (type/expr-sub type-env expr)
          (type/type-sub type-env type) ...)]
  [(type/expr-sub type-env (I-APP expr idx ...))
   (I-APP (type/expr-sub type-env expr)
          (type/index-sub type-env idx) ...)]
  ; dependent sums
  [(type/expr-sub type-env (PACK idx ... expr type))
   (PACK (type/index-sub type-env idx) ...
         (type/expr-sub type-env expr)
         (type/type-sub type-env type))]
  [(type/expr-sub type-env (UNPACK ([var_wit ... var_cont] ⇐ expr_sum) expr_body))
   (UNPACK ([var_wit ... var_cont] ⇐ (type/expr-sub type-env expr_sum))
           (type/expr-sub type-env expr_body))])

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
  [(type/index-sub type-env (PLUS idx_0 idx_1))
   (PLUS (type/index-sub type-env idx_0) (type/index-sub type-env idx_1))])

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
  [(expr/expr-sub expr-env (λ ([var type] ...) expr_body))
   (λ ([var type] ...)
     (expr/expr-sub (shadow (var ...) expr-env) expr-body))]
  ; covers op and c-op
  [(expr/expr-sub expr-env fun) fun]
  [(expr/expr-sub expr-env (T-λ ([var] ...) expr_body))
   ; type variables do not shadow term variables (whether a variable represents
   ; a term, type, or index is evident from is position)
   (T-λ ([var] ...) (expr/expr-sub expr-env expr-body))]
  [(expr/expr-sub expr-env (I-λ ([var sort] ...) expr_body))
   ; index variables do not shadow term variables
   (I-λ ([var sort] ...) (expr/expr-sub expr-env expr-body))]
  ; application forms
  [(expr/expr-sub expr-env (expr_fun expr_arg ...))
   ((expr/expr-sub expr-env expr_fun) (expr/expr-sub expr-env expr_arg) ...)]
  [(expr/expr-sub expr-env (T-APP expr_fun type_arg ...))
   (T-APP (expr/expr-sub expr-env expr_fun)
          (expr/type-sub expr-env type_arg) ...)]
  [(expr/expr-sub expr-env (I-APP expr_fun idx_arg ...))
   (I-APP (expr/expr-sub expr-env expr_fun)
          (expr/idx-sub expr-env idx_arg) ...)]
  ; dependent sums
  [(expr/expr-sub expr-env (PACK idx ... expr type))
   (PACK (expr/idx-sub expr-env idx) ...
         (expr/expr-sub expr-env expr)
         (expr/type-sub expr-env type))]
  [(expr/expr-sub expr-env (UNPACK ([var_wit ... var_cont] ⇐ expr_sum) expr_body))
   (UNPACK ([var_wit ... var_cont] ⇐ (expr/expr-sub expr-env expr_sum))
           (expr/expr-sub expr-env_shadowed expr_body))
   (where expr-env_shadowed (shadow (var_cont) expr-env))])

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
  [(expr/index-sub expr-env (PLUS idx_0 idx_1))
   (PLUS (expr/index-sub expr-env idx_0) (expr/index-sub expr-env idx_1))])


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
  [(canonicalize-type (Array idx (Σ [(var sort) ...] type)))
   (canonicalize-type (Σ [(var sort) ...] type))]
  ; make sure the element type isn't another array
  [(canonicalize-type (Array idx type))
   (Array (canonicalize-index idx) (canonicalize-type type))
   ; sometimes, Redex has funny binding structure
   (side-condition (not (redex-match Dependent (Array idx type) (term type))))]
  ; if it is another array, we need to collapse the nested array type into
  ; a flat array type
  ; TODO: need to check whether inner or outer index is a variable instead of
  ; a shape literal -- if it is a variable, canonical form is still nested
  [(canonicalize-type (Array (S) (Array idx type))) (Array idx type)]
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
  [(canonicalize-index (PLUS natural_0 natural_1))
   ,(+ (term natural_0) (term natural_1))]
  [(canonicalize-index (PLUS idx_0 idx_1))
   (summands->idx (summand-list (PLUS idx_0 idx_1)))])

; convert an index to a list of summands
(define-metafunction Dependent
  summand-list : idx -> (idx ...)
  ; for a PLUS form, concatenate its summand lists
  [(summand-list (PLUS idx_0 idx_1))
   ,(append (term (summand-list idx_0)) (term (summand-list idx_1)))]
  ; for non-PLUS (natural or var), make a singleton list with the index itself
  [(summand-list idx) (idx)])
; convert a list of summands into a possibly-nested PLUS index
; result should have at most one natural and other summands in standard order
(define-metafunction Dependent
  summands->idx : (idx ...) -> idx
  [(summands->idx (idx ...))
   ; produce a form like (PLUS var1 (PLUS var2 ... (PLUS varn natural) ...))
   ,(foldr (λ (idxs v) (term (PLUS ,idxs ,v)))
           ; sum of all the naturals in the idx list
           (apply + (filter exact-nonnegative-integer? (term (idx ...))))
           ; lex-ordered list of all vars in the idx list
           (sort (filter symbol? (term (idx ...)))
                 (λ (l r) 
                   (string<? (symbol->string l) (symbol->string r)))))])


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
 
 ; annotated array
 (check-equal? (judgment-holds (type-of () () () (A Bool [2] [#f #t]) type)
                               type)
               (term ((Array (S 2) Bool))))
 ; empty array
 (check-equal? (judgment-holds (type-of () () () (A Bool [0 2] []) type)
                               type)
               (term ((Array (S 0 2) Bool))))
 
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
   (type-of () () () (λ ([x (Array (S) Num)]) (A () (3))) type)
   type)
  (term (((Array (S) Num) -> (Array (S) Num)))))
 
 ; multiargument λ term
 (check-equal?
  (judgment-holds
   (type-of () () () (λ ([x (Array (S 3) Num)]
                         [y (Array (S 1) Bool)]) (A () (3))) type)
   type)
  (term (((Array (S 3) Num) (Array (S 1) Bool) -> (Array (S) Num)))))
 
 ; applying the simple λ term
 (check-equal?
  (judgment-holds
   (type-of () () () ((A () ((λ ([x (Array (S) Num)]) (A () (3)))))
                      (A () (4)))
            type)
   type)
  (term ((Array (S) Num))))
 
 ; applying the multiargument λ term
 (check-equal?
  (judgment-holds
   (type-of () () () ((A () ((λ ([x (Array (S 3) Num)]
                                 [y (Array (S 1) Bool)])
                               (A () (3)))))
                      (A (3) (1 2 3)) (A (1) (#f))) type)
   type)
  (term ((Array (S) Num))))
 
 ; applying to arg with equivalent-but-not-identical type
 (check-equal?
  (judgment-holds
   (type-of () () () ((A () ((λ ([x (Array (S 2 3) Num)]) x)))
                      (A (2) ((A (3) (1 2 3))
                              (A (3) (4 5 6)))))
            type)
   type)
  (term ((Array (S 2 3) Num))))
 
 ; applying a nested array of functions
 (check-equal?
  (judgment-holds
   (type-of [] [] [] ((A (3) [(A (2) [(λ ([x (Array (S) Num)]) (A () (1)))
                                      (λ ([x (Array (S) Num)]) (A () (2)))])
                              (A (2) [(λ ([x (Array (S) Num)]) (A () (3)))
                                      (λ ([x (Array (S) Num)]) (A () (4)))])
                              (A (2) [(λ ([x (Array (S) Num)]) (A () (5)))
                                      (λ ([x (Array (S) Num)]) (A () (6)))])])
                      (A (3) (7 23523 245)))
            type)
   type)
  (term ((Array (S 3 2) Num))))
 
 ; type abstraction, and use the bound type variable
 (check-equal?
  (judgment-holds
   (type-of [] [] []
            (T-λ [elt] (A [] [(λ ([x (Array (S) elt)]) x)]))
            type) type)
  (term ((∀ [elt] (Array (S)
                         ((Array (S) elt) -> (Array (S) elt)))))))
 ; type abstraction, but use a free type variable (makes an ill-formed type)
 (check-equal?
  (judgment-holds
   (type-of [] [] []
            (T-λ [elt] (A [] [(λ ([x (Array (S) foo)]) x)]))
            type) type)
  '())
 
 ; type application
 (check-equal?
  (judgment-holds
   (type-of [] [] []
            (T-APP (T-λ [elt] (A [] [(λ ([x (Array (S) elt)]) x)]))
                   Bool)
            type)
   type)
  (term ((Array (S) ((Array (S) Bool) -> (Array (S) Bool))))))
 
 ; index application
 (check-equal?
  (judgment-holds
   (type-of [] [] [(op (Π ([d1 Nat]) ((Array (S d1) Num)
                                      -> (Array (S 1 d1) Num))))]
            (I-APP (A () [op]) 3)
            type)
   type)
  (term ((Array (S) [(Array (S 3) Num) -> (Array (S 1 3) Num)]))))
 
 
 ; index abstraction
 (check-equal?
  (judgment-holds (type-of [][][]
                           (I-λ [(d Nat)]
                                (A [] [(λ [(l (Array (S d) Num))] l)]))
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
   (type-of [] [] [] (UNPACK ([l c] ⇐
                                    (PACK 3 (A [3] [1 2 3])
                                          (Σ [(l Nat)] (Array (S l) Num))))
                             (A [] [0])) type)
   type)
  (term ((Array (S) Num))))
 
 ; creation of a dependent sum
 (check-equal?
  (judgment-holds
   (type-of [][][]
            (PACK 3 (A (3) (0 1 2)) (Σ [(d Nat)] (Array (S d) Num)))
            type)
   type)
  (term ((Σ [(d Nat)] (Array (S d) Num)))))
 
 ; typing compose makes use of typechecker code for handling abstracted indices
 (check-equal?
  (judgment-holds
   (type-of [] [] []
            (I-λ [(s1 Shape) (s2 Shape) (s3 Shape)]
                 (T-λ [α β γ]
                      (A [] [(λ [(f (Array (S) ((Array s1 α) -> (Array s2 β))))
                                 (g (Array (S) ((Array s2 β) -> (Array s3 γ))))]
                               (A [] [(λ [(x (Array s1 α))] (g (f x)))]))])))
            type) type)
  (term ((Π [(s1 Shape) (s2 Shape) (s3 Shape)]
            (∀ [α β γ]
               (Array (S)
                      ((Array (S) ((Array s1 α) -> (Array s2 β)))
                       (Array (S) ((Array s2 β) -> (Array s3 γ)))
                       -> (Array (S) ((Array s1 α) -> (Array s3 γ))))))))))
 
 ; so does typing a fork composition
 (check-equal?
  (judgment-holds
   (type-of
    [] [] []
    (I-λ [(s-li Shape) (s-lo Shape) (s-ri Shape) (s-ro Shape) (s-jo Shape)]
         (T-λ [t-li t-lo t-ri t-ro t-jo]
              (A [] [(λ ([f-l (Array (S) ((Array s-li t-li)
                                          -> (Array s-lo t-lo)))]
                         [f-r (Array (S) ((Array s-ri t-ri)
                                          -> (Array s-ro t-ro)))]
                         [f-j (Array (S) ((Array s-lo t-lo)
                                          (Array s-ro t-ro)
                                          -> (Array s-jo t-jo)))])
                       (A [] [(λ [(x (Array s-li t-li))
                                  (y (Array s-ri t-ri))]
                                (f-j (f-l x) (f-r y)))]))])))
    type) type)
  (term ((Π [(s-li Shape) (s-lo Shape) (s-ri Shape) (s-ro Shape) (s-jo Shape)]
            (∀ [t-li t-lo t-ri t-ro t-jo]
               (Array (S) ((Array (S) ((Array s-li t-li)
                                       -> (Array s-lo t-lo)))
                           (Array (S) ((Array s-ri t-ri)
                                       -> (Array s-ro t-ro)))
                           (Array (S) ((Array s-lo t-lo)
                                       (Array s-ro t-ro)
                                       -> (Array s-jo t-jo)))
                           -> (Array (S) ((Array s-li t-li)
                                          (Array s-ri t-ri)
                                          -> (Array s-jo t-jo))))))))))
 
 ;-------------------
 ; type and index well-formedness
 ;-------------------
 (check-true
  (judgment-holds (kind-of [] [] (Array (S 4 3) Num))))
 (check-true
  (judgment-holds (kind-of [] [] ((Array (S 4 3) Num) -> Bool))))
 (check-true
  (judgment-holds (kind-of ([sh Shape]) [] ((Array sh Num) -> Bool))))
 (check-true
  (judgment-holds (kind-of ([sh Shape]) [] (× (Array sh Num) Bool))))
 (check-true
  (judgment-holds (kind-of ([sh Shape]) [] (Array sh (× Num Bool)))))
 (check-true
  (judgment-holds (kind-of [] [] (Π ([sh Shape]) ((Array sh Num) -> Bool)))))
 (check-true
  (judgment-holds (kind-of [] []
                           (Π ([sh Shape]) ((Array sh Num) -> Bool)))))
 (check-true
  (judgment-holds (kind-of [] []
                           (Π ([len Nat]) ((Array (S 3 len 2) Num) -> Bool)))))
 (check-true
  (judgment-holds (kind-of [] []
                           (∀ (elt)
                              (Π ([fr Shape])
                                 ((Array fr elt) -> (Array fr elt)))))))
 ; can tell by position whether a variable indicates a type or an index
 (check-true
  (judgment-holds (kind-of [(x Shape)] [(x ★)]
                           (Array x x))))
 ; free index variable
 (check-false
  (judgment-holds (kind-of [] [] (Array sh Num))))
 ; free type variable
 (check-false
  (judgment-holds (kind-of [] [] (Array (S) elt))))
 
 
 
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

