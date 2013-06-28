#lang racket

(require redex
         "dependent-lang.rkt"
         "language.rkt"
         "redex-utils.rkt")

(define-extended-language Annotated Dependent
  ; fully-annotated expression forms
  (expr:t (expr:t expr:t ... : type)
            var:t
            arr:t
            (T-λ [var ...] expr:t : type)
            (T-APP expr:t type ... : type)
            (PACK idx ... expr:t : type)
            (UNPACK ([var ... var] ⇐ expr:t) expr:t : type)
            (I-λ [(var sort) ...] expr:t : type)
            (I-APP expr:t idx ... : type))
  ; add type annotation to variable/array
  (var:t (var : type))
  ; 1st type (if present) describes el-exprs
  ; 2nd type describes entire array
  (arr:t (A type (num ...) (el-expr:t ...) : type)
         (A (num ...) (el-expr:t ...) : type))
  (el-expr:t expr:t
             base
             fun:t)
  (elt:t base
         fun:t)
  (fun:t op
         (λ [(var type) ...] expr:t : type))
  
  (expr:t-env (e:t-bind ...)) (e:t-bind [var expr:t])
  
  (val:t elt:t
         (A (num ...) (elt:t ...) : type)
         (PACK idx ... val:t : type))
  
  (E hole
     (E expr:t ... : type)
     (val:t ... E expr:t ... : type)
     (A (num ...) (val:t ... E el-expr:t ...) : type)
     (PACK idx ... E : type)
     (UNPACK ([var ... var] ⇐ E) expr:t : type)))

(define ->Typed
  (reduction-relation
   Annotated
   #:domain expr:t
   [--> (in-hole E ((A [] [op] : (Array (S) (type_arg ... -> type_ret)))
                    val:t ... : type_e))
        (in-hole E (annotate/cl (apply-op op [(type-erase val:t) ...])))
        (where (type_arg/canon ...) ((canonicalize-type type_arg) ...))
        (where (type_val/canon ...)
               ((canonicalize-type (extract-annotation val:t)) ...))
        (side-condition (equal? (term (type_arg/canon ...))
                                (term (type_val/canon ...))))
        op]
   [--> (in-hole E ((A [] [(λ [(var type_arg) ...] expr:t : type_fun)]
                       : type_arr)
                    val:t ... : type_app))
        (in-hole E (expr:t/expr:t-sub [(var val:t) ...] expr:t))
        (where (type_arg/canon ...) ((canonicalize-type type_arg) ...))
        (where (type_val/canon ...)
               ((canonicalize-type (extract-annotation val:t)) ...))
        (side-condition (equal? (term (type_arg/canon ...))
                                (term (type_val/canon ...))))
        apply]
   [--> (in-hole E ((A [num_f ...] [fun:t ...]
                       : (Array (S num_f ...) (type_arg ... -> type_ret)))
                    val:t ...
                    : (Array (S num_f ... num_c ...) type_app)))
        (in-hole E (A [num_f ...]
                      [((A [] [fun:t] : (Array (S) (type_arg ... -> type_ret)))
                        arr:t_cell ... : type_app/c) ...]
                      : (Array (S num_f ...) type_app/c)))
        (side-condition (< 0 (length (term (num_f ...)))))
        (where type_app/c (canonicalize-type (Array (S num_c ...) type_app)))
        (side-condition (term (equiv-type? (Array (S) type_ret) type_app/c)))
        (side-condition
         (term (all ((equiv-type? (extract-annotation val:t)
                                  (Array (S num_f ...) type_arg)) ...))))
        (where ((Array idx_c type_argelt) ...) (type_arg ...))
        (where (S num_c ...) (unique-elt (idx_c ...)))
        (where ((arr:t_cell ...) ...)
               (transpose/m
                ((cells/shape:t (num_c ...) val:t) ...)))
        map]
   [--> (in-hole E ((A [num_f ...] [fun:t ...]
                      : (Array (S num_f ...) (type_arg ... -> type_ret)))
                    val:t ...
                    : (Array (S num_app ...) type_app)))
        (in-hole E (arr:t_lifted val:t_lifted ...
                                 : (Array (S num_app ...) type_app)))
        ; identify cell shape for fun/args
        (where ((Array (S num_argcell ...) type_argelt) ...)
               ((canonicalize-type (Array (S) type_arg)) ...))
        ; make sure frame ranks aren't all the same
        ; (for well-typed code, equivalent to "shapes aren't all the same")
        (where ((Array (S num_argframe ... num_argcell ...) type_elt) ...)
               ((canonicalize-type (extract-annotation val:t)) ...))
        (side-condition
         (not (term (all-equal? ((num_f ...) (num_argframe ...) ...)))))
        ; duplicate the cells
        (where (arr:t_lifted val:t_lifted ...)
               (frame-lift:t
                [(0 (A [num_f ...] [fun:t ...]
                                   : (Array (S num_f ...)
                                            (type_arg ... -> type_ret))))
                 ((length/m (num_argcell ...)) val:t) ...]))
        lift]
   [--> (in-hole E (A [num_f ...] [(A [num_c ...] [elt:t ...] : type_c) ...]
                      : type_f))
        (in-hole E (A [num_f ... num_c0 ...] [any_v ...]
                      : (canonicalize-type type_f)))
        (where (num_c0 ...) (unique-elt ((num_c ...) ...)))
        (where (any_v ...) ,(foldr append '() (term ((elt:t ...) ...))))
        collapse]
   [--> (in-hole E (T-APP (T-λ [var ...] expr:t : type_abst)
                          type_arg ...
                          : type_app))
        (in-hole E (type/expr:t-sub ([var type_arg] ...) expr:t))
        type-apply]
   [--> (in-hole E (I-APP (I-λ [(var sort) ...] expr:t : type_abst)
                          idx_arg ...
                          : type_app))
        (in-hole E (idx/expr:t-sub ([var idx_arg] ...) expr:t))
        idx-apply]))

;; grow argument arrays by duplication so they all have their desired ranks
;; cell ranks must be naturalized
(define-metafunction Annotated
  ; [(cell-rank array) ...]
  frame-lift:t : [(num arr:t) ...] -> (arr:t ...) or #f
  [(frame-lift:t []) ()]
  ; make sure arrays can be lifted into same frame
  ; (need prefix relation for frame shapes)
  ; "principal frame" comes from least-overranked array
  [(frame-lift:t [(num_cr arr:t) ...])
   ((cell-dup:t num_cr (num_pr-frame-dim ...) arr:t) ...)
   ; extract frame shapes
   (where ((num_fr ...) ...)
          ((drop-right/m (shape:t arr:t) num_cr) ...))
   ; find the longest one -- that is the principal frame
   (where (num_pr-frame-dim ...) (longest ((num_fr ...) ...)))
   ; all other frames must be prefixes of it
   (side-condition
    (term (all ((prefix? (num_fr ...) (num_pr-frame-dim ...)) ...))))]
  ; not a frame-liftable input (e.g. due to frame mismatch)
  [(frame-lift:t any) #f])

;; extract shape of array
(define-metafunction Annotated
  shape:t : arr:t -> (num ...)
  [(shape:t (A (num ...) (el-expr:t ...) : type)) (num ...)])

;; extract rank of array
(define-metafunction Annotated
  rank:t : arr:t -> num
  [(rank:t (A (num ...) (el-expr:t ...) : type))
   ,(length (term (num ...)))])

;; duplicate cells of given array to lift it into desired frame
(define-metafunction Annotated
  ; cell rank, frame shape, initial array
  cell-dup:t : num (num ...) arr:t -> arr:t
  ; All elements of a single cell should appear consecutively in value segment
  ; Just split value into chunks, repeat chunks right number of times, and
  ; update the shape.
  [(cell-dup:t num_cell-rank (num_frame-dim ...) arr:t)
   ; new array's shape is frame-portion ++ growth-portion ++ cell-shape
   ; new array's value comes from repeating the cells (number of copies is
   ; product of the "growth" portion of the shape)
   (A ,(append (term (drop-right/m (shape:t arr:t) num_cell-rank))
               (term (num_growth ...))
               (term (take-right/m (shape:t arr:t) num_cell-rank)))
      ,(foldr append '()
              (term ((repeat ,(foldr * 1 (term (num_growth ...)))
                             (el-expr:t_cell ...)) ...)))
      : (Array (S num_frame-dim ... num_cell-dim ...) type_atom))
   ; constructing the result annotation based on the original annotation
   (where (Array (S num_orig ...) type_atom)
          (extract-annotation arr:t))
   (where (num_cell-dim ...)
          (take-right/m (num_orig ...) num_cell-rank))
   ; break the array's value segment into its cells
   (where ((el-expr:t_cell ...) ...)
          (cell-values:t (take-right/m (shape:t arr:t) num_cell-rank) arr:t))
   ; identify the part of the result shape that comes from lifting
   ; drop frame portion of array from left side of frame
   (where (num_growth ...)
          (drop/m (num_frame-dim ...)
                  ,(- (term (rank:t arr:t)) (term num_cell-rank))))
   ; require that the array actually be liftable into the frame
   ; i.e. frame portion of array must be prefix of given frame
   (side-condition (term (prefix? (drop-right/m (shape:t arr:t) num_cell-rank)
                                  (num_frame-dim ...))))])

;; extract the value segments of an array's cells
(define-metafunction Annotated
  ; cell shape, array
  cell-values:t : (num ...) arr:t -> ((el-expr:t ...) ...)
  [(cell-values:t (num_cellshape ...) arr:t)
   ((el-expr:t ...) ...)
   (where ((A (num ...) (el-expr:t ...) : type) ...)
          (cells/shape:t (num_cellshape ...) arr:t))])

;; split an array into cells
(define-metafunction Annotated
  ; cell shape, array
  cells/shape:t : (num ...) arr:t -> (arr:t ...)
  [(cells/shape:t (num_cell-dim ...) (A (num_arr-dim ...) () : type)) ()]
  [(cells/shape:t (num_cell-dim ...)
                  (A (num_arr-dim ...) (el-expr:t ...) : type))
   ,(cons (term (A (num_cell-dim ...) (take/m (el-expr:t ...) num_cellsize)
                   : (Array (S num_cell-dim ...) type_elt)))
          ; drop one cell's elements from array, and split remaining elements
          (term (cells/shape:t (num_cell-dim ...)
                               (A (num_arr-dim ...)
                                  (drop/m (el-expr:t ...) num_cellsize)
                                  : type))))
   (where (Array idx type_elt) (canonicalize-type type))
   (where num_cellsize ,(foldr * 1 (term (num_cell-dim ...))))])

; select the unique element from a list that repeats only that element
(define-metafunction Annotated
  unique-elt : (any any ...) -> any
  [(unique-elt (any)) any]
  [(unique-elt (any_0 any_0 any_1 ...)) (unique-elt (any_0 any_1 ...))])

; check whether two types are equivalent
(define-metafunction Dependent
  equiv-type? : type type -> bool
  [(equiv-type? type_0 type_1)
   #t
   (side-condition (equal? (term (canonicalize-type type_0))
                           (term (canonicalize-type type_1))))]
  [(equiv-type? type_0 type_1) #f])

; use type-of judgment to identify the unique type that matches a given expr
(define-metafunction Dependent
  unique-type-of : sort-env kind-env type-env el-expr -> type or #f
  [(unique-type-of sort-env kind-env type-env el-expr)
   type_result
   (where (type_result)
          ,(judgment-holds (type-of sort-env kind-env type-env el-expr type)
                           type))]
  [(unique-type-of sort-env kind-env type-env el-expr) #f])


; use kind-of judgment to determine whether a given type is well-formed
(define-metafunction Dependent
  well-kinded : sort-env kind-env type -> bool
  [(well-kinded sort-env el-expr)
   ,(judgment-holds (sort-of sort-env idx type))])

; use sort-of judgment to identify the unique sort that matches a given idx
(define-metafunction Dependent
  unique-sort-of : sort-env idx -> sort or #f
  [(unique-sort-of sort-env idx)
   sort_result
   (where (sort_result)
          ,(judgment-holds (sort-of sort-env idx sort)
                           sort))]
  [(unique-sort-of sort-env idx) #f])

; add type annotations to convert from expr to expr:t
; assumes the expr is actually well-typed
; annotating the body of an abstraction requires looking up the vars it binds
(define-metafunction Annotated
  annotate : sort-env kind-env type-env el-expr -> el-expr:t
  [(annotate sort-env kind-env type-env (expr_fun expr_arg ...))
   ((annotate sort-env kind-env type-env expr_fun)
    (annotate sort-env kind-env type-env expr_arg) ... : type)
   (where type (unique-type-of sort-env kind-env type-env
                               (expr_fun expr_arg ...)))]
  [(annotate sort-env kind-env type-env var)
   (var : type)
   (where type (unique-type-of sort-env kind-env type-env var))]
  
  [(annotate sort-env kind-env type-env (A (num ...) (el-expr ...)))
   (A (num ...) ((annotate sort-env kind-env type-env el-expr) ...) : type)
   (where type (unique-type-of sort-env kind-env type-env
                               (A (num ...) (el-expr ...))))]
  [(annotate sort-env kind-env type-env (A type_elt (num ...) (el-expr ...)))
   (A (num ...) ((annotate sort-env kind-env type-env el-expr) ...) : type)
   (where type (unique-type-of sort-env kind-env type-env
                               (A type_elt (num ...) (el-expr ...))))]
  
  [(annotate sort-env kind-env type-env (T-λ [var ...] expr))
   (T-λ [var ...] (annotate sort-env
                            (kind-env-update [var ★] ... kind-env)
                            type-env
                            expr) : type)
   (where type (unique-type-of sort-env kind-env type-env
                               (T-λ [var ...] expr)))]
  [(annotate sort-env kind-env type-env (T-APP expr type_arg ...))
   (T-APP (annotate sort-env kind-env type-env expr) type_arg ... : type)
   (where type (unique-type-of sort-env kind-env type-env
                               (T-APP expr type_arg ...)))]
  
  [(annotate sort-env kind-env type-env (I-λ [(var sort) ...] expr))
   (I-λ [(var sort) ...]
        (annotate (sort-env-update (var sort) ... sort-env)
                  kind-env type-env
                  expr) : type)
   (where type (unique-type-of sort-env kind-env type-env
                               (I-λ [(var sort) ...] expr)))]
  [(annotate sort-env kind-env type-env (I-APP expr idx ...))
   (I-APP (annotate sort-env kind-env type-env expr) idx ... : type)
   (where type (unique-type-of sort-env kind-env type-env
                               (I-APP expr idx ...)))]
  
  [(annotate sort-env kind-env type-env (λ [(var type_arg) ...] expr))
   (λ [(var type_arg) ...]
     (annotate sort-env kind-env
               (type-env-update (var type_arg) ... type-env)
               expr) : type)
   (where type (unique-type-of sort-env kind-env type-env
                               (λ [(var type_arg) ...] expr)))]
  [(annotate sort-env kind-env type-env op) op]
  [(annotate sort-env kind-env type-env base) base])
; specialized version for closed terms
(define-metafunction Annotated
  annotate/cl : el-expr -> el-expr:t
  [(annotate/cl el-expr) (annotate [] [] [] el-expr)])

; drop type annotations to convert from expr:t to expr
; assumes the expr:t is actually well-typed
(define-metafunction Annotated
  type-erase : el-expr:t -> el-expr
  [(type-erase (expr:t_fun expr:t_arg ... : type))
   ((type-erase expr:t_fun) (type-erase expr:t_arg) ...)]
  [(type-erase (var : type)) var]
  
  [(type-erase (A type_elt (num ...) (el-expr:t ...) : type_arr))
   (A type_elt (num ...) ((type-erase el-expr:t) ...))]
  [(type-erase (A (num ...) (el-expr:t_0 el-expr:t_1 ...) : type_arr))
   (A (num ...) ((type-erase el-expr:t_0) (type-erase el-expr:t_1) ...))]
  ; if the array has no elements, we must identify the element type and
  ; put an element annotation for it
  [(type-erase (A (num ...) (el-expr:t ...) : type))
   (A type_elt (num ...) ((type-erase el-expr:t) ...))
   (where (Array (S num ... num_extras ...) type_atom) (canonicalize-type type))
   (where type_elt (canonicalize-type (Array (S num_extras ...) type_atom)))]
  
  [(type-erase (T-λ [var ...] expr:t : type))
   (T-λ [var ...] (type-erase expr:t))]
  [(type-erase (T-APP expr:t type_arg ... : type))
   (T-APP (type-erase expr:t) type_arg ...)]
  
  [(type-erase (PACK idx ... expr:t : type))
   (PACK idx ... (type-erase expr:t) type)]
  [(type-erase (UNPACK ([var_witness ... var_contents] ⇐ expr:t_sum)
                       expr:t_body : type))
   (UNPACK ([var_witness ... var_contents] ⇐ (type-erase expr:t_sum))
           (type-erase expr:t_body))]
  
  [(type-erase (I-λ [(var sort) ...] expr:t : type))
   (I-λ [(var sort) ...] (type-erase expr:t))]
  [(type-erase (I-APP expr:t idx ... : type))
   (I-APP (type-erase expr:t) idx ...)]
  
  [(type-erase (λ [(var type_arg) ...] expr:t : type_fun))
   (λ [(var type_arg) ...] (type-erase expr:t))]
  [(type-erase op) op]
  [(type-erase base) base])

; extract an el-expr:t's type
(define-metafunction Annotated
  extract-annotation : el-expr:t -> type
  [(extract-annotation (any ... : type)) type])

; substitute an expr:t into an expr:t
(define-metafunction Annotated
  expr:t/expr:t-sub : expr:t-env el-expr:t -> el-expr:t
  [(expr:t/expr:t-sub ([var_0 expr:t_0] ... [var expr:t] [var_1 expr:t_1] ...)
                      (var : type))
   expr:t]
  [(expr:t/expr:t-sub expr:t-env var) (var : type)]
  
  [(expr:t/expr:t-sub expr:t-env (expr:t_fun expr:t_arg ... : type))
   ((expr:t/expr:t-sub expr:t-env expr:t_fun)
    (expr:t/expr:t-sub expr:t-env expr:t_arg) ... : type)]
  
  [(expr:t/expr:t-sub expr:t-env (A type_elt (num ...) (el-expr:t ...) : type))
   (A type_elt (num ...) [(expr:t/expr:t-sub expr:t-env el-expr:t) ...] : type)]
  [(expr:t/expr:t-sub expr:t-env (A (num ...) (el-expr:t ...) : type))
   (A (num ...) [(expr:t/expr:t-sub expr:t-env el-expr:t) ...] : type)]
  
  [(expr:t/expr:t-sub expr:t-env (T-λ [var ...] expr:t : type))
   (T-λ [var ...] (expr:t/expr:t-sub expr:t-env expr:t) : type)]
  [(expr:t/expr:t-sub expr:t-env (T-APP expr:t type_arg ... : type))
   (T-APP (expr:t/expr:t-sub expr:t-env expr:t) type_arg ... : type)]
  
  [(expr:t/expr:t-sub expr:t-env (PACK idx ... expr:t : type))
   (PACK idx ... (expr:t/expr:t-sub expr:t-env expr:t) : type)]
  [(expr:t/expr:t-sub
    expr:t-env (UNPACK ([var_witness ... var_contents] ⇐ expr:t_sum)
                       expr:t_body : type))
   (UNPACK ([var_witness ... var_contents]
            ⇐ (expr:t/expr:t-sub expr:t-env expr:t_sum))
           (expr:t/expr:t-sub (shadow [var_contents] expr:t-env)
                              expr:t_body) : type)]
  [(expr:t/expr:t-sub expr:t-env (I-λ [(var sort) ...] expr:t : type))
   (I-λ [(var sort) ...] (expr:t/expr:t-sub expr:t-env expr:t) : type)]
  [(expr:t/expr:t-sub expr:t-env (T-APP expr:t type_arg ... : type))
   (T-APP (expr:t/expr:t-sub expr:t-env expr:t) type_arg ... : type)]
  
  [(expr:t/expr:t-sub expr:t-env op) op]
  [(expr:t/expr:t-sub expr:t-env base) base]
  [(expr:t/expr:t-sub expr:t-env (λ [(var type_arg) ...] expr:t : type_fun))
   (λ [(var type_arg) ...]
     (expr:t/expr:t-sub (shadow (var ...) expr:t-env) expr:t) : type_fun)])

; substitute a type into an expr:t
(define-metafunction Annotated
  type/expr:t-sub : type-env el-expr:t -> el-expr:t
  [(type/expr:t-sub type-env (var : type))
   (var : (type/type-sub type-env type))]
  
  [(type/expr:t-sub type-env (expr:t_fun expr:t_arg ... : type))
   ((type/expr:t-sub type-env expr:t_fun)
    (type/expr:t-sub type-env expr:t_arg) ...
    : (type/type-sub type-env type))]
  
  [(type/expr:t-sub type-env (A type_elt (num ...) (el-expr:t ...) : type))
   (A (type/expr:t-sub type-env type_elt)
      (num ...) ((type/expr:t-sub type-env el-expr:t) ...)
      : (type/type-sub type-env type))]
  [(type/expr:t-sub type-env (A (num ...) (el-expr:t ...) : type))
   (A (num ...) ((type/expr:t-sub type-env el-expr:t) ...)
      : (type/type-sub type-env type))]
  
  [(type/expr:t-sub type-env (T-λ [var ...] expr:t : type))
   (T-λ [var ...]
        (type/expr:t-sub (shadow (var ...) type-env) expr:t)
        : (type/type-sub type-env type))]
  [(type/expr:t-sub type-env (T-APP expr:t type_arg ... : type))
   (T-APP (type/expr:t-sub type-env expr:t)
          (type/type-sub type-env type_arg) ...
          : (type/type-sub type-env type))]
  
  [(type/expr:t-sub type-env (PACK idx ... expr:t : type))
   (PACK idx ... (type/expr:t-sub type-env expr:t)
         : (type/type-sub type-env type))]
  [(type/expr:t-sub type-env
                    (UNPACK ([var_witness ... var_contents] ⇐ expr:t_sum)
                       expr:t_body : type))
   (UNPACK ([var_witness ... var_contents]
            ⇐ (type/expr:t-sub type-env expr:t_sum))
           (type/expr:t-sub type-env expr:t_body)
           : (type/type-sub type-env type))]
  
  [(type/expr:t-sub type-env (I-λ [(var sort) ...] expr:t : type))
   (I-λ [var ...] (type/expr:t-sub type-env expr:t)
        : (type/type-sub type-env type))]
  [(type/expr:t-sub type-env (I-APP expr:t idx_arg ... : type))
   (I-APP (type/expr:t-sub type-env expr:t) idx_arg ...
          : (type/type-sub type-env type))]
  
  [(type/expr:t-sub type-env op) op]
  [(type/expr:t-sub type-env base) base]
  [(type/expr:t-sub type-env (λ [(var type_arg) ...] expr:t : type_fun))
   (λ [(var (type/type-sub type-env type_arg)) ...]
     (type/expr:t-sub type-env expr:t)
     : (type/type-sub type-env type_fun))])

; substitute an index into an expr:t
(define-metafunction Annotated
  idx/expr:t-sub : idx-env el-expr:t -> el-expr:t
  [(idx/expr:t-sub idx-env (var : type)) (var : (idx/type-sub idx-env type))]
  
  [(idx/expr:t-sub idx-env (expr:t_fun expr:t_arg ... : type))
   ((idx/expr:t-sub idx-env expr:t_fun)
    (idx/expr:t-sub idx-env expr:t_arg) ...
    : (idx/type-sub idx-env type))]
  
  [(idx/expr:t-sub idx-env (A type_elt (num ...) (el-expr:t ...) : type))
   (A (idx/type-sub idx-env type_elt)
      (num ...) ((idx/expr:t-sub idx-env el-expr:t) ...)
      : (idx/type-sub idx-env type))]
  [(idx/expr:t-sub idx-env (A (num ...) (el-expr:t ...) : type))
   (A (num ...) ((idx/expr:t-sub idx-env el-expr:t) ...)
      : (idx/type-sub idx-env type))]
  
  [(idx/expr:t-sub idx-env (T-λ [var ...] expr:t : type))
   (T-λ [var ...] (idx/expr:t-sub idx-env expr:t)
        : (idx/type-sub idx-env type))]
  [(idx/expr:t-sub idx-env (T-APP expr:t type_arg ... : type))
   (T-APP (idx/expr:t-sub idx-env expr:t) (idx/type-sub idx-env type_arg) ...
          : (idx/type-sub idx-env type))]
  
  [(idx/expr:t-sub idx-env (PACK idx ... expr:t : type))
   (PACK (idx/idx-sub idx) ... (idx/expr:t-sub idx-env expr:t)
         : (idx/type-sub idx-env type))]
  [(idx/expr:t-sub idx-env (UNPACK ([var_witness ... var_contents] ⇐ expr:t_sum)
                                   expr:t_body : type))
   (UNPACK ([var_witness ... var_contents]
            ⇐ (idx/expr:t-sub idx-env expr:t_sum))
           (idx/expr:t-sub idx-env expr:t_body)
           : (idx/type-sub idx-env type))]
  
  [(idx/expr:t-sub idx-env (I-λ [(var sort) ...] expr:t : type))
   (I-λ [(var sort) ...] (idx/expr:t-sub (shadow (var ...) idx-env) expr:t)
        : (idx/type-sub idx-env type))]
  [(idx/expr:t-sub idx-env (I-APP expr:t idx ... : type))
   (I-APP (idx/expr:t-sub idx-env expr:t) (idx/idx-sub idx) ...
          : (idx/type-sub idx-env type))]
  
  [(idx/expr:t-sub idx-env op) op]
  [(idx/expr:t-sub idx-env base) base]
  [(idx/expr:t-sub idx-env (λ [(var type_arg) ...] expr:t : type_fun))
   (λ [(var (idx/type-sub idx-env type_arg)) ...]
     (idx/expr:t-sub idx-env expr:t)
     : (idx/type-sub idx-env type_fun))])

; like using traces with ->Typed, but hide the type annotations
(define (simple-trace t)
  (define t/ann
    (cond [(redex-match Annotated expr:t t) t]
          [(redex-match Annotated expr t) (term (annotate/cl ,t))]))
  (traces #:pp (λ (v port width text)
                 (default-pretty-printer
                   (term (type-erase ,v))
                   port width text))
          ->Typed t/ann))


(module+
 test
 (require rackunit)
 
 (check-equal?
  (deterministic-reduce
   ->Typed
   (term (annotate/cl ((A [] [+]) (A [] [1]) (A [] [1])))))
  (term (A [] [2] : (Array (S) Num))))
 
 (check-equal?
  (deterministic-reduce
   ->Typed
   (term (annotate/cl ((A [] [+]) (A [] [1])
                                  ((A [] [+]) (A [] [1]) (A [] [1]))))))
  (term (A [] [3] : (Array (S) Num))))
 
 (check-equal?
  (deterministic-reduce
   ->Typed
   (term (annotate/cl ((A [] [+]) ((A [] [+]) (A [] [1]) (A [] [1]))
                                  (A [] [1])))))
  (term (A [] [3] : (Array (S) Num))))
 
 (check-equal?
  (deterministic-reduce
   ->Typed
   (term (annotate/cl ((A [2] [+ -]) (A [2] [10 20]) (A [2] [3 4])))))
  (term (annotate/cl (A [2] [13 16]))))
 
 (check-equal?
  (deterministic-reduce
   ->Typed
   (term (annotate/cl ((A [] [+]) (A [2 3] [1 2 3 4 5 6]) (A [2] [10 20])))))
  (term (annotate/cl (A [2 3] [11 12 13 24 25 26]))))
 
 (check-equal?
  (deterministic-reduce
   ->Typed
   (term (annotate/cl ((A [] [+]) (A [3 2] [1 2 3 4 5 6]) (A [3] [10 20 30])))))
  (term (annotate/cl (A [3 2] [11 12 23 24 35 36]))))
 
 (check-equal?
  (deterministic-reduce
   ->Typed
   (term (annotate/cl ((A [2] [+ -]) (A [2 3] [1 2 3 4 5 6]) (A [2] [10 20])))))
  (term (annotate/cl (A [2 3] [11 12 13 -16 -15 -14]))))
 
 (check-equal?
  (deterministic-reduce
   ->Typed
   (term (annotate/cl ((A [] [(λ [(x (Array (S) Num))] x)])
                       (A [6] [1 2 3 4 5 6])))))
  (term (annotate/cl (A [6] [1 2 3 4 5 6]))))
 
 (check-equal?
  (deterministic-reduce
   ->Typed
   (term (annotate/cl ((T-APP (T-λ [l] (A [] [(λ [(x (Array (S) l))] x)])) Num)
                       (A [6] [1 2 3 4 5 6])))))
  (term (annotate/cl (A [6] [1 2 3 4 5 6]))))
 
 (check-equal?
  (deterministic-reduce
   ->Typed
   (term (annotate/cl ((I-APP (I-λ [(n Nat)]
                                   (A [] [(λ [(x (Array (S n) Num))]
                                            ((A [] [+]) (A [] [1]) x))])) 3)
                       (A [3] [20 30 40])))))
  (term (annotate/cl (A [3] [21 31 41]))))
 
 (check-equal?
  (deterministic-reduce
   ->Typed
   (term (annotate/cl ((I-APP (I-λ [(n Nat)]
                                   (A [] [(λ [(x (Array (S n) Num))]
                                            ((A [] [+]) (A [] [1]) x))])) 3)
                       (A [2 3] [20 30 40 500 600 700])))))
  (term (annotate/cl (A [2 3] [21 31 41 501 601 701]))))
 
 (check-equal?
  (term (annotate [][][] ((A [] [+]) (A Num [2] [1 3]) (A [] [4]))))
  (term ((A [] [+] : (Array (S) ((Array (S) Num)
                                 (Array (S) Num)
                                 -> (Array (S) Num))))
         (A [2] [1 3] : (Array (S 2) Num))
         (A [] [4]: (Array (S) Num))
         : (Array (S 2) Num))))
 
 (check-equal?
  (term (annotate
         [][][]
         (I-λ [(s1 Shape) (s2 Shape) (s3 Shape)]
              (T-λ [α β γ]
                   (A [] [(λ [(f (Array (S) ((Array s1 α) -> (Array s2 β))))
                              (g (Array (S) ((Array s2 β) -> (Array s3 γ))))]
                            (A [] [(λ [(x (Array s1 α))] (g (f x)))]))])))))
 (term (I-λ [(s1 Shape) (s2 Shape) (s3 Shape)]
            (T-λ [α β γ]
                 (A [] [(λ [(f (Array (S) ((Array s1 α) -> (Array s2 β))))
                            (g (Array (S) ((Array s2 β) -> (Array s3 γ))))]
                          (A [] [(λ [(x (Array s1 α))]
                                   ([g : (Array (S) ((Array s2 β)
                                                     -> (Array s3 γ)))]
                                    ([f : (Array (S) ((Array s1 α)
                                                      -> (Array s2 β)))]
                                     [x : (Array s1 α)]
                                     : (Array s2 β))
                                    : (Array s3 γ))
                                   : ((Array s1 α) -> (Array s3 γ)))]
                             : (Array (S) ((Array s1 α) -> (Array s3 γ))))
                          : ((Array (S) ((Array s1 α) -> (Array s2 β)))
                             (Array (S) ((Array s2 β) -> (Array s3 γ)))
                             -> (Array (S) ((Array s1 α) -> (Array s3 γ)))))]
                    : (Array (S)
                             ((Array (S) ((Array s1 α) -> (Array s2 β)))
                              (Array (S) ((Array s2 β) -> (Array s3 γ)))
                              -> (Array (S) ((Array s1 α) -> (Array s3 γ))))))
                 : (∀ [α β γ]
                      (Array (S)
                             ((Array (S) ((Array s1 α) -> (Array s2 β)))
                              (Array (S) ((Array s2 β) -> (Array s3 γ)))
                              -> (Array (S) ((Array s1 α) -> (Array s3 γ)))))))
            : (Π [(s1 Shape) (s2 Shape) (s3 Shape)]
                 (∀ [α β γ]
                    (Array (S)
                           ((Array (S) ((Array s1 α) -> (Array s2 β)))
                            (Array (S) ((Array s2 β) -> (Array s3 γ)))
                            -> (Array (S) ((Array s1 α) -> (Array s3 γ))))))))))
 
 (check-equal?
  (term (type-erase (A (3) [(A (2) [1 2] : (Array (S 2) Num))
                            (A (2) [3 4] : (Array (S 2) Num))
                            (A (2) [5 6] : (Array (S 2) Num))]
                       : (Array (S 3) (Array (S 2) Num)))))
  (term (A [3] [(A [2] [1 2])
                (A [2] [3 4])
                (A [2] [5 6])])))
 
 (check-equal?
  (term
   (type-erase
    (I-λ [(s1 Shape) (s2 Shape) (s3 Shape)]
         (T-λ [α β γ]
              (A [] [(λ [(f (Array (S) ((Array s1 α) -> (Array s2 β))))
                         (g (Array (S) ((Array s2 β) -> (Array s3 γ))))]
                       (A [] [(λ [(x (Array s1 α))]
                                ([g : (Array (S) ((Array s2 β)
                                                  -> (Array s3 γ)))]
                                 ([f : (Array (S) ((Array s1 α)
                                                   -> (Array s2 β)))]
                                  [x : (Array s1 α)]
                                  : (Array s2 β))
                                 : (Array s3 γ))
                                : ((Array s1 α) -> (Array s3 γ)))]
                          : (Array (S) ((Array s1 α) -> (Array s3 γ))))
                       : ((Array (S) ((Array s1 α) -> (Array s2 β)))
                          (Array (S) ((Array s2 β) -> (Array s3 γ)))
                          -> (Array (S) ((Array s1 α) -> (Array s3 γ)))))]
                 : (Array (S)
                          ((Array (S) ((Array s1 α) -> (Array s2 β)))
                           (Array (S) ((Array s2 β) -> (Array s3 γ)))
                           -> (Array (S) ((Array s1 α) -> (Array s3 γ))))))
              : (∀ [α β γ]
                   (Array (S)
                          ((Array (S) ((Array s1 α) -> (Array s2 β)))
                           (Array (S) ((Array s2 β) -> (Array s3 γ)))
                           -> (Array (S) ((Array s1 α) -> (Array s3 γ)))))))
         : (Π [(s1 Shape) (s2 Shape) (s3 Shape)]
              (∀ [α β γ]
                 (Array (S)
                        ((Array (S) ((Array s1 α) -> (Array s2 β)))
                         (Array (S) ((Array s2 β) -> (Array s3 γ)))
                         -> (Array (S) ((Array s1 α) -> (Array s3 γ))))))))))
  (term (I-λ [(s1 Shape) (s2 Shape) (s3 Shape)]
             (T-λ [α β γ]
                  (A [] [(λ [(f (Array (S) ((Array s1 α) -> (Array s2 β))))
                             (g (Array (S) ((Array s2 β) -> (Array s3 γ))))]
                           (A [] [(λ [(x (Array s1 α))]
                                    (g (f x)))]))]))))))
