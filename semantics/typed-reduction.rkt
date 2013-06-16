#lang racket

(require redex
         "dependent-lang.rkt")

(define-extended-language Annotated Dependent
  ; fully-annotated expression forms
  (ann-expr (ann-expr ann-expr ... : type)
            ann-arr
            ann-var
            (T-λ [var ...] ann-expr : type)
            (T-APP ann-expr type ... : type)
            (PACK idx ... ann-expr : type)
            (UNPACK ([var ... var] ⇐ ann-expr) ann-expr : type)
            (I-λ [(var sort) ...] ann-expr : type)
            (I-APP ann-expr idx ... : type))
  ; add type annotation to variable/array
  (ann-var (var : type))
  ; 1st type (if present) describes el-exprs
  ; 2nd type describes entire array
  (ann-arr (A type (num ...) (el-expr ...) : type)
           (A (num ...) (el-expr ...) : type)))

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
  [(well-kinded sort-env el-expr) ,(judgment-holds (sort-of sort-env idx type))])

; use sort-of judgment to identify the unique sort that matches a given idx
(define-metafunction Dependent
  unique-sort-of : sort-env idx -> sort or #f
  [(unique-sort-of sort-env el-expr)
   type_result
   (where (type_result)
          ,(judgment-holds (sort-of sort-env idx type)
                           type))]
  [(unique-sort-of sort-env idx) #f])