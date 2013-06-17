#lang racket

(require redex
         "dependent-lang.rkt")

(define-extended-language Annotated Dependent
  ; fully-annotated expression forms
  (expr/t (expr/t expr/t ... : type)
            var/t
            arr/t
            (T-λ [var ...] expr/t : type)
            (T-APP expr/t type ... : type)
            (PACK idx ... expr/t : type)
            (UNPACK ([var ... var] ⇐ expr/t) expr/t : type)
            (I-λ [(var sort) ...] expr/t : type)
            (I-APP expr/t idx ... : type))
  ; add type annotation to variable/array
  (var/t (var : type))
  ; 1st type (if present) describes el-exprs
  ; 2nd type describes entire array
  (arr/t (A type (num ...) (el-expr/t ...) : type)
         (A (num ...) (el-expr/t ...) : type))
  (el-expr/t base
             fun/t)
  (fun/t op
         (λ [(var type) ...] expr/t : type)))

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