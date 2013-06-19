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
  (el-expr/t expr/t
             base
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
  [(well-kinded sort-env el-expr)
   ,(judgment-holds (sort-of sort-env idx type))])

; use sort-of judgment to identify the unique sort that matches a given idx
(define-metafunction Dependent
  unique-sort-of : sort-env idx -> sort or #f
  [(unique-sort-of sort-env el-expr)
   type_result
   (where (type_result)
          ,(judgment-holds (sort-of sort-env idx type)
                           type))]
  [(unique-sort-of sort-env idx) #f])

; add type annotations to convert from expr to expr/t
; assumes the expr is actually well-typed
; annotating the body of an abstraction requires looking up the vars it binds
(define-metafunction Annotated
  annotate : sort-env kind-env type-env el-expr -> el-expr/t
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

; drop type annotations to convert from expr/t to expr
; assumes the expr/t is actually well-typed
(define-metafunction Annotated
  type-erase : el-expr/t -> el-expr
  [(type-erase (expr/t_fun expr/t_arg ... : type))
   ((type-erase expr/t_fun) (type-erase expr/t_arg) ...)]
  [(type-erase (var : type)) var]
  
  [(type-erase (A type_elt (num ...) (el-expr/t ...) : type_arr))
   (A type_elt (num ...) ((type-erase el-expr/t) ...))]
  [(type-erase (A (num ...) (el-expr/t_0 el-expr/t_1 ...) : type_arr))
   (A (num ...) ((type-erase el-expr/t_0) (type-erase el-expr/t_1) ...))]
  ; if the array has no elements, we must identify the element type and
  ; put an element annotation for it
  [(type-erase (A (num ...) (el-expr/t ...) : type))
   (A type_elt (num ...) ((type-erase el-expr/t) ...))
   (where (Array (S num ... num_extras ...) type_atom) (canonicalize-type type))
   (where type_elt (canonicalize-type (Array (S num_extras ...) type_atom)))]
  
  [(type-erase (T-λ [var ...] expr/t : type))
   (T-λ [var ...] (type-erase expr/t))]
  [(type-erase (T-APP expr/t type_arg ... : type))
   (T-APP (type-erase expr/t) type_arg ...)]
  
  [(type-erase (PACK idx ... expr/t : type))
   (PACK idx ... (type-erase expr/t) type)]
  [(type-erase (UNPACK ([var_witness ... var_contents] ⇐ expr/t_sum)
                       expr/t_body : type))
   (UNPACK ([var_witness ... var_contents] ⇐ (type-erase expr/t_sum))
           (type-erase expr/t_body))]
  
  [(type-erase (I-λ [(var sort) ...] expr/t : type))
   (I-λ [(var sort) ...] (type-erase expr/t))]
  [(type-erase (I-APP expr/t idx ... : type))
   (I-APP (type-erase expr/t) idx ...)]
  
  [(type-erase (λ [(var type_arg) ...] expr/t : type_fun))
   (λ [(var type_arg) ...] (type-erase expr/t))]
  [(type-erase op) op]
  [(type-erase base) base])

(module+
 test
 (require rackunit)
 
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
