#lang racket/base

(require "semantics.rkt"
         syntax/parse
         (for-syntax "semantics.rkt"
                     syntax/parse
                     racket/base))

(provide Iλ IApp Rλ check-sort)




(begin-for-syntax
  (define-syntax-class idx
    #:description "Remora index"
    (pattern (+ idx:idx ...+))
    (pattern (append idx:idx ...+))
    (pattern var:id)
    (pattern nat:number))
  (define-syntax-class isort
    #:description "index sort"
    (pattern #'Nat)
    (pattern #'Shape)))

(define (check-sort i s)
  (define well-sorted?
    (cond [(equal? 'Nat s) (nat-idx? i)]
          [(equal? 'Shape s) (shape-idx? i)]
          [else (error "invalid index sort: " s)]))
  (unless well-sorted? (error "index does not match sort: " i s)))

(define-syntax (Iλ stx)
  (syntax-parse stx
    [(_ ((var:id sort:isort) ...) body ...+)
     #'(λ (var ...) (check-sort var sort) ... body ...)]))

(define-syntax (IApp stx)
  (syntax-parse stx
    [(_ fun:expr arg:idx ...)
     #'(fun arg ...)]))

(define-syntax (Rλ stx)
  (syntax-parse stx
    [(_ ((var:id type:expr) ...) body ...+)
    #'(rem-proc (λ (var ...) body ...)
                 (list type ...))]))

;; TODO: whole-module binding check ("for now," can just assume everything's OK)
;; local-expand the module to Racket core forms plus Iλ and IApp
;; how to deal with imported Remora functions?
