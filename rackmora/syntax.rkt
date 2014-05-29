#lang racket/base

(require "semantics.rkt"
         syntax/parse
         (for-syntax "semantics.rkt"
                     syntax/parse
                     racket/base))

(provide Rλ)




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


(define-syntax (Rλ stx)
  (syntax-parse stx
    [(_ ((var:id rank:expr) ...) body ...+)
    #'(rem-proc (λ (var ...) body ...)
                 (list rank ...))]))

