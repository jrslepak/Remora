#lang racket/base

(require "syntax.rkt"
         "semantics.rkt"
         "reader.rkt"
         "basis-lib.rkt"
         (for-syntax racket/base
                     syntax/parse)
         (rename-in (only-in racket/base #%module-begin)
                    [#%module-begin #%racket-module-begin]))

(provide (all-from-out "syntax.rkt"
                       "semantics.rkt"
                       "basis-lib.rkt")
         (rename-out [remora-module-begin #%module-begin]
                     [remora-top-interaction #%top-interaction])
         #%racket-module-begin
         (except-out (all-from-out racket/base)
                     #%module-begin
                     #%top-interaction))

(current-read-interaction remora-read-syntax)

(define-syntax (remora-module-begin stx)
  (syntax-parse stx
    [(_ body ...)
     #'(#%racket-module-begin (remora body) ...)]))

(define-syntax (remora-top-interaction stx)
  (syntax-parse stx
    [(_ body ...)
     #'(remora (body ...))]))
