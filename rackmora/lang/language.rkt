#lang racket/base

(require racket/provide
         racket/require
         "syntax.rkt"
         (except-in "semantics.rkt"
                    list->array)
         ;"basis-lib.rkt"
         (for-syntax racket/base
                     syntax/parse)
         (rename-in (only-in racket/base #%module-begin)
                    [#%module-begin #%racket-module-begin]))

;;; Take all Remora primitive operations from the basis library, stripping the
;;; "R_" prefix
(require (filtered-in (λ (name)
                        (define new-name
                          (if (regexp-match #rx"^R_" name)
                              (regexp-replace #rx"^R_" name "")
                              name))
                        #;(printf "got ~v from basis lib, providing ~v\n"
                                name new-name)
                        new-name)
                      "basis-lib.rkt"))

;;; Take everything from racket/base that doesn't have the same name as a
;;; (prefix stripped) Remora primop or anything else from Remora's internals
(require (subtract-in racket/base
                      (filtered-in
                       (λ (name)
                         (define new-name
                           (if (regexp-match #rx"^R_" name)
                               (regexp-replace #rx"^R_" name "")
                               name))
                         #;(printf "got ~v from basis lib, providing ~v\n"
                                 name new-name)
                         new-name)
                       "basis-lib.rkt")
                      "syntax.rkt"
                      "semantics.rkt"))
;;; Prefix the reader's exports so they don't conflict with things from
;;; racket/base or the Remora basis library
(require (filtered-in
          (λ (name) (string-append "READER_" name))
          "reader.rkt"))

(provide (all-from-out "syntax.rkt"
                       "semantics.rkt"
                       "basis-lib.rkt")
         (rename-out [remora-module-begin #%module-begin]
                     [remora-top-interaction #%top-interaction])
         #%racket-module-begin
         ;; Provide everything from racket/base that doesn't have the same name
         ;; as anything from Remora (must combine racket/base with the Remora
         ;; primops in order to subtract out Remora primops that don't shadow
         ;; anything in racket/base)
         (except-out (combine-out (all-from-out racket/base)
                                  (filtered-out
                                   (λ (name)
                                     (if (regexp-match #rx"^R_" name)
                                         (regexp-replace #rx"^R_" name "")
                                         name))
                                   (all-from-out "basis-lib.rkt")))
                     (filtered-out
                      (λ (name)
                        (if (regexp-match #rx"^R_" name)
                            (regexp-replace #rx"^R_" name "")
                            name))
                      (all-from-out "basis-lib.rkt"))
                     #%module-begin
                     #%top-interaction))

(current-read-interaction READER_remora-read-syntax)

(define-syntax (remora-module-begin stx)
  (syntax-parse stx
    [(_ body ...)
     #'(#%racket-module-begin (remora body) ...)]))

(define-syntax (remora-top-interaction stx)
  (syntax-parse stx
    [(_ body ...)
     #'(remora (body ...))]
    [(_ . body)
     #'(remora body)]))
