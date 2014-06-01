#lang racket/base

(require "semantics.rkt"
         syntax/parse
         (for-syntax "semantics.rkt"
                     syntax/parse
                     racket/base
                     racket/list))

(provide Rλ)




(begin-for-syntax
  (define-syntax-class CONST
    #:description "Remora constant"
    #:literals (quote)
    (pattern bool:boolean)
    (pattern numlit:number)
    (pattern strlit:str)
    (pattern (quote sexp)))
  (define-syntax-class RANK
    #:description "Remora argument rank"
    (pattern 'all)
    (pattern cell-rank:nat))
  (define-syntax-class ATOM
    #:description "Remora atom"
    #:datum-literals (fn)
    (pattern const:CONST)
    (pattern (fn ((var:id r:RANK) ...) body:EXP)))
  (define-syntax-class ALITERAL
    #:description "Remora array literal"
    #:datum-literals (array alit)
    (pattern (array piece:ALITERAL ...))
    (pattern (alit (dim:nat ...) elt:ATOM ...)))
  (define-syntax-class EXP
    #:description "Remora expression"
    #:datum-literals (apply apply/shape box unbox vec :)
    (pattern arr:ALITERAL)
    (pattern var:id)
    (pattern (apply fun:EXP arg:EXP ...))
    (pattern (apply/shape shp:EXP fun:EXP arg:EXP ...))
    (pattern (box contents:EXP))
    (pattern (unbox var:id arr:EXP body:EXP))
    (pattern (vec arr:EXP ...))
    ; sugar for  (apply fun:EXP arg:EXP ...)
    (pattern (fun:EXP arg:EXP ...))
    ; sugar for  (apply/shape shp:EXP fun:EXP arg:EXP ...)
    (pattern (: shp:EXP fun:EXP arg:EXP ...))))


(define-syntax (Rλ stx)
  (syntax-parse stx
    [(_ ((var:id rank:expr) ...) body ...+)
    #'(rem-proc (λ (var ...) body ...)
                 (list rank ...))]))

(define-syntax (remora? stx)
  (syntax-parse stx
    [(_ const:CONST) #'"remora constant"]
    [(_ rank:RANK) #'"remora rank"]
    [(_ atom:ATOM) #'"remora atom"]
    [(_ aliteral:ALITERAL) #'"remora array literal"]
    [(_ exp:EXP) #'"remora expression"]
    [(_ otherwise) #'"not remora"]))

(define-syntax (remora-const? stx)
  (syntax-parse stx
    [(_ const:CONST) #'#t]
    [(_ otherwise) #'#f]))
(define-syntax (remora-rank? stx)
  (syntax-parse stx
    [(_ rank:RANK) #'#t]
    [(_ otherwise) #'#f]))
(define-syntax (remora-atom? stx)
  (syntax-parse stx
    [(_ atom:ATOM) #'#t]
    [(_ otherwise) #'#f]))
(define-syntax (remora-array-literal? stx)
  (syntax-parse stx
    [(_ aliteral:ALITERAL) #'#t]
    [(_ otherwise) #'#f]))
(define-syntax (remora-exp? stx)
  (syntax-parse stx
    [(_ exp:EXP) #'#t]
    [(_ otherwise) #'#f]))

; (alit (nat ...) atom ...)
;  (rem-array (vector nat ...) (vector atom ...))
; TODO: automated test
(define-syntax (alit stx)
  (syntax-parse stx
    #;[(_ (dim:nat ...) elt:CONST ...) #`(#,rem-array (#,vector dim ...)
                                                    (#,vector elt ...))]
    [(_ (dim:nat ...) elt:ATOM ...) #`(#,rem-array (#,vector dim ...)
                                                   (#,vector elt ...))]))

; (apply expr1 expr2 ...)
;  (apply-rem-array expr expr ...)
; TODO: automated test
(define-syntax (apply stx)
  (syntax-parse stx
    [(_ fun:EXP arg:EXP ...) #`(fun arg ...)]))

; (apply/shape expr0 expr1 expr2 ...)
;  (apply-rem-array (rem-array->vector expr0) expr expr ...)
; TODO: automated test
(define-syntax (apply/shape stx)
  (syntax-parse stx
    [(_ shp:EXP fun:EXP arg:EXP ...)
     #`(apply-rem-array (#,rem-array->vector shp)
                        fun arg ...)]))
; or should the shape only get evaluated if it turns out to be needed?
; if it's a by-need thing, will need to make it a thunk and have apply-rem-array
; force it whenn needed

; (box expr)
;  (rem-box expr)
; TODO: automated test
(define-syntax (box stx)
  (syntax-parse stx
    [(_ contents:EXP) #`(#,rem-box contents)]))

; (unbox var some-box expr)
;  (let ([var (rem-box-contents some-box)]) expr)
; TODO: automated test
(define-syntax (unbox stx)
  (syntax-parse stx
    [(_ var:id some-box:EXP body:EXP)
     #`(let ([var (#,rem-box-contents some-box)]) body)]))

; (vec expr ...)
;  (build-vec expr ...)
; TODO: automated test
(define-syntax (vec stx)
  (syntax-parse stx
    [(_ piece:EXP ...) #`(#,build-vec piece ...)]))

; (array . array-literals)
; "smart constructor"
; if all exps are (alit ...) with same shape, gather them into one alit form
; disallow (alit ...)s with mismatching forms
; otherwise, (apply build-array exps)
; TODO: automated test
(define-syntax (array stx)
  (syntax-parse stx
    #:datum-literals (alit)
    [(_ (alit (dim:nat ...) elt:ATOM ...) ...+)
     #:when (all-equal? (syntax->datum (syntax ((dim ...) ...))))
     #:with (old-dims ...) (first (syntax-e #`((dim ...) ...)))
     #:with outer-dim (datum->syntax
                       stx
                       (length (syntax->datum #`((elt ...) ...))))
     #:with (joined-elts ...) (datum->syntax
                               stx
                               (apply append (syntax->datum #`((elt ...) ...))))
     #'(alit (outer-dim old-dims ...) joined-elts ...)]
    [(_ (alit (dim:nat ...) elt:ATOM ...) ...+)
     #:when (not (all-equal? (syntax->datum (syntax ((dim ...) ...)))))
     (error "array literal components with mismatched shapes at\n"
            (syntax-source stx)
            (syntax-line stx)
            (syntax-column stx))]
    [(_ piece:ALITERAL ...)
     #`(vec piece ...)]))
(begin-for-syntax
  (define (all-equal? xs)
    (cond [(<= (length xs) 1) #t]
          [else (and (equal? (first xs) (second xs))
                     (all-equal? (rest xs)))])))

