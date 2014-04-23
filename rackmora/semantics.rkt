#lang racket/base

(require racket/list
         racket/vector
         racket/sequence)

;;;-------------------------------------
;;; Internal use structures:
;;;-------------------------------------

;; Apply a Remora array (in Remora, an array may appear in function position)
(define (apply-rem-array fun . args)
  ; check whether the data portion of fun is Remora procedures
  (unless (for/and [(p (rem-array-data fun))] (rem-proc? p))
    (error "Array in function position must contain only Remora functions" fun))
  
  ; if last arg is a shape, split it off; args - last are the actual array args
  ;    otherwise args are the array args,
  ;              and result-shape should be something representing "not a shape"
  (define-values (array-args result-shape)
    (cond [(empty? args) (values '() 'no-annotation)]
          [(shape-idx? (last args)) (values (drop-right args 1) (last args))]
          [else (values args 'no-annotation)]))
  
  ; check whether array-args actually are Remora arrays
  (unless (for/and [(arr array-args)] (rem-array? arr))
    (error "Remora arrays can only by applied to Remora arrays" fun array-args))
  
  ; identify expected argument cell ranks
  (define individual-exp-ranks
    (for/list [(p (rem-array-data fun))]
      (for/vector [(t (rem-proc-type p))] (type->rank t))))
  (define expected-rank
    (cond [(empty? individual-exp-ranks) 
           'empty-function-array]
          [(for/and [(p individual-exp-ranks)]
             (equal? p (first individual-exp-ranks)))
           (first individual-exp-ranks)]
          [else (error "Could not identify expected rank for function" fun)]))
  (printf "expected-rank = ~v\n" expected-rank)
  
  ; find principal frame shape
  (define principal-frame
    (or (for/fold ([max-frame (rem-array-shape fun)])
          ([arr array-args]
           [r expected-rank])
          (prefix-max 
           (vector-drop-right (rem-array-shape arr) r)
           max-frame))
        (error "Incompatible argument frames"
               (cons (rem-array-shape fun)
                     (for/list ([arr array-args]
                                [r expected-rank])
                       (vector-drop-right (rem-array-shape arr) r)))
               #;(for/vector [(arr array-args)]
                   (rem-array-rank arr))
               #;expected-rank)))
  (printf "principal-frame = ~v\n" principal-frame)
  
  ; compute argument cell sizes
  (define cell-sizes
    (for/list ([arr array-args]
               [r expected-rank])
      (sequence-fold * 1 (vector-take-right (rem-array-shape arr) r))))
  (printf "cell-sizes = ~v\n" cell-sizes)
  
  ; compute argument frame sizes
  (define frame-sizes
    (for/list ([arr array-args]
               [r expected-rank])
      (sequence-fold * 1 (vector-drop-right (rem-array-shape arr) r))))
  (printf "frame-sizes = ~v\n" frame-sizes)
  
  ; compute each result cell
  (define result-cells
    (for/vector ([cell-id (sequence-fold * 1 principal-frame)])
      (apply
       (vector-ref
        (rem-array-data fun)
        (quotient cell-id
                  (quotient (sequence-fold * 1 principal-frame)
                            (sequence-fold * 1 (rem-array-shape fun)))))
       (for/list ([arr array-args]
                  [csize cell-sizes]
                  [fsize frame-sizes]
                  [r expected-rank])
         (rem-array
          (vector-take-right (rem-array-shape arr) r)
          (subvector (rem-array-data arr)
                     (quotient cell-id
                               (quotient (sequence-fold * 1 principal-frame)
                                         fsize))
                     csize))
         #;(take (antibase (vector->list principal-frame) cell-id)
                 (rem-array-rank arr))))))
  (printf "result-cells = ~v\n" result-cells)
  
  ; determine final result shape
  (define final-shape
    (cond
      ; empty frame and no shape annotation -> error
      [(and (equal? result-shape 'no-annotation)
            (equal? 0 (vector-length result-cells)))
           (error "Empty frame with no shape annotation: ~v applied to ~v"
                  fun array-args)]
      ; empty frame -> use annotated shape
      ; TODO: should maybe check for mismatch between annotated and actual
      ;       (i.e. frame-shape ++ cell-shape) result shapes
      [(equal? 0 (vector-length result-cells)) result-shape]
      [(for/and ([c result-cells])
         (equal? (rem-array-shape (vector-ref result-cells 0))
                 (rem-array-shape c)))
       (vector-append principal-frame
                      (rem-array-shape (vector-ref result-cells 0)))]
      [else (error "Result cells have mismatched shapes: ~v" result-cells)])
    )
  (printf "final-shape = ~v\n" final-shape)
  
  ; determine final result data: all result cells' data vectors concatenated
  (define final-data
    (apply vector-append
           (for/list ([r result-cells])
             (rem-array-data r))))
  (printf "final-data = ~v\n" final-data)
  
  (rem-array final-shape final-data))

;; A Remora array has
;; - shape, a vector of numbers
;; - data, a vector of any
(define-struct rem-array (shape data)
  #:transparent
  #:property prop:procedure apply-rem-array)

;; Find the rank of a Remora array
(define (rem-array-rank arr) (vector-length (rem-array-shape arr)))


;; Apply a Remora procedure (for internal convenience)
(define (apply-rem-proc fun . args)
  (apply (rem-proc-body fun) args))

;; A Remora procedure has
;; - body, a Racket procedure which consumes and produces Remora arrays
;; - type, a list of partly-erased Remora type describing the procedure's
;;     expected argument shapes
(define-struct rem-proc (body type)
  #:transparent
  ; may decide to drop this part
  #:property prop:procedure apply-rem-proc)

;; A partially-erased Remora type is one of
;; - a (rem-type-append some-shape-idx some-rem-type)
;; - 'scalar
(define-struct rem-type-append (head tail)
  #:transparent)
(define (rem-type? x) (or (rem-type-append? x)
                          (equal? 'scalar x)))

;; Convert a Remora procedure's expected cell type to an expected cell rank
(define (type->rank type)
  (cond [(equal? 'scalar type) 0]
        [(rem-type-append? type)
         (+ (vector-length (shape-idx-dims (rem-type-append-head type)))
            (type->rank (rem-type-append-tail type)))]))

;; A Remora index is one of
;; - a Nat index, which wraps a natural number
(define-struct nat-idx (num)
  #:transparent)
;; - a Shape index, which wraps a vector of Nat indices
(define-struct shape-idx (dims)
  #:transparent)

;; Add Remora indices
;; Consumes nat-idx arguments, produces a nat-idx
(define (idx+ . xs)
  (nat-idx (apply + (map nat-idx-num xs))))

;; A Remora box (dependent sum) has
;; - contents, a Remora value
;; - indices, a list of the witness indices
(define-struct rem-box (contents indices)
  #:transparent)

;; A Remora index abstraction is a Racket procedure which consumes Remora
;; indices and produces a Remora value

;; Remora index application is translated to Racket term application


;; Identify which of two sequences is the prefix of the other, or return #f
;; if neither is a prefix of the other (or if either sequence is #f)
(define (prefix-max seq1 seq2)
  (and seq1 seq2
       (for/and ([a seq1] [b seq2])
         (equal? a b))
       (if (> (sequence-length seq1) (sequence-length seq2)) seq1 seq2)))


;; Extract a contiguous piece of a vector
(define (subvector vec offset size)
  (vector-take (vector-drop vec offset) size))


;; Express a number in a given radix sequence
(define (antibase radix num)
  (define (antibase-internal radix num)
    (cond [(empty? radix) (list num)]
          [else (cons (quotient num (foldr * 1 radix))
                      (antibase-internal (rest radix)
                                         (remainder num (foldr * 1 radix))))]))
  (rest (antibase-internal radix num)))



;;;-------------------------------------
;;; Integration utilities
;;;-------------------------------------
;; Build a scalar Remora procedure from a Racket procedure
(define (rem-scalar-proc p arity)
  (rem-proc (λ args
              (rem-array
               #()
               (vector
                (apply + (for/list [(a args)]
                           (vector-ref (rem-array-data a) 0))))))
            (for/list [(i arity)] 'scalar)))

;; Build a scalar Remora array from a Racket value
(define (scalar v) (rem-array #() (vector v)))

;;;-------------------------------------
;;; Translation
;;;-------------------------------------
;; Transform an Erased Remora AST into Racket code

