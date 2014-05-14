#lang racket/base

(require racket/list
         racket/vector
         racket/sequence
         racket/contract/base)
(module+ test
  (require rackunit))
(define debug-mode (make-parameter #f))
(provide debug-mode)

;;;-------------------------------------
;;; Internal use structures:
;;;-------------------------------------

;; TODO: what about passing the result shape annotation as a keyword argument?
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
  (when (debug-mode) (printf "Result shape is ~v\n" result-shape))
  
  ; check whether array-args actually are Remora arrays
  (unless (for/and [(arr array-args)] (rem-array? arr))
    (error "Remora arrays can only by applied to Remora arrays" fun array-args))
  (when (debug-mode) (printf "checked for Remora array arguments\n"))
  
  ; identify expected argument cell ranks
  (define individual-exp-ranks
    (for/list [(p (rem-array-data fun))]
      (when (debug-mode) (printf "checking expected ranks for ~v\n" p))
      (for/vector [(t (rem-proc-type p))]
        (when (debug-mode) (printf " - ~v\n" t))
        (type->rank t))))
  (when (debug-mode) (printf "individual expected ranks are ~v\n"
                             individual-exp-ranks))
  (define expected-rank
    (cond [(empty? individual-exp-ranks) 
           'empty-function-array]
          [(for/and [(p individual-exp-ranks)]
             (equal? p (first individual-exp-ranks)))
           (first individual-exp-ranks)]
          [else (error "Could not identify expected rank for function" fun)]))
  (when (debug-mode) (printf "expected-rank = ~v\n" expected-rank))
  
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
                       (vector-drop-right (rem-array-shape arr) r))))))
  (when (debug-mode) (printf "principal-frame = ~v\n" principal-frame))
  
  ; compute argument cell sizes
  (define cell-sizes
    (for/list ([arr array-args]
               [r expected-rank])
      (sequence-fold * 1 (vector-take-right (rem-array-shape arr) r))))
  (when (debug-mode) (printf "cell-sizes = ~v\n" cell-sizes))
  
  ; compute argument frame sizes
  (define frame-sizes
    (for/list ([arr array-args]
               [r expected-rank])
      (sequence-fold * 1 (vector-drop-right (rem-array-shape arr) r))))
  (when (debug-mode) (printf "frame-sizes = ~v\n" frame-sizes))
  
  ; compute each result cell
  (define result-cells
    (for/vector ([cell-id (sequence-fold * 1 principal-frame)])
      (when (debug-mode)
        (printf
         "using function cell ~v\n"
         (quotient cell-id
                   (quotient (sequence-fold * 1 principal-frame)
                             (sequence-fold * 1 (rem-array-shape fun))))))
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
         (when (debug-mode)
           (printf "  arg cell #~v, csize ~v, pfr ~v, fr ~v -- ~v\n"
                   cell-id csize (sequence-fold * 1 principal-frame) fsize
                   (rem-array
                    (vector-take-right (rem-array-shape arr) r)
                    (subvector (rem-array-data arr)
                               (quotient (* cell-id csize)
                                         (quotient (sequence-fold * 1 principal-frame)
                                                   fsize))
                               csize))))
         (rem-array
          (vector-take-right (rem-array-shape arr) r)
          (subvector (rem-array-data arr)
                     (quotient (* cell-id csize)
                               (quotient (sequence-fold * 1 principal-frame)
                                         fsize))
                     csize))))))
  (when (debug-mode) (printf "result-cells = ~v\n" result-cells))
  
  (when (debug-mode)
    (printf "# of result cells: ~v\nresult-shape = ~v\n"
            (vector-length result-cells) result-shape))
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
      [(equal? 0 (vector-length result-cells)) (shape-idx-dims result-shape)]
      [(for/and ([c result-cells])
         (equal? (rem-array-shape (vector-ref result-cells 0))
                 (rem-array-shape c)))
       (vector-append principal-frame
                      (rem-array-shape (vector-ref result-cells 0)))]
      [else (error "Result cells have mismatched shapes: ~v" result-cells)]))
  (when (debug-mode) (printf "final-shape = ~v\n" final-shape))
  
  ; determine final result data: all result cells' data vectors concatenated
  (define final-data
    (apply vector-append
           (for/list ([r result-cells])
             (rem-array-data r))))
  (when (debug-mode) (printf "final-data = ~v\n" final-data))
  
  (rem-array final-shape final-data))

;; Contract constructor for immutable vectors of specified length
(define ((vector-length/c elts len) vec)
  (and ((vectorof elts #:immutable #t) vec)
       (equal? (vector-length vec) len)))

;; A Remora array has
;; - shape, a vector of numbers
;; - data, a vector of any
(provide
 (contract-out
  #;(struct rem-array
      ([shape (vectorof exact-nonnegative-integer? #:immutable #t)]
       [data (vectorof any #:immutable #t)])
      #:omit-constructor)
  (rem-array (->i ([shape (vectorof exact-nonnegative-integer? #:immutable #t)]
                   [data (shape) (vector-length/c
                                  any/c
                                  (for/product ([dim shape]) dim))])
                  [result any/c]))
  (rem-array-shape (-> rem-array?
                       (vectorof exact-nonnegative-integer? #:immutable #t)))
  (rem-array-data  (-> rem-array?
                       (vectorof any/c)))
  (rem-array? (-> any/c boolean?))))
(define-struct rem-array (shape data)
  #:transparent
  #:property prop:procedure apply-rem-array)
(module+ test
  (define array-ex:scalar1 (rem-array #() #(4)))
  (define array-ex:scalar2 (rem-array #() #(2)))
  (define array-ex:vector1 (rem-array #(2) #(10 20)))
  (define array-ex:matrix1 (rem-array #(2 3) #(1 2 3 4 5 6))))

;; Find the rank of a Remora array
(provide
 (contract-out
  (rem-array-rank (-> rem-array? exact-nonnegative-integer?))))
(define (rem-array-rank arr) (vector-length (rem-array-shape arr)))
(module+ test
  (check-equal? 0 (rem-array-rank array-ex:scalar1))
  (check-equal? 1 (rem-array-rank array-ex:vector1))
  (check-equal? 2 (rem-array-rank array-ex:matrix1)))


;; Apply a Remora procedure (for internal convenience)
;; TODO: consider eliminating this (see note in rem-proc struct defn)
(define (apply-rem-proc fun . args)
  (apply (rem-proc-body fun) args))

;; A Remora procedure has
;; - body, a Racket procedure which consumes and produces Remora arrays
;; - type, a list of partly-erased Remora type describing the procedure's
;;     expected argument shapes
(provide
 (contract-out (struct rem-proc ([body procedure?] [type (listof rem-type?)]))))
(define-struct rem-proc (body type)
  #:transparent
  ; may decide to drop this part -- it seems to hide a common error:
  ;   using (R+ arr1 arr2) instead of ([scalar R+] arr1 arr2) means no lifting
  #:property prop:procedure apply-rem-proc)
(module+ test
  (define R+ (rem-scalar-proc + 2))
  (define R- (rem-scalar-proc - 2))
  (define R* (rem-scalar-proc * 2))
  (check-equal? (R+ array-ex:scalar1 array-ex:scalar2)
                (rem-array #() #(6))))

;; A partially-erased Remora type is one of
;; - a (rem-type-append some-shape-idx some-rem-type)
;; - 'scalar
(provide
 (contract-out (struct rem-type-append ([head shape-idx] [tail rem-type?]))))
(define-struct rem-type-append (head tail)
  #:transparent)
(provide
 (contract-out (rem-type? (-> any/c boolean?))))
(define (rem-type? x) (or (rem-type-append? x)
                          (equal? 'scalar x)))
(module+ test
  (define type-2:s (rem-type-append (shape-idx #(2)) 'scalar))
  (define type-2x3:s (rem-type-append (shape-idx #(2 3)) 'scalar))
  (define type-2:3:s (rem-type-append (shape-idx #(2))
                                      (rem-type-append (shape-idx #(3))
                                                       'scalar)))
  (check-true (rem-type? type-2x3:s))
  (check-true (rem-type? 'scalar))
  (check-false (rem-type? (shape-idx #(2)))))

;; Convert a Remora procedure's expected cell type to an expected cell rank
(provide
 (contract-out (type->rank (-> rem-type? exact-nonnegative-integer?))))
(define (type->rank type)
  (cond [(equal? 'scalar type) 0]
        [(rem-type-append? type)
         (+ (vector-length (shape-idx-dims (rem-type-append-head type)))
            (type->rank (rem-type-append-tail type)))]))
(module+ test
  (check-equal? (type->rank type-2:s) 1)
  (check-equal? (type->rank type-2x3:s) 2)
  (check-equal? (type->rank type-2:3:s) 2))

;; TODO: may eventually want to just use unwrapped naturals/vectors
;; A Remora index is one of
;; - a Nat index, which wraps a natural number
(provide (contract-out (struct nat-idx ([num exact-nonnegative-integer?]))))
(define-struct nat-idx (num)
  #:transparent)
;; - a Shape index, which wraps a vector of Nat indices
(provide (contract-out
          (struct shape-idx ([dims (vectorof exact-nonnegative-integer?)]))))
(define-struct shape-idx (dims)
  #:transparent)
;; Easier way to build a shape
(provide (contract-out (shape (->* () #:rest (listof exact-nonnegative-integer?)
                                   shape-idx?))))
(define (shape . dims)
  (shape-idx (apply vector-immutable dims)))

;; Add Remora indices
;; Consumes nat-idx arguments, produces a nat-idx
(provide (contract-out (idx+ (->* () #:rest (listof nat-idx?)
                                  nat-idx?))))
(define (idx+ . xs)
  (nat-idx (apply + (map nat-idx-num xs))))
(module+ test
  (check-equal? (idx+ (nat-idx 3) (nat-idx 5))
                (nat-idx 8))
  (check-equal? (idx+ (nat-idx 1) (nat-idx 2) (nat-idx 3))
                (nat-idx 6)))

;; A Remora box (dependent sum) has
;; - contents, a Remora value
;; - indices, a list of the witness indices
(provide (contract-out
          (struct rem-box ([contents rem-array?]
                           [indices (listof (or/c nat-idx? shape-idx?))]))))
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
(module+ test
  (check-equal? (prefix-max #(3 4) #(3 4)) #(3 4))
  (check-equal? (prefix-max #(3 4) #(3 4 9)) #(3 4 9))
  (check-equal? (prefix-max #(3 4 2) #(3 4)) #(3 4 2))
  (check-equal? (prefix-max #(3) #(3 4)) #(3 4))
  (check-equal? (prefix-max #(3 2) #(3 4)) #f)
  (check-equal? (prefix-max #(3 2) #(3 4 5)) #f))


;; Extract a contiguous piece of a vector
(define (subvector vec offset size)
  (vector-take (vector-drop vec offset) size))
(module+ test
  (check-equal? (subvector #(2 4 6 3 5 7) 1 3) #(4 6 3))
  (check-equal? (subvector #(2 4 6 3 5 7) 4 2) #(5 7))
  (check-equal? (subvector #(2 4 6 3 5 7) 4 0) #()))


;; Express a number in a given radix sequence
(define (antibase radix num)
  (define (antibase-internal radix num)
    (cond [(empty? radix) (list num)]
          [else (cons (quotient num (foldr * 1 radix))
                      (antibase-internal (rest radix)
                                         (remainder num (foldr * 1 radix))))]))
  (rest (antibase-internal radix num)))



;; tests for array application
;; TODO: test array application for functions that consume/produce non-scalars
(module+ test
  (check-equal? ((scalar R+) (scalar 3) (scalar 4))
                (scalar 7))
  (check-equal? ((scalar R+) (rem-array #(2 3) #(1 2 3 4 5 6))
                             (rem-array #(2) #(10 20)))
                (rem-array #(2 3) #(11 12 13 24 25 26)))
  (check-equal? ((rem-array #(2) (vector R+ R-))
                 (rem-array #(2 3) #(1 2 3 4 5 6))
                 (rem-array #(2) #(10 20)))
                (rem-array #(2 3) #(11 12 13 -16 -15 -14))))

;;;-------------------------------------
;;; Integration utilities
;;;-------------------------------------
;; Build a scalar Remora procedure from a Racket procedure
(provide
 (contract-out
  (rem-scalar-proc (-> procedure? exact-nonnegative-integer? rem-proc?))))
(define (rem-scalar-proc p arity)
  (rem-proc (λ args
              (rem-array
               #()
               (vector-immutable
                (apply p (for/list [(a args)]
                           (vector-ref (rem-array-data a) 0))))))
            (for/list [(i arity)] 'scalar)))

;; Build a scalar Remora array from a Racket value
(define (scalar v) (rem-array #() (vector-immutable v)))

;;;-------------------------------------
;;; Translation
;;;-------------------------------------
;; Transform an Erased Remora AST into Racket code

