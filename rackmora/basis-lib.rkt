#lang racket/base

(require "semantics.rkt"
         "syntax.rkt"
         racket/math
         racket/vector
         racket/list
         racket/contract)
(module+ test
  (require rackunit))

#;
(define (shape-idx->vector s)
  (cond [(shape-idx? s) (vector->immutable-vector (shape-idx-dims s))]
        [(equal? 'scalar s) #()]
        [else s]))

#;
(define (rem-type->vector t)
  (cond [(equal? 'scalar t) #()]
        [else (vector-append (shape-idx->vector (rem-type-append-head t))
                             (rem-type->vector (rem-type-append-tail t)))]))
(define/contract (rem-type->vector t)
  (-> rem-type? vector?)
  (list->vector (rem-type->list t)))
(define (shape->type s)
  (rem-type-append s 'scalar))

(define (rem-type->list idx)
  (cond [(equal? idx 'scalar) '()]
        [else (append (shape-idx->list (rem-type-append-head idx))
                      (rem-type->list (rem-type-append-tail idx)))]))
(define (shape-idx->list idx)
  (vector->list (shape-idx-dims idx)))
(define (shape-idx->vector idx)
  (vector->immutable-vector (list->vector (shape-idx->list idx))))
(define (shape-idx->product idx)
  (for/product ([d (shape-idx->list idx)]) d))

(define (shape-append . sh)
  (shape-idx (apply vector-append (map shape-idx-dims sh))))

(define R_id (rem-scalar-proc (λ (x) x) 1))

(define R_+ (rem-scalar-proc + 2))
(define R_- (rem-scalar-proc - 2))
(define R_* (rem-scalar-proc * 2))
(define R_/ (rem-scalar-proc / 2))
(define R_^ (rem-scalar-proc expt 2))

(define R_and (rem-scalar-proc (λ (x y) (and x y)) 2))
(define R_or (rem-scalar-proc (λ (x y) (or x y)) 2))

(define R_ceiling (rem-scalar-proc ceiling 2))
(define R_floor (rem-scalar-proc floor 2))

(define R_add1 (rem-scalar-proc add1 1))
(define R_sub1 (rem-scalar-proc sub1 1))

(define R_neg (rem-scalar-proc (λ (x) (- x)) 1))
(define R_inv (rem-scalar-proc (λ (x) (/ 1 x)) 1))

(define R_exp (rem-scalar-proc exp 1))

(define R_sqr (rem-scalar-proc sqr 1))
(define R_sqrt (rem-scalar-proc sqrt 1))

(define R_gcd (rem-scalar-proc gcd 1))
(define R_lcm (rem-scalar-proc lcm 1))

(define R_conjugate (rem-scalar-proc conjugate 1))

(define R_signum (rem-scalar-proc (λ (x) (/ x (magnitude x))) 1))

(define (logb b x) (/ (log x) (log b)))
(define R_logb (rem-scalar-proc logb 2))
(define R_ln (rem-scalar-proc log 1))
(define R_log (rem-scalar-proc (λ (x) (logb 10 x)) 1))
(define R_lg (rem-scalar-proc (λ (x) (logb 2 x)) 1))

(define R_imag-part (rem-scalar-proc imag-part 1))
(define R_real-part (rem-scalar-proc real-part 1))
(define R_magnitude (rem-scalar-proc magnitude 1))
(define R_angle (rem-scalar-proc angle 1))

(define R_= (rem-scalar-proc equal? 2))
(define R_< (rem-scalar-proc < 2))
(define R_<= (rem-scalar-proc <= 2))
(define R_> (rem-scalar-proc > 2))
(define R_>= (rem-scalar-proc >= 2))




; head, tail, behead, curtail really consume an arg with major axis length + 1,
; but the Nat index argument is effectively irrelevant
; "cell-shape" here refers to the -1-cells which will be pushed around
; "length" is how many -1-cells there are
(define R_head
  (Iλ ([cell-shape 'Shape]
       [length 'Nat])
      (Rλ ([arr (rem-type-append (shape (nat-idx-num length))
                                 (shape->type cell-shape))])
          (rem-array (shape-idx->vector cell-shape)
                     (vector->immutable-vector
                      (vector-take (rem-array-data arr)
                                   (shape-idx->product cell-shape)))))))
(module+ test
  (check-equal?
   ((rem-array #() (vector-immutable (R_head (shape) (nat-idx 3))))
     (rem-array #(4 3) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(4) #(0 3 6 9)))
  (check-equal?
   ((rem-array #() (vector-immutable (R_head (shape 3) (nat-idx 4))))
     (rem-array #(4 3) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(3) #(0 1 2))))

(define R_tail
  (Iλ ([cell-shape 'Shape]
       [length 'Nat])
      (Rλ ([arr (rem-type-append (shape (nat-idx-num length))
                                 (shape->type cell-shape))])
          (rem-array (shape-idx->vector cell-shape)
                     (vector->immutable-vector
                      (vector-take-right (rem-array-data arr)
                                         (shape-idx->product cell-shape)))))))
(module+ test
  (check-equal?
   ((rem-array #() (vector-immutable (R_tail (shape) (nat-idx 3))))
     (rem-array #(4 3) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(4) #(2 5 8 11)))
  (check-equal?
   ((rem-array #() (vector-immutable (R_tail (shape 3) (nat-idx 4))))
     (rem-array #(4 3) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(3) #(9 10 11))))

(define R_behead
  (Iλ ([cell-shape 'Shape]
       [length 'Nat])
      (Rλ ([arr (rem-type-append (shape (nat-idx-num length))
                                 (shape->type cell-shape))])
          #;(printf "shape: ~v\ndata: ~v\n"
                  (list->vector
                   (cons (sub1 length)
                         (shape-idx->list cell-shape)))
                  (vector-drop (rem-array-data arr)
                               (shape-idx->product cell-shape)))
          (rem-array (vector->immutable-vector
                      (list->vector
                       (cons (sub1 (vector-ref (rem-array-shape arr) 0))
                             (shape-idx->list cell-shape))))
                     (vector->immutable-vector
                      (vector-drop (rem-array-data arr)
                                   (shape-idx->product cell-shape)))))))
(module+ test
  (check-equal?
   ((rem-array #() (vector-immutable (R_behead (shape) (nat-idx 3))))
     (rem-array #(4 3) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(4 2) #(1 2 4 5 7 8 10 11)))
  (check-equal?
   ((rem-array #() (vector-immutable (R_behead (shape 3) (nat-idx 4))))
     (rem-array #(4 3) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(3 3) #(3 4 5 6 7 8 9 10 11))))

(define R_curtail
  (Iλ ([cell-shape 'Shape]
       [length 'Nat])
      (Rλ ([arr (rem-type-append (shape (nat-idx-num length))
                                 (shape->type cell-shape))])
          (rem-array (vector->immutable-vector
                      (list->vector
                       (cons (sub1 (vector-ref (rem-array-shape arr) 0))
                             (shape-idx->list cell-shape))))
                     (vector->immutable-vector
                      (vector-drop-right (rem-array-data arr)
                                         (shape-idx->product cell-shape)))))))
(module+ test
  (check-equal?
   ((rem-array #() (vector-immutable (R_curtail (shape) (nat-idx 3))))
     (rem-array #(4 3) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(4 2) #(0 1 3 4 6 7 9 10)))
  (check-equal?
   ((rem-array #() (vector-immutable (R_curtail (shape 3) (nat-idx 4))))
     (rem-array #(4 3) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(3 3) #(0 1 2 3 4 5 6 7 8))))

; Split an array into a list of cells of a given rank
(define (array->cell-list arr cell-rank)
  (define nat-cell-rank
    (if (>= cell-rank 0)
        cell-rank
        (+ (rem-array-rank arr) cell-rank)))
  (define frame-shape (vector-drop-right (rem-array-shape arr) nat-cell-rank))
  (define cell-count (for/product ([d frame-shape]) d))
  (define cell-shape (vector-take-right (rem-array-shape arr) nat-cell-rank))
  (define cell-size
    (for/product ([d cell-shape]) d))
  (for/list ([i cell-count])
    (rem-array cell-shape (subvector (rem-array-data arr)
                                     (* i cell-size)
                                     cell-size))))
(module+ test
  (check-equal?
   (array->cell-list (rem-array #(3 2 4) (for/vector ([i 24]) i)) 0)
   (for/list ([i 24]) (rem-array #() (vector i))))
  (check-equal?
   (array->cell-list (rem-array #(3 2 4) (for/vector ([i 24]) i)) 1)
   (for/list ([i 6]) (rem-array #(4) (for/vector ([j 4]) (+ j (* 4 i))))))
  (check-equal?
   (array->cell-list (rem-array #(3 2 4) (for/vector ([i 24]) i)) 2)
   (for/list ([i 3]) (rem-array #(2 4) (for/vector ([j 8]) (+ j (* 8 i)))))))

; Merge a list of cells into an array with the given frame shape
; If there are no cells in the list (i.e. empty frame), specify a cell shape
(define/contract (cell-list->array arrs frame-shape [opt-cell-shape #f])
  (->* (list? shape-idx?)
       (shape-idx?)
       rem-array?)
  (define cell-shape (or opt-cell-shape (rem-array-shape (first arrs))))
  (rem-array (vector-append (shape-idx->vector frame-shape)
                            (shape-idx->vector cell-shape))
             (apply vector-append (map rem-array-data arrs))))



(define R_reverse
  (Iλ ([cell-shape 'Shape]
       [length 'Nat])
      (Rλ ([arr (rem-type-append (shape (nat-idx-num length))
                                 (shape->type cell-shape))])
          (cell-list->array (reverse (array->cell-list arr -1))
                            (shape (nat-idx-num length))
                            cell-shape))))
(module+ test
  (check-equal?
   ((rem-array #() (vector-immutable (R_reverse (shape) (nat-idx 4))))
    (rem-array #(3 4) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(3 4) #(3 2 1 0 7 6 5 4 11 10 9 8))))
#;(check-equal?
   ((rem-array #() (vector-immutable (R_reverse (rem-type-append 4 'scalar) 3)))
    (rem-array #(3 4) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   ??)

(define R_append
  (Iλ ([cell-shape 'Shape]
       [len1 'Nat]
       [len2 'Nat])
      (Rλ ([arr1 (rem-type-append (shape len1) cell-shape)]
           [arr2 (rem-type-append (shape len1) cell-shape)])
          (cell-list->array (append (array->cell-list arr1 -1)
                                    (array->cell-list arr2 -1))
                            (vector (+ len1 len2))
                            (shape-idx->vector cell-shape)))))
