#lang racket/base

(require "semantics.rkt"
         "syntax.rkt"
         racket/math
         racket/vector)

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


(define (shape-idx->list idx)
  (cond [(equal? idx 'scalar) '()]
        [else (cons (rem-type-append-head idx)
                    (shape-idx->list (rem-type-append-tail idx)))]))
(define (shape-idx->vector idx)
  (vector->immutable-vector (list->vector (shape-idx->list idx))))
(define (shape-idx->product idx)
  (for/product ([d (shape-idx->list idx)]) d))

(define R_head
  (Iλ ([cell-shape 'Shape]
       [length 'Nat])
      (Rλ ([arr (rem-type-append (shape length) cell-shape)])
          (rem-array (shape-idx->vector cell-shape)
                     (vector->immutable-vector
                      (vector-take (rem-array-data arr)
                                   (shape-idx->product cell-shape)))))))
(define R_tail
  (Iλ ([cell-shape 'Shape]
       [length 'Nat])
      (Rλ ([arr (rem-type-append (shape length) cell-shape)])
          (rem-array (shape-idx->vector cell-shape)
                     (vector->immutable-vector
                      (vector-take-right (rem-array-data arr)
                                         (shape-idx->product cell-shape)))))))
(define R_behead
  (Iλ ([cell-shape 'Shape]
       [length 'Nat])
      (Rλ ([arr (rem-type-append (shape length) cell-shape)])
          (rem-array (vector->immutable-vector
                      (list->vector
                       (cons (sub1 length)
                             (shape-idx->list cell-shape))))
                     (vector->immutable-vector
                      (vector-drop (rem-array-data arr)
                                   (shape-idx->product cell-shape)))))))
(define R_curtail
  (Iλ ([cell-shape 'Shape]
       [length 'Nat])
      (Rλ ([arr (rem-type-append (shape length) cell-shape)])
          (rem-array (vector->immutable-vector
                      (list->vector
                       (cons (sub1 length)
                             (shape-idx->list cell-shape))))
                     (vector->immutable-vector
                      (vector-drop-right (rem-array-data arr)
                                         (shape-idx->product cell-shape)))))))