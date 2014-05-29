#lang racket/base

(require "semantics.rkt"
         "syntax.rkt"
         racket/math
         racket/vector
         racket/list
         racket/contract)
(module+ test
  (require rackunit))


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
  (Rλ ([arr 'all])
      ; operates on the -1-cells
      (define cell-shape (vector-drop (rem-array-shape arr) 1))
      (rem-array (vector-drop (rem-array-shape arr) 1)
                 (vector-take (rem-array-data arr)
                              (for/product ([d cell-shape]) d)))))
(module+ test
  ; TODO: make a "rerank" macro and rewrite test cases using it
  (check-equal?
   ((rem-array #() (vector (Rλ ([arr 1]) (R_head arr))))
     (rem-array #(4 3) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(4) #(0 3 6 9)))
  (check-equal?
   ((rem-array #() (vector R_head #;(R_head (shape 3) (nat-idx 4))))
     (rem-array #(4 3) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(3) #(0 1 2))))

(define R_tail
  (Rλ ([arr 'all])
      ; operates on the -1-cells
      (define cell-shape (vector-drop (rem-array-shape arr) 1))
      (rem-array (vector-drop (rem-array-shape arr) 1)
                 (vector-take-right (rem-array-data arr)
                                    (for/product ([d cell-shape]) d)))))
(module+ test
  (check-equal?
   ((rem-array #() (vector (Rλ ([arr 1]) (R_tail arr))))
    (rem-array #(4 3) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(4) #(2 5 8 11)))
  (check-equal?
   ((rem-array #() (vector R_tail))
    (rem-array #(4 3) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(3) #(9 10 11))))

(define R_behead
  (Rλ ([arr 'all])
      (define cell-shape (vector-drop (rem-array-shape arr) 1))
      #;(printf "shape: ~v\ndata: ~v\n"
                (list->vector
                 (cons (sub1 length)
                       (shape-idx->list cell-shape)))
                (vector-drop (rem-array-data arr)
                             (shape-idx->product cell-shape)))
      (rem-array (vector-append
                  (vector (sub1 (vector-ref (rem-array-shape arr) 0)))
                  cell-shape)
                 (vector-drop (rem-array-data arr)
                              (for/product ([d cell-shape]) d)))))
(module+ test
  (check-equal?
   ((rem-array #() (vector (Rλ [(arr 1)] (R_behead arr))))
    (rem-array #(4 3) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(4 2) #(1 2 4 5 7 8 10 11)))
  (check-equal?
   ((rem-array #() (vector R_behead))
    (rem-array #(4 3) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(3 3) #(3 4 5 6 7 8 9 10 11))))

(define R_curtail
  (Rλ ([arr 'all])
      (define cell-shape (vector-drop (rem-array-shape arr) 1))
      (rem-array (vector-append
                  (vector (sub1 (vector-ref (rem-array-shape arr) 0)))
                  cell-shape)
                 (vector-drop-right (rem-array-data arr)
                                     (for/product ([d cell-shape]) d)))))
(module+ test
  (check-equal?
   ((rem-array #() (vector (Rλ [(arr 1)] (R_curtail arr))))
    (rem-array #(4 3) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(4 2) #(0 1 3 4 6 7 9 10)))
  (check-equal?
   ((rem-array #() (vector R_curtail))
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
  (->* (list? vector?)
       (vector?)
       rem-array?)
  (define cell-shape (or opt-cell-shape (rem-array-shape (first arrs))))
  (rem-array (vector-append frame-shape cell-shape)
             (apply vector-append (map rem-array-data arrs))))



(define R_reverse
  (Rλ ([arr 'all])
      (define length (vector-ref (rem-array-shape arr) 0))
      (cell-list->array (reverse (array->cell-list arr -1))
                        (vector length)
                        (vector-drop (rem-array-shape arr) 1))))
(module+ test
  (check-equal?
   ((rem-array #() (vector (Rλ ([arr 1]) (R_reverse arr))))
    (rem-array #(3 4) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(3 4) #(3 2 1 0 7 6 5 4 11 10 9 8)))
  (check-equal?
   ((rem-array #() (vector R_reverse))
    (rem-array #(3 4) #(0 1 2 3 4 5 6 7 8 9 10 11)))
   (rem-array #(3 4) #(8 9 10 11 4 5 6 7 0 1 2 3))))

(define R_append
  (Rλ ([arr1 'all]
       [arr2 'all])
      (define cell-shape
        (if (equal? (vector-drop (rem-array-shape arr1) 1)
                    (vector-drop (rem-array-shape arr2) 1))
            (vector-drop (rem-array-shape arr1) 1)
            (error 'R_append "shape mismatch: ~v\t~v" arr1 arr2)))
      (cell-list->array (append (array->cell-list arr1 -1)
                                (array->cell-list arr2 -1))
                        (vector (+ (vector-ref (rem-array-shape arr1) 0)
                                   (vector-ref (rem-array-shape arr2) 0)))
                        cell-shape)))

(module+ test
  (check-equal?
   ((rem-array #() (vector R_append))
    (rem-array #(4 3) #(0 1 2 3 4 5 6 7 8 9 10 11))
    (rem-array #(2 3) #(20 30 40 50 60 70)))
   (rem-array #(6 3) #(0 1 2 3 4 5 6 7 8 9 10 11 20 30 40 50 60 70)))
  (check-equal?
   ((rem-array #() (vector (Rλ ([arr1 1] [arr2 1]) (R_append arr1 arr2))))
    (rem-array #(3 4) #(0 1 2 3 4 5 6 7 8 9 10 11))
    (rem-array #(3 2) #(20 30 40 50 60 70)))
   (rem-array #(3 6) #(0 1 2 3 20 30 4 5 6 7 40 50 8 9 10 11 60 70))))
