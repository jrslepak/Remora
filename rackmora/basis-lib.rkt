#lang racket/base

(require "semantics.rkt"
         "syntax.rkt"
         racket/math
         racket/vector
         racket/list
         racket/contract)
(module+ test
  (require rackunit))


(define R_id (rem-array #() (vector (rem-scalar-proc (λ (x) x) 1))))

(define R_+ (rem-array #() (vector (rem-scalar-proc + 2))))
(define R_- (rem-array #() (vector (rem-scalar-proc - 2))))
(define R_* (rem-array #() (vector (rem-scalar-proc * 2))))
(define R_/ (rem-array #() (vector (rem-scalar-proc / 2))))
(define R_^ (rem-array #() (vector (rem-scalar-proc expt 2))))

(define R_and (rem-array #() (vector (rem-scalar-proc (λ (x y) (and x y)) 2))))
(define R_or (rem-array #() (vector (rem-scalar-proc (λ (x y) (or x y)) 2))))

(define R_ceiling (rem-array #() (vector (rem-scalar-proc ceiling 2))))
(define R_floor (rem-array #() (vector (rem-scalar-proc floor 2))))

(define R_add1 (rem-array #() (vector (rem-scalar-proc add1 1))))
(define R_sub1 (rem-array #() (vector (rem-scalar-proc sub1 1))))

(define R_neg (rem-array #() (vector (rem-scalar-proc (λ (x) (- x)) 1))))
(define R_inv (rem-array #() (vector (rem-scalar-proc (λ (x) (/ 1 x)) 1))))

(define R_exp (rem-array #() (vector (rem-scalar-proc exp 1))))

(define R_sqr (rem-array #() (vector (rem-scalar-proc sqr 1))))
(define R_sqrt (rem-array #() (vector (rem-scalar-proc sqrt 1))))

(define R_gcd (rem-array #() (vector (rem-scalar-proc gcd 1))))
(define R_lcm (rem-array #() (vector (rem-scalar-proc lcm 1))))

(define R_conjugate (rem-array #() (vector (rem-scalar-proc conjugate 1))))

(define R_signum
  (rem-array #() (vector (rem-scalar-proc (λ (x) (/ x (magnitude x))) 1))))

(define (logb b x) (/ (log x) (log b)))
(define R_logb (rem-array #() (vector (rem-scalar-proc logb 2))))
(define R_ln (rem-array #() (vector (rem-scalar-proc log 1))))
(define R_log (rem-array #() (vector (rem-scalar-proc (λ (x) (logb 10 x)) 1))))
(define R_lg (rem-array #() (vector (rem-scalar-proc (λ (x) (logb 2 x)) 1))))

(define R_imag-part (rem-array #() (vector (rem-scalar-proc imag-part 1))))
(define R_real-part (rem-array #() (vector (rem-scalar-proc real-part 1))))
(define R_magnitude (rem-array #() (vector (rem-scalar-proc magnitude 1))))
(define R_angle (rem-array #() (vector (rem-scalar-proc angle 1))))

(define R_= (rem-array #() (vector (rem-scalar-proc equal? 2))))
(define R_< (rem-array #() (vector (rem-scalar-proc < 2))))
(define R_<= (rem-array #() (vector (rem-scalar-proc <= 2))))
(define R_> (rem-array #() (vector (rem-scalar-proc > 2))))
(define R_>= (rem-array #() (vector (rem-scalar-proc >= 2))))




; head, tail, behead, curtail really consume an arg with major axis length + 1,
; but the Nat index argument is effectively irrelevant
; "cell-shape" here refers to the -1-cells which will be pushed around
; "length" is how many -1-cells there are
(define R_head
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all])
               ; operates on the -1-cells
               (define cell-shape (vector-drop (rem-array-shape arr) 1))
               (rem-array (vector-drop (rem-array-shape arr) 1)
                          (vector-take (rem-array-data arr)
                                       (for/product ([d cell-shape]) d)))))))
(module+ test
  (check-equal?
   (remora
    ((rerank (1) R_head)
     (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (4) 0 3 6 9)))
  (check-equal?
   (remora
    (R_head
     (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (3) 0 1 2))))

(define R_tail
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all])
        ; operates on the -1-cells
        (define cell-shape (vector-drop (rem-array-shape arr) 1))
        (rem-array (vector-drop (rem-array-shape arr) 1)
                   (vector-take-right (rem-array-data arr)
                                      (for/product ([d cell-shape]) d)))))))
(module+ test
  (check-equal?
   (remora
    ((rerank (1) R_tail)
     (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (4) 2 5 8 11)))
  (check-equal?
   (remora
    (R_tail
     (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (3) 9 10 11))))

(define R_behead
  (rem-array
   #()
   (vector
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
                                (for/product ([d cell-shape]) d)))))))
(module+ test
  (check-equal?
   (remora
    ((rerank (1) R_behead)
     (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (4 2) 1 2 4 5 7 8 10 11)))
  (check-equal?
   (remora
    (R_behead
     (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (3 3) 3 4 5 6 7 8 9 10 11))))

(define R_curtail
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all])
        (define cell-shape (vector-drop (rem-array-shape arr) 1))
        (rem-array (vector-append
                    (vector (sub1 (vector-ref (rem-array-shape arr) 0)))
                    cell-shape)
                   (vector-drop-right (rem-array-data arr)
                                      (for/product ([d cell-shape]) d)))))))
(module+ test
  (check-equal?
   (remora
    ((rerank (1) R_curtail)
     (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (4 2) 0 1 3 4 6 7 9 10)))
  (check-equal?
   (remora
    (R_curtail
     (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (3 3) 0 1 2 3 4 5 6 7 8))))

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
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all])
        (define length (vector-ref (rem-array-shape arr) 0))
        (cell-list->array (reverse (array->cell-list arr -1))
                          (vector length)
                          (vector-drop (rem-array-shape arr) 1))))))
(module+ test
  (check-equal?
   (remora ((rerank (1) R_reverse)
            (alit (3 4) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (3 4) 3 2 1 0 7 6 5 4 11 10 9 8)))
  (check-equal?
   (remora (R_reverse
            (alit (3 4) 0 1 2 3 4 5 6 7 8 9 10 11)))
   (remora (alit (3 4) 8 9 10 11 4 5 6 7 0 1 2 3))))

(define R_append
  (rem-array
   #()
   (vector
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
                          cell-shape)))))

(module+ test
  (check-equal?
   (remora (R_append
            (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)
            (alit (2 3) 20 30 40 50 60 70)))
   (remora (alit (6 3) 0 1 2 3 4 5 6 7 8 9 10 11 20 30 40 50 60 70)))
  (check-equal?
   (remora
    ((rerank (1 1) R_append)
     (alit (3 4) 0 1 2 3 4 5 6 7 8 9 10 11)
     (alit (3 2) 20 30 40 50 60 70)))
   (remora (alit (3 6) 0 1 2 3 20 30 4 5 6 7 40 50 8 9 10 11 60 70))))
