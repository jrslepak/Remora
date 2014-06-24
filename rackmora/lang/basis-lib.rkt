#lang racket/base

(require "semantics.rkt"
         "syntax.rkt"
         racket/math
         racket/vector
         racket/list
         racket/contract
         racket/sequence)
(module+ test
  (require rackunit))

(provide (except-out (all-defined-out)
                     array->cell-list
                     cell-list->array))


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

(define R_take
  (rem-array
   #()
   (vector
    (Rλ ([n 0] [arr 'all])
        (define cell-shape (vector-drop (rem-array-shape arr) 1))
        (rem-box
         (rem-array (vector-append (vector (scalar->atom n)) cell-shape)
                    (vector-take (rem-array-data arr)
                                 (* (for/product ([d cell-shape]) d)
                                    (scalar->atom n)))))))))
(module+ test
  (check-equal?
   (remora (R_take 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (box (array (array 0 1 2)
                       (array 3 4 5)))))
  (check-equal?
   (remora ((rerank (0 1) R_take) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (box (array 0 1))
                  (box (array 3 4))
                  (box (array 6 7))))))

(define R_take-right
  (rem-array
   #()
   (vector
    (Rλ ([n 0] [arr 'all])
        (define cell-shape (vector-drop (rem-array-shape arr) 1))
        (rem-box
         (rem-array (vector-append (vector (scalar->atom n)) cell-shape)
                    (vector-take-right (rem-array-data arr)
                                       (* (for/product ([d cell-shape]) d)
                                          (scalar->atom n)))))))))
(module+ test
  (check-equal?
   (remora (R_take-right 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (box (array (array 3 4 5)
                       (array 6 7 8)))))
  (check-equal?
   (remora ((rerank (0 1) R_take-right) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (box (array 1 2))
                  (box (array 4 5))
                  (box (array 7 8))))))

(define R_drop
  (rem-array
   #()
   (vector
    (Rλ ([n 0] [arr 'all])
        (define cell-shape (vector-drop (rem-array-shape arr) 1))
        (rem-box
         (rem-array (vector-append
                     (vector (- (vector-ref (rem-array-shape arr) 0)
                                (scalar->atom n)))
                     cell-shape)
                    (vector-drop (rem-array-data arr)
                                 (* (for/product ([d cell-shape]) d)
                                    (scalar->atom n)))))))))
(module+ test
  (check-equal?
   (remora (R_drop 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (box (array (array 6 7 8)))))
  (check-equal?
   (remora ((rerank (0 1) R_drop) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (box (array 2))
                  (box (array 5))
                  (box (array 8))))))

(define R_drop-right
  (rem-array
   #()
   (vector
    (Rλ ([n 0] [arr 'all])
        (define cell-shape (vector-drop (rem-array-shape arr) 1))
        (rem-box
         (rem-array (vector-append
                     (vector (- (vector-ref (rem-array-shape arr) 0)
                                (scalar->atom n)))
                     cell-shape)
                    (vector-drop-right (rem-array-data arr)
                                       (* (for/product ([d cell-shape]) d)
                                          (scalar->atom n)))))))))
(module+ test
  (check-equal?
   (remora (R_drop-right 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (box (array (array 0 1 2)))))
  (check-equal?
   (remora ((rerank (0 1) R_drop-right) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (box (array 0))
                  (box (array 3))
                  (box (array 6))))))

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


;; randomly choose items from an array without replacement
(define R_deal
  (rem-array
   #()
   (vector
    (Rλ ([count 0]
         [arr 'all])
        (define cell-shape (vector-drop (rem-array-shape arr) 1))
        (define cells (array->cell-list arr -1))
        (define shuffled (shuffle cells))
        (define first-cells (take shuffled
                                  (vector-ref (rem-array-data count) 0)))
        (cell-list->array first-cells
                          (rem-array-data count)
                          cell-shape)))))
;; randomly permute a list
(remora (def R_shuffle (fn ((xs 'all)) (R_deal (R_tally xs) xs))))

;; Express a number in a given radix sequence
(define (antibase radix num)
  (define (antibase-internal radix num)
    (cond [(empty? radix) (list num)]
          [else (cons (quotient num (for/product ([d radix]) d))
                      (antibase-internal
                       (sequence-tail radix 1)
                       (remainder num
                                  (for/product ([d radix]) d))))]))
  (rest (antibase-internal radix num)))

(define R_antibase
  (rem-array
   #()
   (vector
    (Rλ ([radix 1]
         [num 0])
        (define digits (antibase (vector->list (rem-array-data radix))
                                 (vector-ref (rem-array-data num) 0)))
        (rem-array (vector (length digits))
                   (list->vector digits))))))
(module+ test
  (check-equal? (remora (R_antibase (alit (3) 3 2 4) (alit () 15)))
                (remora (alit (3) 1 1 3)))
  (check-equal? (remora (R_antibase (alit (3) 3 2 4) (alit (2) 15 25)))
                (remora (alit (2 3) 1 1 3 0 0 1)))
  (check-equal? (remora (R_antibase (alit (2 3) 2 5 1 3 2 4) (alit () 15)))
                (remora (alit (2 3) 1 0 0 1 1 3))))


(define (scan op xs)
  (reverse
   (for/fold ([acc (list (sequence-ref xs 0))])
     ([elt (sequence-tail xs 1)])
     (cons (op elt (first acc)) acc))))
; Interpret a digit list in a given radix
(define (base radix digits)
  ; if radix is too short, extend by copying its first element
  (define padded-radix
    (if (> (length digits) (length radix))
        (append (for/list ([c (- (length digits)
                                 (length radix))])
                  (first radix))
                radix)
        radix))
  ; if digits is too short, zero-extend it
  (define padded-digits
    (if (> (length radix) (length digits))
        (append (for/list ([c (- (length radix)
                                 (length digits))])
                  0)
                digits)
        digits))
  (for/sum ([place-value (reverse (scan * (cons 1 (reverse
                                                   (rest padded-radix)))))]
            [digit padded-digits])
    (* place-value digit)))
(define R_base
  (rem-array
   #()
   (vector
    (Rλ ([radix 1]
         [digits 1])
        (rem-array #() (vector
                        (base (vector->list (rem-array->vector radix))
                              (vector->list (rem-array->vector digits)))))))))
(module+ test
  (check-equal? (remora (R_base (alit (3) 3 2 4) (alit (3) 1 2 3)))
                (remora (alit () 19)))
  (check-equal? (remora (R_base (alit (1) 2) (alit (3) 1 0 1)))
                (remora (alit () 5)))
  (check-equal? (remora (R_base (alit (4) 7 24 60 60) (alit (3) 1 11 12)))
                (remora (alit () 4272))))

(define R_rotate
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all]
         [shift 0])
        (define cells (array->cell-list arr -1))
        (define shift-atom (vector-ref (rem-array-data shift) 0))
        (define actual-shift
          (cond [(equal? 0 shift-atom) 0]
                [(negative? shift-atom) (+ (length cells)
                                           (modulo shift-atom (length cells)))]
                [else (modulo shift-atom (length cells))]))
        (cell-list->array (append (drop cells actual-shift)
                                  (take cells actual-shift))
                          (vector (length cells))
                          (vector-drop (rem-array-shape arr) 1))))))
(module+ test
  (check-equal? (remora (R_rotate (array 1 2 3 4 5) 1))
                (remora (array 2 3 4 5 1)))
  (check-equal? (remora (R_rotate (array (array 0 1)
                                         (array 2 3)
                                         (array 4 5))
                                  1))
                (remora (array (array 2 3)
                               (array 4 5)
                               (array 0 1))))
  (check-equal? (remora ((rerank (1 0) R_rotate) (array (array 0 1)
                                                        (array 2 3)
                                                        (array 4 5))
                                  1))
                (remora (array (array 1 0)
                               (array 3 2)
                               (array 5 4))))
  (check-equal? (remora (R_rotate (array 0 1 2 3 4 5) (array 0 1 2 3)))
                (remora (array (array 0 1 2 3 4 5)
                               (array 1 2 3 4 5 0)
                               (array 2 3 4 5 0 1)
                               (array 3 4 5 0 1 2)))))

(define R_shape-of
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all])
        (rem-array (vector (vector-length (rem-array-shape arr)))
                   (rem-array-shape arr))))))
(module+ test
  (check-equal? (remora (R_shape-of (alit (4 1 2) 3 6 2 3 5 2 3 4)))
                (remora (array 4 1 2))))

(define R_reshape
  (rem-array
   #()
   (vector
    (Rλ ([new-shape 1]
         [arr 'all])
        (define new-elt-count (for/product ([d (rem-array-data new-shape)]) d))
        (define old-elts (rem-array-data arr))
        (define old-elt-count (vector-length old-elts))
        (rem-box
         (rem-array
          (rem-array-data new-shape)
          (vector-take
           (apply vector-append
                  (for/list ([i (ceiling (/ new-elt-count old-elt-count))])
                    old-elts))
           new-elt-count)))))))
(module+ test
  (check-equal? (remora (R_reshape (array 3 2)
                                   (alit (9) 1 2 3 4 5 6 7 8 9)))
                (remora (box (array (array 1 2)
                                    (array 3 4)
                                    (array 5 6)))))
  (check-equal? (remora (R_reshape (array 3 2)
                                   (alit (5) 'a 'b 'c 'd 'e)))
                (remora (box (array (array 'a 'b)
                                    (array 'c 'd)
                                    (array 'e 'a)))))
  (check-equal? (remora (R_reshape (array (array 3 2)
                                          (array 2 3)
                                          (array 3 3))
                                   (alit (9) 1 2 3 4 5 6 7 8 9)))
                (remora (array (box (array (array 1 2)
                                           (array 3 4)
                                           (array 5 6)))
                               (box (array (array 1 2 3)
                                           (array 4 5 6)))
                               (box (array (array 1 2 3)
                                           (array 4 5 6)
                                           (array 7 8 9)))))))

(define R_iota
  (rem-array
   #()
   (vector
    (Rλ ([shape 1])
        (define size (for/product ([d (rem-array-data shape)]) d))
        (rem-box (rem-array (rem-array-data shape)
                            (for/vector ([i size]) i)))))))
(module+ test
  (check-equal? (remora (R_iota (array 4)))
                (remora (box (array 0 1 2 3))))
  (check-equal? (remora (R_iota (array 4 3)))
                (remora (box (array (array 0 1 2)
                                    (array 3 4 5)
                                    (array 6 7 8)
                                    (array 9 10 11)))))
  (check-equal? (remora (R_iota (array (array 4)
                                       (array 3))))
                (remora (array (box (array 0 1 2 3))
                               (box (array 0 1 2))))))


(define (list-nub xs [already-seen '()])
  (cond [(empty? xs) '()]
        [(member (first xs) already-seen) (list-nub (rest xs) already-seen)]
        [else (cons (first xs)
                    (list-nub (rest xs)
                              (cons (first xs) already-seen)))]))
(define R_nub
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all])
        (define cells
          (list-nub (array->cell-list arr -1)))
        (cell-list->array cells
                          (vector (length cells))
                          (vector-drop (rem-array-shape arr) 1))))))
(module+ test
  (check-equal? (remora (R_nub (array 1 4 2 3 1 9 2 3 8 2)))
                (remora (array 1 4 2 3 9 8)))
  (check-equal? (remora (R_nub (array (array 1 4)
                                      (array 2 3)
                                      (array 1 9)
                                      (array 2 3)
                                      (array 8 2))))
                (remora (array (array 1 4)
                               (array 2 3)
                               (array 1 9)
                               (array 8 2)))))

(define (list-nub-sieve xs [already-seen '()])
  (cond [(empty? xs) '()]
        [(member (first xs) already-seen)
         (cons #f (list-nub-sieve (rest xs) already-seen))]
        [else (cons #t (list-nub-sieve (rest xs)
                                       (cons (first xs) already-seen)))]))
(define R_nub-sieve
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all])
        (define cells
          (list-nub-sieve (array->cell-list arr -1)))
        (rem-array (vector (length cells))
                   (list->vector cells))))))
(module+ test
  (check-equal? (remora (R_nub-sieve (array 1 4 2 3 1 9 2 3 8 2)))
                (remora (array #t #t #t #t #f #t #f #f #t #f)))
  (check-equal? (remora (R_nub-sieve (array (array 1 4)
                                            (array 2 3)
                                            (array 1 9)
                                            (array 2 3)
                                            (array 8 2))))
                (remora (array #t #t #t #f #t))))

(define R_ravel
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all])
        (rem-array (vector (for/product ([d (rem-array-shape arr)]) d))
                   (rem-array-data arr))))))
(module+ test
  (check-equal? (remora (R_ravel (array 1 2 3 4)))
                (remora (array 1 2 3 4)))
  (check-equal? (remora (R_ravel (array (array 1 2)
                                        (array 3 4))))
                (remora (array 1 2 3 4))))

(define R_itemize
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all])
        (rem-array (vector-append #(1) (rem-array-shape arr))
                   (rem-array-data arr))))))
(module+ test
  (check-equal? (remora (R_itemize (array 1 2 3 4)))
                (remora (array (array 1 2 3 4))))
  (check-equal? (remora ((rerank (0) R_itemize) (array 1 2 3 4)))
                (remora (array (array 1)
                               (array 2)
                               (array 3)
                               (array 4))))
  (check-equal? (remora (R_itemize (array (array 1 2)
                                          (array 3 4))))
                (remora (array (array (array 1 2)
                                      (array 3 4))))))

(define R_tally
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all])
        (rem-array #() (vector (vector-ref (rem-array-shape arr) 0)))))))
(module+ test
  (check-equal? (remora (R_tally (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
                (remora 4))
  (check-equal? (remora ((rerank (1) R_tally)
                         (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
                (remora (array 3 3 3 3))))


(define R_equal
  (rem-array
   #()
   (vector
    (Rλ ([xs 'all] [ys 'all])
        (rem-array #() (vector (equal? xs ys)))))))
(module+ test
  (check-equal? (remora (R_equal (array 1 1 1 1) 1))
                (remora #f))
  (check-equal? (remora ((rerank (0 0) R_equal) (array 1 1 1 1) 1))
                (remora (array #t #t #t #t)))
  (check-equal? (remora ((rerank (0 0) R_equal) (array 0 1 2 3) 1))
                (remora (array #f #t #f #f))))

;; print a whole array structure (don't just lift and print atoms one-by-one)
(define R_show
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all]) (rem-array #() (vector (print arr)))))))

;; read a whole array structure
(define R_read
  (rem-array
   #()
   (vector
    (Rλ ([port 0]) (list->array (read (vector-ref (rem-array-data port) 0)))))))


;; right fold a list of Remora arrays using a Remora function array
(define (rem-foldr op base arrays)
  (cond [(empty? arrays) base]
        [else (remora-apply op
                            (first arrays)
                            (rem-foldr op base (rest arrays)))]))
(define R_foldr
  (rem-array
   #()
   (vector
    (Rλ ([op 'all] [base 'all] [arr 'all])
        (rem-foldr op base (array->cell-list arr -1))))))
(module+ test
  (check-equal? (remora (R_foldr + 0 (array 1 2 3 4)))
                (remora 10))
  (check-equal? (remora (R_foldr - 0 (array 1 2 3 4)))
                (remora -2))
  (check-equal? (remora (R_foldr (array + -) 0 (array 1 2 3 4)))
                (remora (array 10 -2))))

;; left fold a list of Remora arrays using a Remora function array
(define (rem-foldl op base arrays)
  (cond [(empty? arrays) base]
        [else
         (rem-foldl op (remora-apply op base (first arrays)) (rest arrays))
         #;(remora-apply op
                         (first arrays)
                         (rem-foldr op base (rest arrays)))]))
(define R_foldl
  (rem-array
   #()
   (vector
    (Rλ ([op 'all] [base 'all] [arr 'all])
        (rem-foldl op base (array->cell-list arr -1))))))
(module+ test
  (check-equal? (remora (R_foldl + 0 (array 1 2 3 4)))
                (remora 10))
  (check-equal? (remora (R_foldl - 0 (array 1 2 3 4)))
                (remora -10))
  (check-equal? (remora (R_foldl (array + -) 0 (array 1 2 3 4)))
                (remora (array 10 -10))))


;; Extract a box's contents
;; Applying this to an array of boxes risks producing result cells with
;; mismatching shapes.
(define R_unsafe-unbox
  (rem-array #()
             (vector
              (Rλ ([b 0])
                  (if (rem-box? b)
                      (rem-box-contents b)
                      (printf "oops, b is ~s\n" b))))))
