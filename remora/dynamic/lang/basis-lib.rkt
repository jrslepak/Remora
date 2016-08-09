#lang racket/base

(require "semantics.rkt"
         "syntax.rkt"
         racket/math
         racket/vector
         racket/list
         racket/contract
         racket/sequence
         racket/provide
         (for-syntax racket/base))
(module+ test
  (require rackunit))

;;; Remora primops are prefixed with "R_"
(provide (filtered-out
          (λ (name) (if (regexp-match #rx"^R_" name) name #f))
          (all-defined-out)))


(define R_id (rem-array #() (vector (rem-scalar-proc (λ (x) x) 1))))

(define R_and (rem-array #() (vector (rem-scalar-proc (λ (x y) (and x y)) 2))))
(define R_or (rem-array #() (vector (rem-scalar-proc (λ (x y) (or x y)) 2))))

(define R_neg (rem-array #() (vector (rem-scalar-proc (λ (x) (- x)) 1))))
(define R_inv (rem-array #() (vector (rem-scalar-proc (λ (x) (/ 1 x)) 1))))

(define R_signum
  (rem-array #() (vector (rem-scalar-proc (λ (x) (/ x (magnitude x))) 1))))

(define (logb b x) (/ (log x) (log b)))
(define R_logb (rem-array #() (vector (rem-scalar-proc logb 2))))
(define R_ln (rem-array #() (vector (rem-scalar-proc log 1))))
(define R_log (rem-array #() (vector (rem-scalar-proc (λ (x) (logb 10 x)) 1))))
(define R_lg (rem-array #() (vector (rem-scalar-proc (λ (x) (logb 2 x)) 1))))

(define R_bool->int
  (rem-array #() (vector (rem-scalar-proc (λ (b) (if b 1 0)) 1))))



;;; "cell-shape" here refers to the -1-cells which will be pushed around
;;; "length" is how many -1-cells there are
(define R_head
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all])
               ;; operates on the -1-cells
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
        ;; operates on the -1-cells
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
        (rem-array (vector-append (vector (scalar->atom n)) cell-shape)
                   (vector-take (rem-array-data arr)
                                (* (for/product ([d cell-shape]) d)
                                   (scalar->atom n))))))))
(module+ test
  (check-equal?
   (remora (R_take 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (array 0 1 2)
                  (array 3 4 5))))
  (check-equal?
   (remora ((rerank (0 1) R_take) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (array 0 1)
                  (array 3 4)
                  (array 6 7)))))
(define R_take*
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
   (remora (R_take* 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (box (array (array 0 1 2)
                       (array 3 4 5)))))
  (check-equal?
   (remora ((rerank (0 1) R_take*) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (box (array 0 1))
                  (box (array 3 4))
                  (box (array 6 7))))))

(define R_take-right
  (rem-array
   #()
   (vector
    (Rλ ([n 0] [arr 'all])
        (define cell-shape (vector-drop (rem-array-shape arr) 1))
        (rem-array (vector-append (vector (scalar->atom n)) cell-shape)
                   (vector-take-right (rem-array-data arr)
                                      (* (for/product ([d cell-shape]) d)
                                         (scalar->atom n))))))))
(module+ test
  (check-equal?
   (remora (R_take-right 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (array 3 4 5)
                  (array 6 7 8))))
  (check-equal?
   (remora ((rerank (0 1) R_take-right) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (array 1 2)
                  (array 4 5)
                  (array 7 8)))))
(define R_take-right*
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
   (remora (R_take-right* 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (box (array (array 3 4 5)
                       (array 6 7 8)))))
  (check-equal?
   (remora ((rerank (0 1) R_take-right*) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (box (array 1 2))
                  (box (array 4 5))
                  (box (array 7 8))))))

(define R_drop
  (rem-array
   #()
   (vector
    (Rλ ([n 0] [arr 'all])
        (define cell-shape (vector-drop (rem-array-shape arr) 1))
        (rem-array (vector-append
                    (vector (- (vector-ref (rem-array-shape arr) 0)
                               (scalar->atom n)))
                    cell-shape)
                   (vector-drop (rem-array-data arr)
                                (* (for/product ([d cell-shape]) d)
                                   (scalar->atom n))))))))
(module+ test
  (check-equal?
   (remora (R_drop 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (array 6 7 8))))
  (check-equal?
   (remora ((rerank (0 1) R_drop) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (array 2)
                  (array 5)
                  (array 8)))))
(define R_drop*
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
   (remora (R_drop* 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (box (array (array 6 7 8)))))
  (check-equal?
   (remora ((rerank (0 1) R_drop*) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (box (array 2))
                  (box (array 5))
                  (box (array 8))))))

(define R_drop-right
  (rem-array
   #()
   (vector
    (Rλ ([n 0] [arr 'all])
        (define cell-shape (vector-drop (rem-array-shape arr) 1))
        (rem-array (vector-append
                    (vector (- (vector-ref (rem-array-shape arr) 0)
                               (scalar->atom n)))
                    cell-shape)
                   (vector-drop-right (rem-array-data arr)
                                      (* (for/product ([d cell-shape]) d)
                                         (scalar->atom n))))))))
(module+ test
  (check-equal?
   (remora (R_drop-right 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (array 0 1 2))))
  (check-equal?
   (remora ((rerank (0 1) R_drop-right) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (array 0)
                  (array 3)
                  (array 6)))))
(define R_drop-right*
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
   (remora (R_drop-right* 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (box (array (array 0 1 2)))))
  (check-equal?
   (remora ((rerank (0 1) R_drop-right*) 2 (alit (3 3) 0 1 2 3 4 5 6 7 8)))
   (remora (array (box (array 0))
                  (box (array 3))
                  (box (array 6))))))

;;; Split an array into a list of cells of a given rank
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

;;; Merge a list of cells into an array with the given frame shape
;;; If there are no cells in the list (i.e. empty frame), specify a cell shape
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


;;; randomly choose items from an array without replacement
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
;;; randomly permute a list
(remora (def R_shuffle (fn ((xs all)) (R_deal (R_length xs) xs))))

;;; Express a number in a given radix sequence
;;; TODO: permit +inf.0 so it can be used in outermost digit
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


(define (scan op init xs)
  (reverse
   (for/fold ([acc (list init) #;(list (sequence-ref xs 0))])
     ([elt xs #;(sequence-tail xs 1)])
     (cons (op (first acc) elt) acc))))

(define R_scan
  (rem-array
   #()
   (vector
    (Rλ ([op 'all] [init 'all] [xs 'all])
        (define input-items (array->cell-list xs -1))
        #;(printf "input items: ~s\n" input-items)
        (define result-items
          (scan (λ (left right)
                  #;(printf "emitted ~s, next is ~s\n" left right)
                  (remora-apply op left right))
                init
                input-items))
        #;(printf "result items: ~s\n" result-items)
        (if (empty? result-items)
            (cell-list->array
             result-items
             (vector (length result-items))
             (rem-array-shape init))
            (cell-list->array
             result-items
             (vector (length result-items))))))))
(module+ test
  (check-equal? (remora (R_scan - 0 (array 1 2 3)))
                (remora (array 0 -1 -3 -6)))
  (check-equal? (remora (R_scan + (array 0 0) (array (array 1 2)
                                                     (array 3 4))))
                (remora (array (array 0 0)
                               (array 1 2)
                               (array 4 6))))
  (check-equal? (remora ((rerank (all all 1) R_scan)
                         +
                         0 (array (array 1 2)
                                  (array 3 4))))
                (remora (array (array 0 1 3)
                               (array 0 3 7)))))


;;; Interpret a digit list in a given radix
(define (base radix digits)
  ;; if radix is too short, extend by copying its first element
  (define padded-radix
    (if (> (length digits) (length radix))
        (append (for/list ([c (- (length digits)
                                 (length radix))])
                  (first radix))
                radix)
        radix))
  ;; if digits is too short, zero-extend it
  (define padded-digits
    (if (> (length radix) (length digits))
        (append (for/list ([c (- (length radix)
                                 (length digits))])
                  0)
                digits)
        digits))
  (for/sum ([place-value (reverse (scan * 1 (reverse
                                             (rest padded-radix))
                                        #;(cons 1 (reverse
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
                [(negative? shift-atom) (modulo shift-atom (length cells))]
                [else (modulo shift-atom (length cells))]))
        (cell-list->array (append (drop cells actual-shift)
                                  (take cells actual-shift))
                          (vector (length cells))
                          (vector-drop (rem-array-shape arr) 1))))))
(module+ test
  (check-equal? (remora (R_rotate (array 1 2 3 4 5) 1))
                (remora (array 2 3 4 5 1)))
  (check-equal? (remora (R_rotate (array 1 2 3 4 5) -1))
                (remora (array 5 1 2 3 4)))
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
        (rem-array (vector (vector-length (rem-value-shape arr)))
                   (rem-value-shape arr))))))
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
        (rem-array
         (rem-array-data new-shape)
         (vector-take
          (apply vector-append
                 (for/list ([i (ceiling (/ new-elt-count old-elt-count))])
                   old-elts))
          new-elt-count))))))
(module+ test
  (check-equal? (remora (R_reshape (array 3 2)
                                   (alit (9) 1 2 3 4 5 6 7 8 9)))
                (remora (array (array 1 2)
                               (array 3 4)
                               (array 5 6))))
  (check-equal? (remora (R_reshape (array 3 2)
                                   (alit (5) 'a 'b 'c 'd 'e)))
                (remora (array (array 'a 'b)
                               (array 'c 'd)
                               (array 'e 'a)))))
(define R_reshape*
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
  (check-equal? (remora (R_reshape* (array 3 2)
                                   (alit (9) 1 2 3 4 5 6 7 8 9)))
                (remora (box (array (array 1 2)
                                    (array 3 4)
                                    (array 5 6)))))
  (check-equal? (remora (R_reshape* (array 3 2)
                                   (alit (5) 'a 'b 'c 'd 'e)))
                (remora (box (array (array 'a 'b)
                                    (array 'c 'd)
                                    (array 'e 'a)))))
  (check-equal? (remora (R_reshape* (array (array 3 2)
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
        (rem-array (rem-array-data shape)
                   (for/vector ([i size]) i))))))
(module+ test
  (check-equal? (remora (R_iota (array 4)))
                (remora (array 0 1 2 3)))
  (check-equal? (remora (R_iota (array 4 3)))
                (remora (array (array 0 1 2)
                               (array 3 4 5)
                               (array 6 7 8)
                               (array 9 10 11)))))
(define R_iota*
  (rem-array
   #()
   (vector
    (Rλ ([shape 1])
        (define size (for/product ([d (rem-array-data shape)]) d))
        (rem-box (rem-array (rem-array-data shape)
                            (for/vector ([i size]) i)))))))
(module+ test
  (check-equal? (remora (R_iota* (array 4)))
                (remora (box (array 0 1 2 3))))
  (check-equal? (remora (R_iota* (array 4 3)))
                (remora (box (array (array 0 1 2)
                                    (array 3 4 5)
                                    (array 6 7 8)
                                    (array 9 10 11)))))
  (check-equal? (remora (R_iota* (array (array 4)
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

(define R_length
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all])
        (rem-array #() (vector (vector-ref (rem-array-shape arr) 0)))))))
(module+ test
  (check-equal? (remora (R_length (alit (4 3) 0 1 2 3 4 5 6 7 8 9 10 11)))
                (remora 4))
  (check-equal? (remora ((rerank (1) R_length)
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

;;; print a whole array structure (don't just lift and print atoms one-by-one)
;;; TODO: need a version of this that lets caller specify an output port
(define R_show
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all]) (rem-array #() (vector (print arr)))))))

;;; read a whole array structure
(define R_read
  (rem-array
   #()
   (vector
    (Rλ ([port 0]) (list->array (read (vector-ref (rem-array-data port) 0)))))))


;;; right fold a list of Remora arrays using a Remora function array
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

;;; left fold a list of Remora arrays using a Remora function array
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


;;; reduce a vector (not list!) of Remora arrays using a Remora function array
;;; note: reduce assumes associativity and has no "base" value
(define (rem-reduce op base arrays)
  (cond [(= 0 (vector-length arrays)) base]
        [(= 1 (vector-length arrays)) (vector-ref arrays 0)]
        [else (define-values (left right)
                (vector-split-at arrays (ceiling (/ (vector-length arrays) 2))))
              (remora-apply op (rem-reduce op base left) (rem-reduce op base right))]))
(define R_reduce
  (rem-array
   #()
   (vector
    (Rλ ([op 'all] [base 'all] [arr 'all])
        (rem-reduce op base (list->vector (array->cell-list arr -1)))))))
(module+ test
  (check-equal? (remora (R_reduce + 0 (array 1 2 3 4)))
                (remora 10))
  (check-equal? (remora (R_reduce (array + *) (array 0 1) (array 1 2 3 4)))
                (remora (array 10 24))))

;;; Extract a box's contents
;;; Applying this to an array of boxes risks producing result cells with
;;; mismatching shapes.
(define R_unsafe-unbox
  (rem-array #()
             (vector
              (Rλ ([b 0])
                  (if (rem-box? b)
                      (rem-box-contents b)
                      (printf "oops, b is ~s\n" b))))))


(define R_filter
  (rem-array
   #()
   (vector
    (Rλ ([bools 1] [items 'all])
        (define cell-shape (vector-drop (rem-array-shape items) 1))
        (define old-cells (array->cell-list items -1))
        (define choices (rem-array-data bools))
        (define new-cells 
          (for/list ([b choices] [c old-cells] #:when b) c))
        (cell-list->array new-cells
                          (vector (length new-cells))
                          cell-shape)))))
(module+ test
  (check-equal? (remora (R_filter (array #t #t #f #t #f #t)
                                  (array 1 2 #t 3 #f 4)))
                (remora (array 1 2 3 4)))
  (check-equal? (remora (R_filter (array #f #f #t #f #t #f)
                                  (array 1 2 #t 3 #f 4)))
                (remora (array #t #f)))
  (check-equal? (remora ((rerank (all 1) R_filter)
                         (array #t #f #t)
                         (array (array 1 2 3)
                                (array 4 5 6))))
                (remora (array (array 1 3)
                               (array 4 6))))
  (check-equal? (remora (R_filter (array #t #f #f #t)
                                  (array (array 1 2 3 0)
                                         (array 0 1 3 2)
                                         (array 2 4 5 9)
                                         (array 6 6 6 0))))
                (remora (array (array 1 2 3 0)
                                    (array 6 6 6 0)))))
(define R_filter*
  (rem-array
   #()
   (vector
    (Rλ ([bools 1] [items 'all])
        (define cell-shape (vector-drop (rem-array-shape items) 1))
        (define old-cells (array->cell-list items -1))
        (define choices (rem-array-data bools))
        (define new-cells 
          (for/list ([b choices] [c old-cells] #:when b) c))
        (rem-box
         (cell-list->array new-cells
                           (vector (length new-cells))
                           cell-shape))))))
(module+ test
  (check-equal? (remora (R_filter* (array #t #t #f #t #f #t)
                                   (array 1 2 #t 3 #f 4)))
                (remora (box (array 1 2 3 4))))
  (check-equal? (remora (R_filter* (array #f #f #t #f #t #f)
                                   (array 1 2 #t 3 #f 4)))
                (remora (box (array #t #f))))
  (check-equal? (remora ((rerank (all 1) R_filter)
                         (array #t #f #t)
                         (array (array 1 2 3)
                                (array 4 5 6))))
                (remora (array (array 1 3)
                               (array 4 6))))
  (check-equal? (remora (R_filter* (array #t #f #f #t)
                                   (array (array 1 2 3 0)
                                          (array 0 1 3 2)
                                          (array 2 4 5 9)
                                          (array 6 6 6 0))))
                (remora (box (array (array 1 2 3 0)
                                    (array 6 6 6 0))))))

(define R_select
  (rem-array
   #()
   (vector
    (Rλ ([bool 0] [a 'all] [b 'all])
        (if (scalar->atom bool) a b)))))
(module+ test
  (check-equal? (remora (R_select #t (array 1 2 3) (array 4 5 6)))
                (remora (array 1 2 3)))
  (check-equal? (remora (R_select #f (array 1 2 3) (array 4 5 6)))
                (remora (array 4 5 6)))
  (check-equal? (remora (R_select (array #t #f) (array 1 2 3) (array 4 5 6)))
                (remora (array (array 1 2 3) (array 4 5 6))))
  (check-equal? (remora ((rerank (0 0 0) R_select) (array #t #f #t)
                                                   (array 1 2 3)
                                                   (array 4 5 6)))
                (remora (array 1 5 3))))

;;; Enable "sliding window" computation over a vector. Subsequences of specified
;;; length are aligned along the major axis for easy folding.
(define R_window
  (rem-array
   #()
   (vector
    (Rλ ([length 0] [arr 'all])
        (R_rotate arr (R_iota (R_itemize length)))))))
(module+ test
  (check-equal? (remora (R_window 3 (array 1 2 3 4 5 6 7 8 9 10)))
                (remora (array (array 1 2 3 4 5 6 7 8 9 10)
                               (array 2 3 4 5 6 7 8 9 10 1)
                               (array 3 4 5 6 7 8 9 10 1 2))))
  (check-equal? (remora ((rerank (0 1) R_window) 3
                                                 (array (array 1 2 3 4 5)
                                                        (array 6 7 8 9 10))))
                (remora [array [array [array 1 2 3 4 5]
                                      [array 2 3 4 5 1]
                                      [array 3 4 5 1 2]]
                               [array [array 6 7 8 9 10]
                                      [array 7 8 9 10 6]
                                      [array 8 9 10 6 7]]])))



;;; Convert a Racket nested list into a Remora array
(define R_list->array
  (rem-array
   #()
   (vector
    (Rλ ([lst 0])
        (list->array (scalar->atom lst))))))

;;; Convert a Remora array to a nested Racket list containing its atoms
(define R_array->nest-list
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all])
        (rem-array #() (vector (array->nest-list arr)))))))
(module+ test
  (check-equal? (remora (R_array->nest-list 4))
                (remora 4))
  (check-equal? (remora (R_array->nest-list (array 1 2 3 4 5 6)))
                (remora '(1 2 3 4 5 6)))
  (check-equal? (remora (R_array->nest-list (array (array 1 2 3 4 5 6))))
                (remora '((1 2 3 4 5 6))))
  (check-equal? (remora (R_array->nest-list (array (array 1 2 3)
                                              (array 4 5 6))))
                (remora '((1 2 3) (4 5 6))))
  (check-equal? (remora (R_array->nest-list (array (array 1 2)
                                              (array 3 4)
                                              (array 5 6))))
                (remora '((1 2) (3 4) (5 6)))))
;;; Convert a Remora array to a nested Racket vector containing its atoms
(define R_array->nest-vector
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all])
        (rem-array #() (vector (array->nest-vector arr)))))))
(module+ test
  (check-equal? (remora (R_array->nest-vector 4))
                (remora 4))
  (check-equal? (remora (R_array->nest-vector (array 1 2 3 4 5 6)))
                (remora '#(1 2 3 4 5 6)))
  (check-equal? (remora (R_array->nest-vector (array (array 1 2 3 4 5 6))))
                (remora '#(#(1 2 3 4 5 6))))
  (check-equal? (remora (R_array->nest-vector (array (array 1 2 3)
                                              (array 4 5 6))))
                (remora '#(#(1 2 3) #(4 5 6))))
  (check-equal? (remora (R_array->nest-vector (array (array 1 2)
                                              (array 3 4)
                                              (array 5 6))))
                (remora '#(#(1 2) #(3 4) #(5 6)))))
;;; Convert a Remora array to a flattened Racket list containing its atoms
(define R_array->flat-list
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all])
        (rem-array #() (vector (vector->list (rem-array-data arr))))))))
(module+ test
  (check-equal? (remora (R_array->flat-list 4))
                (remora '(4)))
  (check-equal? (remora (R_array->flat-list (array 1 2 3 4 5 6)))
                (remora '(1 2 3 4 5 6)))
  (check-equal? (remora (R_array->flat-list (array (array 1 2 3 4 5 6))))
                (remora '(1 2 3 4 5 6)))
  (check-equal? (remora (R_array->flat-list (array (array 1 2 3)
                                                   (array 4 5 6))))
                (remora '(1 2 3 4 5 6)))
  (check-equal? (remora (R_array->flat-list (array (array 1 2)
                                                   (array 3 4)
                                                   (array 5 6))))
                (remora '(1 2 3 4 5 6))))
;;; Convert a Remora array to a flattened Racket vector containing its atoms
(define R_array->flat-vector
  (rem-array
   #()
   (vector
    (Rλ ([arr 'all])
        (rem-array #() (vector (rem-array-data arr)))))))
(module+ test
  (check-equal? (remora (R_array->flat-vector 4))
                (remora '#(4)))
  (check-equal? (remora (R_array->flat-vector (array 1 2 3 4 5 6)))
                (remora '#(1 2 3 4 5 6)))
  (check-equal? (remora (R_array->flat-vector (array (array 1 2 3 4 5 6))))
                (remora '#(1 2 3 4 5 6)))
  (check-equal? (remora (R_array->flat-vector (array (array 1 2 3)
                                                     (array 4 5 6))))
                (remora '#(1 2 3 4 5 6)))
  (check-equal? (remora (R_array->flat-vector (array (array 1 2)
                                                     (array 3 4)
                                                     (array 5 6))))
                (remora '#(1 2 3 4 5 6))))

;;; Convert a Racket string to a Remora vector containing its characters
(remora (def R_string->array
          (fn ((str 0)) (R_list->array (string->list str)))))

;;; Construct a Racket string from a Remora vector of characters
(remora (def R_array->string
          (fn ((arr 1)) (list->string (R_array->nest-list arr)))))

;;; Search an associative array (vector of length-2 lists)
;;; We won't be able to type this without a cast that effectively assumes at
;;; least one result is found.
(remora
 (def R_lookup
   (fn ((needle all) (haystack 1))
     (unbox results (R_filter
                     ((fn ((table-entry 0)) (R_equal needle
                                                     (first table-entry)))
                      haystack)
                     haystack)
       (second (R_head results))))))
;;; Like R_lookup but with an alternative value to use in case the desired key
;;; is not present
(remora
 (def R_lookup+
   (fn ((needle all) (haystack 1) (alternate all))
     (unbox results (R_filter
                     ((fn ((table-entry 0)) (R_equal needle
                                                     (first table-entry)))
                      haystack)
                     haystack)
       (second (R_head (R_append results [array
                                          (list 'missing alternate)])))))))
;;; Like R_lookup but returning every matching value
(remora
 (def R_lookup*
   (fn ((needle all) (haystack 1) (alternate all))
     (unbox results (R_filter
                     ((fn ((table-entry 0)) (R_equal needle
                                                     (first table-entry)))
                      haystack)
                     haystack)
       (second (R_head (R_append results [array
                                          (list 'missing alternate)])))))))

;;; Construct and destruct pairs
(define R_pair
  (rem-array
   #()
   (vector (Rλ ([new-car all] [new-cdr all])
               (rem-array #() (vector (cons new-car new-cdr)))))))
(define R_fst
  (rem-array
   #()
   (vector (Rλ ([pair 0])
               (car (scalar->atom pair))
               #;(rem-array #() (vector (car (scalar->atom pair))))))))
(define R_snd
  (rem-array
   #()
   (vector (Rλ ([pair 0])
               (cdr (scalar->atom pair))
               #;(rem-array #() (vector (cdr (scalar->atom pair))))))))
