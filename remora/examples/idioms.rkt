#lang remora/dynamic

(require racket/function)

(provide (all-defined-out))

;;; An implementation of some sample items from the APL idioms list in Remora

;;; Drop leading spaces from a character vector
(def (drop-leading-space-vec (str 1))
  (filter (behead (scan or #f (not (equal? #\space str))))
          str))

(def (drop-leading-space (str 0))
  (array->string (drop-leading-space-vec (string->array str))))

;;; Drop trailing spaces from a character vector
(def (drop-trailing-space-vec (str 1))
  (filter (reverse (behead (scan or #f (not (equal? #\space (reverse str))))))
          str))

(def (drop-trailing-space (str 0))
  (array->string (drop-trailing-space-vec (string->array str))))

;;; Collapse multiple consecutive spaces to a single space
(def (collapse-multiple-space-vec (str 1))
  (filter (or (not (equal? #\space str))
              (behead (rotate (append [#t] (not (equal? #\space str))) 1)))
          str))

;;; Collapse multiple consectutive spaces in a string to a single space
(def (collapse-multiple-space (str 0))
  (array->string (collapse-multiple-space-vec (string->array str))))

;;; ravel an array, apply a function to it, then reshape the new values
;;; to the shape of the original array
(define (flat-apply (op 0) (arr all))
  (reshape (shape-of arr) (op (ravel arr))))

;;; count the occurances of a value
(define (count-value (arr all) (value 0))
  (foldr + 0 (bool->int ((curry equal value) (ravel arr)))))

;;; if the array contains the given atom
(define (contains? (arr all) (element 0))
  (foldr or #f ((curry equal element) (ravel arr))))

;;; Change zero values to n
(define (zero-to-n (v 0) (n 0))
  (select (equal v 0) n v))

;;; Implementations of sample functions from the J Phrases list

;;; Moves all blanks in a string to the end of the string
(define (move-blanks (str 0))
  (define chars (string->array str))
  (define blanks ((curry equal? #\space) chars))
  (define blank-count (foldr + 0 (bool->int blanks)))
  (array->string (append (filter (not blanks) chars) (reshape [blank-count] #\space))))

;;; converts integer values to booleans
(define (int->bool (n 0))
  (select (equal n 0) #f #t))

;;; creates a checkerboard of 0s and 1s of dimension n by n
(define (checkerboard (n 0))
  (define r1 (modulo (iota [n]) 2))
  (define r2 (modulo (+ 1 (iota [n])) 2))
  (reshape [n n] (append r1 r2)))

;;; creates a checkerboard of #f and #t of dimension n by n
(define (boolean-checkerboard (n 0))
  (int->bool (checkerboard n)))

;;; transpose a matrix
(define (matrix-transpose (matrix 2))
  (define cons (fn ((car 0) (cdr 1))
                   (append (itemize car) cdr)))
  (foldr cons [] matrix))

;;; position matrix of size n by n with positions represented as '(x y)
(define (position-matrix (n 0))
  (define x (reshape [n n] (iota [n])))
  (define y (matrix-transpose x))
  (#r(0 0)array x y))

;;; indices for a truth table of size n by n
(define (truth-indices (n 0))
  (reshape (append (reshape [n] 2) [n]) (antibase (reshape [n] 2) (iota [(expt n 2)]))))

;;; a truth table of order n of function f
(define (truth-table (f all) (n 0))
  (#r(1)f (int->bool (truth-indices n))))

;;; Construct a histogram of a vector of naturals
;;; The "bar" for each number is a sequence of #t, followed by as many #f as
;;; needed to fill the remaining space.
(def (histogram (vec 1))
  (unbox count (iota [(inexact->exact (reduce max -inf.0 vec))])
    (box (#r(0 1)>= vec (add1 count)))))

;;; Construct boxed vector of integers counting from left to right, either up
;;; or down as needed.
(def (range (left 0) (right 0))
  (unbox count (iota [(abs (- right left))])
    (append [left]
            (+ left (* (signum (- right left))
                       (add1 count))))))

;;; Determine how many decimal digits are needed to represent a positive number
(def (num-digits (num 0))
  (floor (add1 (log num))))

