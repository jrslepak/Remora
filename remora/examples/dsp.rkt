#lang remora/dynamic

;;; a simple yet naieve low pass filter
;;; y(n) = x(n) + x(n - 1)
(define (simple-low-pass (seed 0) (data 1))
  (define lpblock (curtail (append [0] (rotate data 1))))
  (+ data lpblock))

;;; a simple yet naieve high pass filter
;;; y(n) = x(n) - x(n - 1)
(define (simple-high-pass (seed 0) (data 1))
  (define hpblock (curtail (append [0] (rotate data 1))))
  (- data hpblock))

;;; a more reasonable low pass filter
;;; y(n) = y(n - 1) - alpha * (x(n - 1) - x(n))
(define (low-pass (seed 0) (alpha 0) (data 1))
  (define (lp-step (x 0) (acc 2))
    (define lx (head (head acc)))
    (define ly (tail (head acc)))
    (define ny (- ly (* alpha (- lx x))))
    (append [[x ny]] acc))
  (curtail (#r(1)tail (foldr lp-step [(reshape [2] seed)] data))))

;;; a more reasonalbe high pass filter
;;; y(n) = α * (y(n - 1) + x(n) - x(n - 1))
(define (high-pass (seed 0) (alpha 0) (data 1))
  (define (hp-step (x 0) (acc 2))
    (define lx (head (head acc)))
    (define ly (tail (head acc)))
    (define ny (* alpha (- (+ ly x) lx)))
    (append [[x ny]] acc))
  (curtail (#r(1)tail (foldr hp-step [(reshape [2] seed)] data))))