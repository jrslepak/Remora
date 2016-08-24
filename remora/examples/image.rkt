#lang remora/dynamic

(require "image-loading.rkt")
(require racket/draw)
(require racket/class)

(define mario (remora-load-image "/Users/Sam/Documents/Development/remora/mario.png"))
(define pumpkin (remora-load-image "/Users/Sam/Documents/Development/remora/pumpkin.jpg"))
(define lenna (remora-load-image "/Users/Sam/Documents/Development/remora/lenna.png"))

#|
(define some-image (make-object bitmap% 100 100))

;;; Not sure off-hand how DrRacket handles relative paths or ~
(send some-image load-file "/Users/jrslepak/Pictures/quincy.jpg" 'jpeg)

(define width (send some-image get-width))
(define height (send some-image get-height))
(define pixel-count (* width height))

(define pixels (make-bytes (* 4 pixel-count)))

(send some-image get-argb-pixels 0 0 width height pixels)

(define pixel-vec
  (for/vector #:length pixel-count ([i pixel-count])
    (for/vector #:length 4 ([channel 4])
      (bytes-ref pixels (+ channel (* i 4))))))
|#

(define (sum (vec all))
  (foldr + 0 vec))

(define (dup (n 0) (x 1))
  (reshape [n] x))

(define (average (m all))
  (/ (sum m) (length m)))

#|
(define (remora-load-image (path 1))
  (define bmp (make-object bitmap% 100 100))
  (send bmp load-file path)
  (define width (send bmp get-width))
  (define height (send bmp get-height))
  (define pixel-count (* width height))
  (define pixels (make-bytes (* 4 pixel-count)))
  (send bmp get-argb-pixels 0 0 width height pixels)
  (define pixel-vec
    (for/vector #:length pixel-count ([i pixel-count])
      (for/vector #:length 4 ([channel 4])
        (bytes-ref pixels (+ channel (* i 4))))))
  (list->array pixel-vec))
|#

;;; consumes an image represented as a matrix of rgba vectors and applies a simple blur
(define (blur (image 3) (frame-size 0))
  (define rotation-amount (- (iota [frame-size]) (floor (/ frame-size 2))))
  (define first-rotation (rotate image rotation-amount))
  (define windows ((λ ((x 0)) (#r(2 all)rotate first-rotation x)) rotation-amount))
  (round (average (average windows))))

(define (apply-blur-kernel (frame 5) (kernel 2))
  (sum (sum (* frame kernel))))

(define 3x3-gaussian-kernel
  (* 1/16
     [[1 2 1]
      [2 4 2]
      [1 2 1]]))

(define 7x7-gaussian-kernel
  [[0.00000067 0.00002292 0.00019117 0.00038771 0.00019117 0.00002292 0.00000067]
   [0.00002292 0.00078634 0.00655965 0.01330373 0.00655965 0.00078633 0.00002292]
   [0.00019117 0.00655965 0.05472157 0.11098164 0.05472157 0.00655965 0.00019117]
   [0.00038771 0.01330373 0.11098164 0.22508352 0.11098164 0.01330373 0.00038771]
   [0.00019117 0.00655965 0.05472157 0.11098164 0.05472157 0.00655965 0.00019117]
   [0.00002292 0.00078633 0.00655965 0.01330373 0.00655965 0.00078633 0.00002292]
   [0.00000067 0.00002292 0.00019117 0.00038771 0.00019117 0.00002292 0.00000067]])


(define (gaussian (image 3) (blur-kernel 2))
  (define frame-size (head (shape-of blur-kernel)))
  (define rotation-amount (- (iota [frame-size]) (floor (/ frame-size 2))))
  (define first-rotation (rotate image rotation-amount))
  (define windows ((λ ((x 0)) (#r(2 all)rotate first-rotation x)) rotation-amount))
  (inexact->exact (round (apply-blur-kernel windows blur-kernel))))
  

;;; consumes an image represented as a vector of rgba vectors and applies a greyscale effect
(define (greyscale (pixel 1))
  (append [(head pixel)] (reshape [3] (round (average (behead pixel))))))

(define (expand-window (shape 1) (base 2))
  (reshape shape (#r(1 1)reshape [(tail shape)] base)))

(define dither-matrix-4x4
  (* 1/17 [[1 9 3 11]
          [13 5 15 7]
          [4 12 2 10]
          [16 8 14 6]]))

(define dither-matrix-8x8
  (* 1/65
     [[1 49 13 61 4 52 16 64]
      [33 17 45 29 36 20 48 32]
      [9 57 5 53 12 60 8 56]
      [41 25 37 21 44 28 40 24]
      [3 51 15 63 2 50 14 62]
      [35 19 47 31 34 18 46 30]
      [11 59 7 55 10 58 6 54]
      [43 27 39 23 42 26 38 22]]))

(define (dither (image 3) (dmatrix 2))
  (define window-expanded-dither-matrix (expand-window (curtail (shape-of image)) dmatrix))
  (define repeated-dither (#r(1 0)reshape [3] window-expanded-dither-matrix))
  (define alpha (#r(1)head image))
  (define rgb (#r(1)behead image))
  (define modified-rgb (+ rgb (* rgb repeated-dither)))
  (min 255 (max 0 (round (#r(1 1)append (#r(0)itemize alpha) modified-rgb)))))

(define (save-image (path 0) (encoding 0) (image 3))
  (define width (head (shape-of image)))
  (define height (head (behead (shape-of image))))
  (define flat (array->flat-vector image))
  (remora-save-image path encoding width height flat))

(define (show-image (image 3))
  (define width (head (shape-of image)))
  (define height (head (behead (shape-of image))))
  (define flat (array->flat-vector image))
  (remora-display-image width height flat))

;; first (#r(1 1)reshape [width] frame)
;; then, reshape that to the full size

(define (cons (car 0) (cdr 1)) (append (itemize car) cdr))
(define test-square
  (#r(0 1)cons 255 (#r(1 0)reshape [3] (round (/ (* (iota [6 6]) 255) 36)))))

;; gaussian blur function which takes a kernel