#lang remora/dynamic

(require racket/math)
(require "idioms.rkt")
(require "kernels.rkt")

;;; a simple yet naieve low pass filter
;;; y(n) = x(n) + x(n - 1)
(define (simple-low-pass (seed 0) (data 1))
  (define lpblock (append (curtail (rotate data 1)) [0]))
  (+ data lpblock))

;;; a simple yet naieve high pass filter
;;; y(n) = x(n) - x(n - 1)
(define (simple-high-pass (seed 0) (data 1))
  (define hpblock (append (curtail (rotate data 1)) [0]))
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

;;; general FIR filter
;;; y(n) = sum k=0 -> M - 1 of h(k) * x(n - k)

(define (dot (a all) (b all))
  (foldr + 0 (* a b)))

;;; sample FIR coefficients based on http://t-filter.engineerjs.com/
(define low-pass-kernel
  [-0.02010411882885732
   -0.05842798004352509
   -0.061178403647821976
   -0.010939393385338943
   0.05125096443534972
   0.033220867678947885
   -0.05655276971833928
   -0.08565500737264514
   0.0633795996605449
   0.310854403656636
   0.4344309124179415
   0.310854403656636
   0.0633795996605449
   -0.08565500737264514
   -0.05655276971833928
   0.033220867678947885
   0.05125096443534972
   -0.010939393385338943
   -0.061178403647821976
   -0.05842798004352509
   -0.02010411882885734])

(define high-pass-kernel
  [0.02857983994169657
   -0.07328836181028245
   0.04512928732568175
   0.03422632401030237
   -0.034724262386629436
   -0.05343090761376418
   0.032914528649623416
   0.09880818246272206
   -0.034135422078843417
   -0.3160339484471911
   0.5341936566511765
   -0.3160339484471911
   -0.034135422078843417
   0.09880818246272206
   0.03291452864962342
   -0.0534309076137642
   -0.034724262386629436
   0.03422632401030237
   0.04512928732568176
   -0.07328836181028245
   0.02857983994169657])

(define band-pass-kernel
  [0.008315515510919604
   0.013703008819203135
   -0.008125257257844711
   -0.01649214060817737
   -0.0016884593471476288
   -0.006913035271285468
   -0.03139161346522045
   0.022740863526439097
   0.11984908724116743
   0.05186355035523461
   -0.17137740316854042
   -0.20124347467075893
   0.08441813048666601
   0.2816314309336389
   0.08441813048666601
   -0.20124347467075893
   -0.17137740316854042
   0.05186355035523461
   0.1198490872411674
   0.022740863526439097
   -0.03139161346522045
   -0.006913035271285468
   -0.0016884593471476288
   -0.01649214060817737
   -0.008125257257844711
   0.013703008819203135
   0.008315515510919604])

(define band-stop-kernel
  [0.037391727827352596
   -0.03299884552335981
   0.04423058396732136
   0.0023050970833628126
   -0.06768087195950102
   -0.04634710540912466
   -0.011717387509232449
   -0.07073422841851829
   -0.04976651728299956
   0.16086413543836361
   0.21561058688743148
   -0.10159456907827959
   0.6638637561392535
   -0.10159456907827963
   0.21561058688743148
   0.16086413543836361
   -0.049766517282999544
   -0.07073422841851829
   -0.011717387509232449
   -0.0463471054091247
   -0.06768087195950102
   0.0023050970833628126
   0.04423058396732135
   -0.0329988455233598
   0.037391727827352596])

;;; general FIR filter
(define (fir-filter (coeffs 1) (data 1))
  (dot coeffs (rotate data (iota [(length coeffs)]))))

;;; DFT
(define (dft-angles (len 0))
  (define transforms (position-matrix len))
  (/ (* 2 pi (#r(1)head transforms) (#r(1)tail transforms)) len))

;;; FFT, butterfly split, then recur