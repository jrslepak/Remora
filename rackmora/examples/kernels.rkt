#lang rackmora
(require racket/math)
;;; TODO: figure out why all-defined-out doesn't export def'd things but
;;; they can still be exported by explicitly naming them
#;(provide (except-out all-defined-out mean))

;;; Mean
;;; XCR shivers: tally -> length
;;; jrslepak: renamed operator
(def mean
  (fn ((samples 1)) (/ (foldr + 0 samples)
                       (length samples))))


;;; XCR shivers: Indent lambda body (and other "bodies", e.g. let) 2 cols.
;;;   Invest in remora emacs package with customised indenting.
;;;   I'd say unbox is also a fn/let indent case.
;;; XCR shivers: Use classic semi convention, not bogus Racket ones:
;;;   3 top level. 2 indented like code. 1 at end of line.
;;; jrslepak: both should be fixed in
;;;   commit d6a370db72c775d7ed4877b0233d9d08f553ee42
;;; Variance
(def variance
  (fn ((samples 1))
    (mean (sqr (- samples (mean samples))))))


;;; Covariance
(def covariance
  (fn ((xs 1) (ys 1))
    (mean (* (- xs (mean xs))
             (- ys (mean ys))))))


;;; Autocovariance -- covariance of a signal and a delayed version of itself
;;; For this example, we pad out the end of the delayed signal with samples
;;; from the beginning of the signal
(def autocovariance
  (fn ((samples 1) (delay 0))
    (covariance samples (rotate samples delay))))


;;; Pearson correlation
(def correlation
  (fn ((xs 1) (ys 1))
    (/ (covariance xs ys)
       (* (sqrt (variance xs)) (sqrt (variance ys))))))


;;; Autocorrelation
(def autocorrelation
  (fn ((signal 1) (delay 0))
    (correlation signal (rotate signal delay))))


;;; Convolution


;;; XCR shivers: Vas ist das R*?
;;; jrslepak: I was trying out the integration of variable-arity functions, so I
;;;   pulled in Racket's * procedure under that name. At this point, Remora does
;;;   not export its own *, so I've removed it. In the future, if we cut out
;;;   support for importing Racket procedures with indeterminate arity, some
;;;   of these multiplication exprs will have to be fixed.
;;; Generate a sinusoid with given (digital) frequency and phase
(def sinusoid
  (fn ((length 0) (freq 0) (phase 0))
    (unbox count (iota [length])
      (cos (+ (* count freq 2 pi)
              phase)))))


;;; Goertzel algorithm (extract single frequency component)
;;; first stage is IIR, second is FIR
;;; Scan is used for demonstration purposes, but for selecting just the single
;;; DFT result, it may be better to use foldl, as only the final value of the
;;; accumulator is actually needed.
(def goertzel-iir-step
  (fn ((freq 0))
    (fn ((next 0) (accum 1))
      (array (- (+ next (* 2 (cos (* 2 pi freq)) (head accum)))
                (tail accum))
             (head accum)))))
(def goertzel-iir
  (fn ((freq 0) (signal 1))
    (#r(1)head (scan (goertzel-iir-step freq) (array 0 0) signal))))
(def goertzel-fir-step
  (fn ((freq 0) (win 1)) ; length-2 window of post-IIR signal
    (- (tail win)
       (* (head win) (exp (- 0 (* 2 (* pi (* 0+i freq)))))))))
(def goertzel-fir
  (fn ((freq 0) (post-iir 1))
    (goertzel-fir-step freq (unsafe-unbox (take-right 2 post-iir)))))
(def goertzel
  (fn ((freq 0) (signal 1))
    ;; magnitude must be scaled down by half the buffer length
    ;; result phase is how far from 0 (i.e., 2π) the buffer's last sample is
    (/ (goertzel-fir freq (goertzel-iir freq signal))
       (/ (length signal) 2))))


