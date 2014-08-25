#lang rackmora
(require racket/math
         (only-in racket/list first second))

;;; Frequency associated with the row each key appears in
(def row-freqs
  (array (list #\1 697) (list #\2 697) (list #\3 697) (list #\A 697)
         (list #\4 770) (list #\5 770) (list #\6 770) (list #\B 770)
         (list #\7 852) (list #\8 852) (list #\9 852) (list #\C 852)
         (list #\* 941) (list #\0 941) (list #\# 941) (list #\D 941)))

;;; Frequency associated with the column each key appears in
(def col-freqs
  (array (list #\1 1209) (list #\2 1336) (list #\3 1477) (list #\A 1633)
         (list #\4 1209) (list #\5 1336) (list #\6 1477) (list #\B 1633)
         (list #\7 1209) (list #\8 1336) (list #\9 1477) (list #\C 1633)
         (list #\* 1209) (list #\0 1336) (list #\# 1477) (list #\D 1633)))

;;; Sample rate for an audio stream (used to convert analog frequencies to
;;; digital frequencies)
(def audio-sample-rate 8000.)


;;; Generate a sinusoid with given (digital) frequency and phase
(def sinusoid
  (fn ((length 0) (freq 0) (phase 0))
    (unbox count (iota [length])
      (cos (+ (* count freq 2 pi)
              phase)))))

;;; Construct a DTMF tone (two sinusoids) for a given key and number of seconds
;;; If the key is not a valid char (0-9, A-D, *, #), produces DC tone
(def dtmf-encode
  (fn ((key 0) (duration 0))
    (def analog-freqs (lookup+ key [row-freqs col-freqs] 0))
    (def digital-freqs (/ analog-freqs audio-sample-rate))
    (def num-samples (inexact->exact (ceiling (* duration audio-sample-rate))))
    (foldr + 0 (sinusoid num-samples digital-freqs 0))))
;;; Version with less naming of intermediate results
(def dtmf-encode*
  (fn ((key 0) (duration 0))
    (foldr + 0 (sinusoid
                (inexact->exact (ceiling (* duration audio-sample-rate)))
                (/ (lookup+ key [row-freqs col-freqs] 0) audio-sample-rate)
                0))))


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


;;; Identify the greatest number in a vector
(def vec-max
  (fn ((vec 1)) (foldr max (head vec) vec)))


;;; Get the normalized magnitudes of the row and column frequencies in a signal
(def r/c-freq-magnitudes
  (fn ((signal 1))
    (def analog-freqs (#r(1)nub (second [row-freqs col-freqs])))
    (def digital-freqs (/ analog-freqs audio-sample-rate))
    (def components (goertzel digital-freqs signal))
    (def mags (magnitude components))
    (/ mags (vec-max mags))))
;;; Shortened version
(def r/c-freq-magnitudes*
  (fn ((signal 1))
    (def mags (magnitude (goertzel (/ (#r(1)nub (second [row-freqs col-freqs]))
                                      audio-sample-rate)
                                   signal)))
    (/ mags (vec-max mags))))


;;; Select the single frequency which is at least 20 dB above the others. If
;;; there is no such frequency, return 0 (or 'invalid or #f?).
(def select-tone
  (fn ((ref-freqs 1) (normalized 1))
    (def assoc (list normalized ref-freqs))
    (unbox above-threshold (filter (> normalized 0.01) assoc)
      (select (= (length above-threshold) 1)
              (second (head above-threshold))
              0))))


;;; Given a signal, determine the corresponding DTMF key. If there is no such
;;; key, return #\nul.
(def dtmf-decode
  (fn ((signal 1))
    (def freq-components (r/c-freq-magnitudes signal))
    (def detected-tones
      (select-tone [(nub (second row-freqs)) (nub (second col-freqs))]
                   freq-components))
    (def row-tone (head detected-tones))
    (def col-tone (tail detected-tones))
    (def cross-match
      (unbox row-matches (filter (= row-tone (second row-freqs))
                                 (first row-freqs))
        (unbox col-matches (filter (= col-tone (second col-freqs))
                                   (first col-freqs))
          ; We actually want another shape annotation in the body of #r(1 0)list
          ; but there's no convenient way to put it there. The type system would
          ; take care of this problem, but we don't have that in this setting.
          (ravel (#r(1 0)list
                    (append row-matches [#\nul])
                    (append col-matches [#\nul]))))))
    (unbox possibilities (filter ((fn ((pair 0))
                                    (equal? (first pair) (second pair)))
                                  cross-match)
                                 (first cross-match))
      (head (append possibilities [#\nul])))))
