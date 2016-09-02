#lang racket

(require racket/draw)
(require racket/class)
(require remora/dynamic)

(provide (all-defined-out))

;;; Initial attempt at image loading and saving for remora.
;;; Look into making this nicer by initializing bitmaps in a better
;;; way. (is there a way to load them with the size of the image?)

(define (remora-load-image path)
  ;; initialize a bitmap of any size
  (define bmp (make-object bitmap% 100 100))
  (send bmp load-file path)
  (define width (send bmp get-width))
  (define height (send bmp get-height))
  (define pixel-count (* width height))
  (define pixels (make-bytes (* 4 pixel-count)))
  (send bmp get-argb-pixels 0 0 width height pixels)
  (define pixel-veclist
    (for/list ([i pixel-count])
      (for/vector #:length 4 ([channel 4])
        (bytes-ref pixels (+ channel (* i 4))))))
  (define channel-vec (apply vector-append pixel-veclist))
  (rem-array (list->vector (list width height 4))
             channel-vec))

(define (remora-save-image path encoding width height image)
  ;; initialize a bitmap to the size of the image
  (define bmp (make-object bitmap% width height))
  (send bmp set-argb-pixels 0 0 width height (list->bytes (vector->list image)))
  (send bmp save-file path encoding))

(define (remora-display-image width height image)
  (define bmp (make-object bitmap% width height))
  (send bmp set-argb-pixels 0 0 width height (list->bytes (vector->list image)))
  (print bmp))
