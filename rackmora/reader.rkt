#lang racket
(require "semantics.rkt"
         "syntax.rkt")

; #A(NAT ...)(ATOM ...)
;  reads as
; (alit (NAT ...) ATOM ...)
(define (read-alit trigger-char
                   port
                   source-name
                   line-num
                   col-num
                   position)
  ; take text from the port, looking for (NAT ...)(ATOM ...)
  (define shape (read port))
  (define atoms (read port))
  (cons 'alit
        (cons shape atoms))
  #;(list 'rem-array
        (cons 'vector shape)
        (cons 'vector atoms)))

; [ALITERAL ...]
;  reads as
; (array ALITERAL ...)
(define (read-array trigger-char
                    port
                    source-name
                    line-num
                    col-num
                    position)
  (define-struct end-of-form () #:transparent)
  (define pieces '())
  (parameterize ([current-readtable
                  (make-readtable (current-readtable)
                                  #\]
                                  'terminating-macro
                                  (λ args (end-of-form)))])
    (do ([next (begin #;(displayln "  start loop") (read port))
               (begin #;(displayln "  step") (read port))])
      ((equal? next (end-of-form)) (set! pieces (cons 'array (reverse pieces)))
                                   #;(printf "finishing with \'~v\'" next))
      #;(printf "got element ~v\n" next)
      (set! pieces (cons next pieces)))
    pieces))

; #r(RANK ...)EXP
;  reads as
; (rerank (RANK ...) EXP)
(define (read-rerank trigger-char
                     port
                     source-name
                     line-num
                     col-num
                     position)
  (define new-ranks (read port))
  (define base-exp (read port))
  (list 'rerank new-ranks base-exp))


(define (extend-readtable base-readtable . new-entries)
  (cond [(empty? new-entries) base-readtable]
        [else
         (printf "new readtable entry ~v\n" (first new-entries))
         (apply extend-readtable
               (cons (apply make-readtable
                            (cons base-readtable (first new-entries)))
               (rest new-entries)))]))

(define remora-readtable
  (extend-readtable
   (current-readtable)
   (list #\A 'dispatch-macro read-alit)
   (list #\[ 'terminating-macro read-array)
   (list #\r 'dispatch-macro read-rerank)))
