#lang s-exp syntax/module-reader
remora/dynamic/lang/language
#:read remora-read
#:read-syntax remora-read-syntax
(require "semantics.rkt"
         "syntax.rkt"
         racket/list)

(provide remora-readtable
         remora-read
         remora-read-syntax)
(define original-readtable (current-readtable))

;;; #A(NAT ...)(ATOM ...)
;;;  reads as
;;; (alit (NAT ...) ATOM ...)
(define (read-alit trigger-char
                   port
                   source-name
                   line-num
                   col-num
                   position)
  ;; take text from the port, looking for (NAT ...)(ATOM ...)
  (define shape (read port))
  (define atoms (read port))
  (cons 'alit
        (cons shape atoms)))

;;; [ALITERAL ...]
;;;  reads as
;;; (array ALITERAL ...)
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

;;; {FIELD ...}
;;;  reads as
;;; (record-lit FIELD ...)
(define (read-record trigger-char
                     port
                     source-name
                     line-num
                     col-num
                     position)
  (define-struct end-of-form () #:transparent)
  (define pieces '())
  (parameterize ([current-readtable
                  (make-readtable (current-readtable)
                                  #\}
                                  'terminating-macro
                                  (λ args (end-of-form)))])
    (do ([next (begin #;(displayln "  start loop") (read port))
               (begin #;(displayln "  step") (read port))])
      ((equal? next (end-of-form))
       (set! pieces (cons 'record-literal (reverse pieces)))
       #;(printf "finishing with \'~v\'" next))
      #;(printf "got element ~v\n" next)
      (set! pieces (cons next pieces)))
    pieces))

;;; #r(RANK ...)EXP
;;;  reads as
;;; (rerank (RANK ...) EXP)
(define (read-rerank trigger-char
                     port
                     source-name
                     line-num
                     col-num
                     position)
  (define new-ranks (parameterize ([current-readtable original-readtable])
                      (read port)))
  (define base-exp (read port))
  (list 'rerank new-ranks base-exp))

;;; #_(FIELD ...)
;;;  reads as
;;; (view (lens FIELD ...))
(define (read-view trigger-char
                   port
                   source-name
                   line-num
                   col-num
                   position)
  (define field-names (parameterize ([current-readtable original-readtable])
                        (read port)))
  (list 'view (cons 'lens field-names)))

;;; #=(FIELD ...)
;;;  reads as
;;; (set (lens FIELD ...))
(define (read-set trigger-char
                  port
                  source-name
                  line-num
                  col-num
                  position)
  (define field-names (parameterize ([current-readtable original-readtable])
                        (read port)))
  (list 'set (cons 'lens field-names)))

;;; #^(FIELD ...)
;;;  reads as
;;; (over (lens FIELD ...))
(define (read-over trigger-char
                   port
                   source-name
                   line-num
                   col-num
                   position)
  (define field-names (parameterize ([current-readtable original-readtable])
                        (read port)))
  (list 'over (cons 'lens field-names)))

(define (extend-readtable base-readtable . new-entries)
  (cond [(empty? new-entries) base-readtable]
        [else
         (apply extend-readtable
               (cons (apply make-readtable
                            (cons base-readtable (first new-entries)))
               (rest new-entries)))]))

(define remora-readtable
  (extend-readtable
   (current-readtable)
   (list #\A 'dispatch-macro read-alit)
   (list #\[ 'terminating-macro read-array)
   (list #\{ 'terminating-macro read-record)
   (list #\r 'dispatch-macro read-rerank)
   (list #\~ 'non-terminating-macro read-rerank)
   (list #\_ 'dispatch-macro read-view)
   (list #\= 'dispatch-macro read-set)
   (list #\^ 'dispatch-macro read-over)))

(define (remora-read . args)
  (parameterize ([current-readtable remora-readtable])
    (apply read args)))
(define (remora-read-syntax . args)
  (parameterize ([current-readtable remora-readtable])
    (apply read-syntax args)))
