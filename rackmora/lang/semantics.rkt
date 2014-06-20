#lang racket/base

; TODO: change how write/print/display work for rem-array

(require racket/list
         racket/vector
         racket/sequence
         racket/contract/base)
(module+ test
  (require rackunit))
(define debug-mode (make-parameter #f))
(provide debug-mode)

;;;-------------------------------------
;;; Internal use structures:
;;;-------------------------------------

;; Apply a Remora array (in Remora, an array may appear in function position)
(provide
 (contract-out
  (apply-rem-array (->* (rem-array?)
                        (#:result-shape
                         (or/c symbol?
                               (vectorof exact-nonnegative-integer?)))
                        #:rest
                        (listof rem-array?)
                        rem-array?))))
(define (apply-rem-array fun
                         #:result-shape [result-shape 'no-annotation]
                         . args)
  ; check whether the data portion of fun is Remora procedures
  (unless (for/and [(p (rem-array-data fun))] (rem-proc? p))
    (error "Array in function position must contain only Remora functions" fun))
  
  (when (debug-mode) (printf "\n\nResult shape is ~v\n" result-shape))
  
  ; check whether args actually are Remora arrays
  (unless (for/and [(arr args)] (rem-array? arr))
    (error "Remora arrays can only by applied to Remora arrays" fun args))
  (when (debug-mode) (printf "checked for Remora array arguments\n"))
  
  ; identify expected argument cell ranks
  (define individual-exp-ranks
    (for/list [(p (rem-array-data fun))]
      (when (debug-mode) (printf "checking expected ranks for ~v\n" p))
      (for/vector [(t (rem-proc-ranks p))
                   (arr args)]
        (when (debug-mode) (printf "~v - ~v\n" p t))
        (if (equal? 'all t)
            (rem-array-rank arr)
            t))))
  (when (debug-mode) (printf "individual expected ranks are ~v\n"
                             individual-exp-ranks))
  (define expected-rank
    (cond [(empty? individual-exp-ranks) 
           'empty-function-array]
          [(for/and [(p individual-exp-ranks)]
             (equal? p (first individual-exp-ranks)))
           (first individual-exp-ranks)]
          [else (error "Could not identify expected rank for function" fun)]))
  (when (debug-mode) (printf "expected-rank = ~v\n" expected-rank))
  
  ; find principal frame shape
  (define principal-frame
    (or (for/fold ([max-frame (rem-array-shape fun)])
          ([arr args]
           [r expected-rank])
          (prefix-max 
           (vector-drop-right (rem-array-shape arr) r)
           max-frame))
        (error "Incompatible argument frames"
               (cons (rem-array-shape fun)
                     (for/list ([arr args]
                                [r expected-rank])
                       (vector-drop-right (rem-array-shape arr) r))))))
  (when (debug-mode) (printf "principal-frame = ~v\n" principal-frame))
  
  ; compute argument cell sizes
  (define cell-sizes
    (for/list ([arr args]
               [r expected-rank])
      (sequence-fold * 1 (vector-take-right (rem-array-shape arr) r))))
  (when (debug-mode) (printf "cell-sizes = ~v\n" cell-sizes))
  
  ; compute argument frame sizes
  (define frame-sizes
    (for/list ([arr args]
               [r expected-rank])
      (sequence-fold * 1 (vector-drop-right (rem-array-shape arr) r))))
  (when (debug-mode) (printf "frame-sizes = ~v\n" frame-sizes))
  
  ; compute each result cell
  (define result-cells
    (for/vector ([cell-id (sequence-fold * 1 principal-frame)])
      (when (debug-mode)
        (printf
         "using function cell ~v\n taken from ~v :: ~v\n"
         (quotient cell-id
                   (quotient (sequence-fold * 1 principal-frame)
                             (sequence-fold * 1 (rem-array-shape fun))))
         (rem-array-data fun)
         (quotient cell-id
                   (quotient (sequence-fold * 1 principal-frame)
                             (sequence-fold * 1 (rem-array-shape fun))))))
      (apply
       (vector-ref
        (rem-array-data fun)
        (quotient cell-id
                  (quotient (sequence-fold * 1 principal-frame)
                            (sequence-fold * 1 (rem-array-shape fun)))))
       (for/list ([arr args]
                  [csize cell-sizes]
                  [fsize frame-sizes]
                  [r expected-rank])
         (when (debug-mode)
           (printf "  arg cell #~v, csize ~v, pfr ~v, fr ~v"
                   cell-id csize (sequence-fold * 1 principal-frame) fsize)
           (printf
            " -- ~v\n"
            (rem-array
             (vector-take-right (rem-array-shape arr) r)
             (subvector (rem-array-data arr)
                        (* csize
                           (quotient
                            cell-id
                            (quotient (sequence-fold * 1 principal-frame)
                                      fsize)))
                        csize))))
         (rem-array
          (vector-take-right (rem-array-shape arr) r)
          (subvector (rem-array-data arr)
                     (* csize
                        (quotient cell-id
                                  (quotient (sequence-fold * 1 principal-frame)
                                            fsize)))
                     csize))))))
  (when (debug-mode) (printf "result-cells = ~v\n" result-cells))
  
  (when (debug-mode)
    (printf "# of result cells: ~v\nresult-shape = ~v\n"
            (vector-length result-cells) result-shape))
  ; determine final result shape
  (define final-shape
    (cond
      ; empty frame and no shape annotation -> error
      [(and (equal? result-shape 'no-annotation)
            (equal? 0 (vector-length result-cells)))
       (error "Empty frame with no shape annotation: ~v applied to ~v"
              fun args)]
      ; empty frame -> use annotated shape
      ; TODO: should maybe check for mismatch between annotated and actual
      ;       (i.e. frame-shape ++ cell-shape) result shapes
      [(equal? 0 (vector-length result-cells)) result-shape]
      [(for/and ([c result-cells])
         (equal? (rem-value-shape (vector-ref result-cells 0))
                 (rem-value-shape c)))
       (vector-append principal-frame
                      (rem-value-shape (vector-ref result-cells 0)))]
      [else (error "Result cells have mismatched shapes: ~v" result-cells)]))
  (when (debug-mode) (printf "final-shape = ~v\n" final-shape))
  
  ; determine final result data: all result cells' data vectors concatenated
  (define final-data
    (if (and (> (vector-length result-cells) 0)
             (rem-box? (vector-ref result-cells 0)))
        (for/vector ([r result-cells]) r)
        (apply vector-append
               (for/list ([r result-cells])
                 (rem-value-data r)))))
  (when (debug-mode) (printf "final-data = ~v\n\n\n" final-data))
  
  (if (and (equal? #() final-shape)
           (rem-box? (vector-ref final-data 0)))
      (vector-ref final-data 0)
      (rem-array final-shape final-data)))

;; Contract constructor for vectors of specified length
(define ((vector-length/c elts len) vec)
  (and ((vectorof elts #:flat? #t) vec)
       (equal? (vector-length vec) len)))

;; String representation of an array, for print, write, or display mode
(define (array->string arr [mode 0])
  (define format-string
    (cond [(member mode '(0 1)) "~v"] ; print
          [(equal? mode #t) "~s"]     ; write
          [(equal? mode #f) "~a"]))   ; display
  (if (equal? mode #t)
      ; write mode
      (format "(rem-array ~s ~s)" (rem-array-shape arr) (rem-array-data arr))
      ; print/display mode
      (cond [(= 0 (rem-array-rank arr))
             (format format-string (vector-ref (rem-array-data arr) 0))]
            [else (string-append
                   (for/fold ([str "["])
                     ([cell (-1-cells arr)]
                      [cell-id (length (-1-cells arr))])
                     (string-append str
                                    (if (equal? 0 cell-id) "" " ")
                                    (array->string cell mode)))
                   "]")])))
;; Print, write, or display an array
(define (show-array arr [port (current-output-port)] [mode 0])
  (display (array->string arr mode) port))

;; A Remora array has
;; - shape, a vector of numbers
;; - data, a vector of any
(provide
 (contract-out
  #;(struct rem-array
      ([shape (vectorof exact-nonnegative-integer? #:immutable #t)]
       [data (vectorof any #:immutable #t)])
      #:omit-constructor)
  (rem-array (->i ([shape (vectorof exact-nonnegative-integer?)]
                   [data (shape) (vector-length/c
                                  any/c
                                  (for/product ([dim shape]) dim))])
                  [result any/c]))
  (rem-array-shape (-> rem-array?
                       (vectorof exact-nonnegative-integer?)))
  (rem-array-data  (-> rem-array?
                       (vectorof any/c)))
  (rem-array? (-> any/c boolean?))))
(struct rem-array (shape data)
  #:transparent
  #:property prop:procedure apply-rem-array
  #:methods gen:custom-write [(define write-proc show-array)])
(module+ test
  (define array-ex:scalar1 (rem-array #() #(4)))
  (define array-ex:scalar2 (rem-array #() #(2)))
  (define array-ex:vector1 (rem-array #(2) #(10 20)))
  (define array-ex:matrix1 (rem-array #(2 3) #(1 2 3 4 5 6))))

;; Find the rank of a Remora array
(provide
 (contract-out
  (rem-array-rank (-> rem-array? exact-nonnegative-integer?))))
(define (rem-array-rank arr) (vector-length (rem-array-shape arr)))
(module+ test
  (check-equal? 0 (rem-array-rank array-ex:scalar1))
  (check-equal? 1 (rem-array-rank array-ex:vector1))
  (check-equal? 2 (rem-array-rank array-ex:matrix1)))

;; Convert a Remora vector (rank 1 Remora array) to a Racket vector
(provide
 (contract-out (rem-array->vector
                (-> (λ (arr) (and (rem-array? arr)
                                  (equal? (rem-array-rank arr) 1)))
                    vector?))))
(define (rem-array->vector arr)
  (if (equal? (rem-array-rank arr) 1)
      (rem-array-data arr)
      (error rem-array->vector "provided array does not have rank 1")))


;; Apply a Remora procedure (for internal convenience)
;; TODO: consider eliminating this (see note in rem-proc struct defn)
(define (apply-rem-proc fun . args)
  (apply (rem-proc-body fun) args))

;; A valid expected rank is either a natural number or 'all
(define (rank? r)
  (or (exact-nonnegative-integer? r) (equal? 'all r)))

;; Print, write, or display a Remora procedure
(define (show-rem-proc proc [port (current-output-port)] [mode 0])
  (display "#<rem-proc>" port))

;; A Remora procedure has
;; - body, a Racket procedure which consumes and produces Remora arrays
;; - ranks, a list of the procedure's expected argument ranks
(provide
 (contract-out (struct rem-proc ([body procedure?]
                                 [ranks (listof rank?)]))))
(define-struct rem-proc (body ranks)
  #:transparent
  ; may decide to drop this part -- it seems to hide a common error:
  ;   using (R+ arr1 arr2) instead of ([scalar R+] arr1 arr2) means no lifting
  #:property prop:procedure apply-rem-proc
  #:methods gen:custom-write [(define write-proc show-rem-proc)])
(module+ test
  (define R+ (rem-scalar-proc + 2))
  (define R- (rem-scalar-proc - 2))
  (define R* (rem-scalar-proc * 2))
  (check-equal? (R+ array-ex:scalar1 array-ex:scalar2)
                (rem-array #() #(6))))


;; Construct an array as a vector of -1-cells
(provide
 (contract-out (build-vec (->* ((or/c rem-array? rem-box?))
                               #:rest (listof (or/c rem-array? rem-box?))
                               rem-array?))))
(define (build-vec arr . arrs)
  (define (only-unique-element xs)
    #;
    (when (equal? 0 (sequence-length xs))
           (error "looking for unique element in empty sequence"))
    (for/fold ([elt (sequence-ref xs 0)])
      ([x xs])
      (if (equal? x elt)
          x
          (error "cannot use vec on arrays of mismatched shape"))))
  (define cell-shape
    (only-unique-element
     (for/list ([a (cons arr arrs)])
       (cond [(rem-array? a) (rem-array-shape a)]
             [(rem-box? a) 'box]))))
  (define num-cells (length (cons arr arrs)))
  (if (equal? cell-shape 'box)
      (rem-array (vector num-cells) (list->vector (cons arr arrs)))
      (rem-array
       (for/vector ([dim (cons num-cells (vector->list cell-shape))]) dim)
       (apply vector-append (for/list ([a (cons arr arrs)])
                              (rem-array-data a))))))



;; A Remora box (dependent sum) has
;; - contents, a Remora value
;; - indices, a list of the witness indices
(provide (contract-out
          (struct rem-box ([contents rem-array?]))))
(define-struct rem-box (contents)
  #:transparent)

;;; More permissive variants of rem-array functions
;; Find the shape of a Remora value (array or box)
;; Boxes are considered to have scalar shape
(define (rem-value-shape v)
  (cond [(rem-array? v) (rem-array-shape v)]
        [(rem-box? v) #()]))
;; Find the rank of a Remora value (array or box)
;; Boxes are considered to have scalar rank
(define (rem-value-rank v)
  (cond [(rem-array? v) (rem-array-rank v)]
        [(rem-box? v) 0]))
;; Find the data contents of a Remora value (array or box)
;; Boxes are considered to be their own contents
(define (rem-value-data v)
  (cond [(rem-array? v) (rem-array-data v)]
        [(rem-box? v) v]))


;; Identify which of two sequences is the prefix of the other, or return #f
;; if neither is a prefix of the other (or if either sequence is #f)
(define (prefix-max seq1 seq2)
  (and seq1 seq2
       (for/and ([a seq1] [b seq2])
         (equal? a b))
       (if (> (sequence-length seq1) (sequence-length seq2)) seq1 seq2)))
(module+ test
  (check-equal? (prefix-max #(3 4) #(3 4)) #(3 4))
  (check-equal? (prefix-max #(3 4) #(3 4 9)) #(3 4 9))
  (check-equal? (prefix-max #(3 4 2) #(3 4)) #(3 4 2))
  (check-equal? (prefix-max #(3) #(3 4)) #(3 4))
  (check-equal? (prefix-max #(3 2) #(3 4)) #f)
  (check-equal? (prefix-max #(3 2) #(3 4 5)) #f))


;; Extract a contiguous piece of a vector
(provide
 (contract-out (subvector (-> (vectorof any/c)
                              exact-nonnegative-integer?
                              exact-nonnegative-integer?
                              (vectorof any/c)))))
(define (subvector vec offset size)
  (vector->immutable-vector (vector-take (vector-drop vec offset) size)))
(module+ test
  (check-equal? (subvector #(2 4 6 3 5 7) 1 3) #(4 6 3))
  (check-equal? (subvector #(2 4 6 3 5 7) 4 2) #(5 7))
  (check-equal? (subvector #(2 4 6 3 5 7) 4 0) #()))


;; Convert a rank-1 or higher array to a list of its -1-cells
(define (-1-cells arr)
  (define cell-shape (vector-drop (rem-array-shape arr) 1))
  (define cell-size (for/product ([dim cell-shape]) dim))
  (define num-cells (vector-ref (rem-array-shape arr) 0))
  (for/list ([cell-id num-cells])
    (rem-array cell-shape
               (subvector (rem-array-data arr)
                          (* cell-id cell-size)
                          cell-size))))



;; tests for array application
;; TODO: test array application for functions that consume/produce non-scalars
(module+ test
  (check-equal? ((scalar R+) (scalar 3) (scalar 4))
                (scalar 7))
  (check-equal? ((scalar R+) (rem-array #(2 3) #(1 2 3 4 5 6))
                             (rem-array #(2) #(10 20)))
                (rem-array #(2 3) #(11 12 13 24 25 26)))
  (check-equal? ((rem-array #(2) (vector R+ R-))
                 (rem-array #(2 3) #(1 2 3 4 5 6))
                 (rem-array #(2) #(10 20)))
                (rem-array #(2 3) #(11 12 13 -16 -15 -14))))

;;;-------------------------------------
;;; Integration utilities
;;;-------------------------------------
;; Build a scalar Remora procedure from a Racket procedure
(provide
 (contract-out
  (rem-scalar-proc (-> procedure? exact-nonnegative-integer? rem-proc?))))
(define (rem-scalar-proc p arity)
  (rem-proc (λ args
              (rem-array
               #()
               (vector-immutable
                (apply p (for/list [(a args)]
                           (vector-ref (rem-array-data a) 0))))))
            (for/list [(i arity)] 0)))

;; Build a scalar Remora array from a Racket value
(define (scalar v) (rem-array #() (vector-immutable v)))

;; Apply what may be a Remora array or a Racket procedure to some Remora arrays,
;; with a possible result shape annotation
(provide (contract-out
          (remora-apply (->* (procedure?)
                             (#:result-shape
                              (or/c symbol?
                                    (vectorof exact-nonnegative-integer?)))
                             #:rest
                             (listof rem-array?)
                             rem-array?))))
(define (remora-apply
         fun
         #:result-shape [result-shape 'no-annotation]
         . args)
  (cond [(rem-array? fun)
         (apply apply-rem-array
                (racket-proc-array->rem-proc-array fun (length args))
                #:result-shape result-shape
                args)]
        [(rem-proc? fun)
         (apply apply-rem-array
                (rem-array #() (vector fun))
                #:result-shape result-shape
                args)]
        [(procedure? fun)
         (apply apply-rem-array
                (rem-array #()
                           (vector (rem-scalar-proc fun (length args))))
                #:result-shape result-shape
                args)]))

;; if given array contains Racket procedures, convert them to Remora procedures
;; of the given arity
(define (racket-proc-array->rem-proc-array arr arity)
  (rem-array (rem-array-shape arr)
             (for/vector ([elt (rem-array-data arr)])
               (cond [(rem-proc? elt) elt]
                     [(procedure? elt) (rem-scalar-proc elt arity)]))))

(provide (contract-out (racket->remora (-> any/c rem-array?))))
(define (racket->remora val)
  (cond [(rem-array? val) val]
        [else (rem-array #() (vector val))]))
