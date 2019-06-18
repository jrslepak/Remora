#lang racket

(require (for-syntax syntax/parse))

(provide (struct-out lrec)
         make-record
         make-lens
         over view set)

(module+ test
  (require rackunit))

;;; An ordered record structure, mapping symbols to values
(define (lrec-print lr port mode)
  (display (lrec->string lr mode) port))
(define (lrec->string lr mode)
  (cond [(number? mode) ; print
         (string-join
          (for/list ([e (lrec-data lr)])
                    (format "(~s ~v)" (car e) (cdr e)))
          " "
          #:before-first "{"
          #:after-last "}")]
        [mode           ; write
         (string-join
          (for/list ([e (lrec-data lr)])
                    (format "(~s ~s)" (car e) (cdr e)))
          " "
          #:before-first "{"
          #:after-last "}")]
        [else           ; display
         (string-join
          (for/list ([e (lrec-data lr)])
                    (format "(~s ~a)" (car e) (cdr e)))
          " "
          #:before-first "{"
          #:after-last "}")]))
(struct lrec (data)
  #:transparent
  #:methods gen:custom-write [(define write-proc lrec-print)])



;;; Build a procedure which constructs an lrec
(define (make-record . fnames)
  (λ fvals (lrec (map cons fnames fvals))))
;;; A couple examples
(define FLAT
  ((make-record 'foo 'bar 'baz 'quux) 10 1-3i #t "wut"))
(define NESTED
  ((make-record 'foo 'bar 'baz 'quux)
   10 1-3i
   ((make-record 'a 'b) "α" "β")
   "wut"))
  
;;; Special syntax for constructing an lrec
(define-syntax (record stx)
  (syntax-parse stx
    [(_ (field:id value:expr) ...)
     #'((make-record 'field ...) value ...)]))

;;; Grab one field from a record
(define (record-ref rec fname)
  (for/first ([field (lrec-data rec)]
              #:when (symbol=? fname (car field)))
             (cdr field)))

;;; Change one field in a record
(define (replace-field rec fname new-val)
  (lrec (for/list ([field (lrec-data rec)])
                  (if (symbol=? fname (car field))
                      (cons fname new-val)
                      field))))


;;; Make something like Functor's fmap available by making the "dictionary
;;; passing" explicit. A value meant to be treated as having an associated
;;; Functor instance must be packaged up along with how it implements fmap.
;;; val : [F s] (for some imagined type constructor F)
;;; fn  : [s -> t] [F s] -> [F t]
(struct functor (val fn) #:transparent)
(define (fmap a b)
  (functor ((functor-fn b) a (functor-val b))
           (functor-fn b)))


;;; Symbol -> [A -> [Functor B]] -> Record -> [Functor Record]
(define (((make-lens fname) xform) rec)
  (fmap (λ (new-val) (replace-field rec fname new-val))
        (xform (record-ref rec fname))))
(define-syntax (lens stx)
  (syntax-parse stx
    [(_ field:id) #'(make-lens 'field)]))


;;; Use a lens to update a field using a passed-in updater function
;;; Lens -> [A -> B] -> [Record over A] -> [Record over B]
(define (((over l) xform) rec)
  (functor-val
   ((l (λ (old-val) (functor (xform old-val)
                             (λ (x y) (x y)))))
    rec)))
(module+ test
  (check-equal?
   (((over (lens foo)) add1) FLAT)
   ((make-record 'foo 'bar 'baz 'quux)
    11 1-3i #t "wut"))
  (check-equal?
   (((over (lens foo)) number->string) FLAT)
   ((make-record 'foo 'bar 'baz 'quux)
    "10" 1-3i #t "wut"))
  (check-equal?
   (((over (lens foo)) add1) NESTED)
   ((make-record 'foo 'bar 'baz 'quux)
    11 1-3i ((make-record 'a 'b) "α" "β") "wut"))
  (check-equal?
   (((over (compose (lens baz) (lens a)))
     (λ (s) (string-append s s)))
    NESTED)
   ((make-record 'foo 'bar 'baz 'quux)
    10 1-3i ((make-record 'a 'b) "αα" "β") "wut")))

;;; Use a lens to extract a field
;;; Lens -> [Record over A] -> A
(define ((view l) rec)
  (functor-val
   ((l (λ (old-val) (functor old-val
                             (λ (x y) y))))
    rec)))
(module+ test
  (check-equal?
   ((view (lens bar))
    NESTED)
   1-3i)
  (check-equal?
   ((view (lens baz))
    NESTED)
   ((make-record 'a 'b) "α" "β"))
  (check-equal?
   ((view (compose (lens baz) (lens a)))
    NESTED)
   "α")
  (check-equal?
   ((view (compose (lens baz) (lens b)))
    NESTED)
   "β"))

;;; Use a lens to replace a field with a passed-in value
;;; Lens -> B -> [Record over A] -> [Record over B]
(define ((set l) new-val)
  ((over l) (const new-val)))
(module+ test
  (check-equal?
   (((set (lens foo)) "eleven") FLAT)
   ((make-record 'foo 'bar 'baz 'quux)
    "eleven" 1-3i #t "wut"))
  (check-equal?
   (((set (lens foo)) 10) FLAT)
   ((make-record 'foo 'bar 'baz 'quux)
    10 1-3i #t "wut"))
  (check-equal?
   (((set (lens foo)) #t) NESTED)
   ((make-record 'foo 'bar 'baz 'quux)
    #t 1-3i ((make-record 'a 'b) "α" "β") "wut"))
  (check-equal?
   (((set (compose (lens baz) (lens a)))
     "αβγδ")
    NESTED)
   ((make-record 'foo 'bar 'baz 'quux)
    10 1-3i ((make-record 'a 'b) "αβγδ" "β") "wut")))
