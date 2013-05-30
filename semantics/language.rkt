#lang racket

(require rackunit
         redex)
(provide Arrays
         take/m drop/m take-right/m drop-right/m prefix?
         shape)

(define-language Arrays
  (expr (expr expr ...)
        arr
        var)
  (el-expr expr elt)
  
  ; array elements: base data and functions
  (elt base fun)
  
  ; base data types: numbers and booleans
  (base num bool)
  (num number)
  (bool #t #f)
  
  ; functions may be builtin or user-defined
  (fun (λ [(var num) ...] expr)
       op)
  ; builtin operators
  (op + - * /
      < > <= >= =
      and or not
      exp log
      append itemize
      shape-of reshape transpose
      nub-sieve
      head behead tail curtail
      iota
      reduce
      fold/r
      fold/l)
  
  ; syntax for writing arrays
  (arr (A (num ...) (el-expr ...)) ; flat representation, shape and values
       (box expr))
  (arr/pv (A (num ...) (elt ...))) ; pseudo-value form -- no app forms inside
  (pseudo-elt arr/pv arr/v base)
  ; value form -- only base data or functions inside
  (arr/v arr/b arr/f arr/box box-val)
  (arr/b (A (num ...) (base ...)))
  (arr/f (A (num ...) (fun ...)))
  ; have to exclude scalar array containing box
  ; (it should be reduced to just the box)
  (arr/box (A (num num ...) (box-val ...)) ; nonscalar
           (A (num ...) (box-val box-val box-val ...))) ; multiple boxes
  (box-val (box arr/v))
  
  ; variables
  (var variable-not-otherwise-mentioned)
  
  ; evaluation contexts
  (E hole
     (E expr ...)
     (arr/v ... E expr ...)
     (A (num ...) (arr/v ... E el-expr ...))
     (box E)))


(define ->Array
  (reduction-relation
   Arrays
   #:domain expr
   [--> (in-hole E ((A () (reduce)) arr/f arr/v_base arr/v))
        (in-hole E (tree-apply arr/f arr/v_base (arr/v_cell ...)))
        (where (arr/v_cell ...) (cells/rank -1 arr/v))
        reduce]
   [--> (in-hole E ((A () (fold/r)) arr/f arr/v_0 arr/v_1))
        (in-hole E (chain-apply/r arr/f (arr/v_cell ... arr/v_0)))
        (where (arr/v_cell ...) (cells/rank -1 arr/v_1))
        fold/r]
   [--> (in-hole E ((A () (fold/l)) arr/f arr/v_0 arr/v_1))
        (in-hole E (chain-apply/l arr/f (arr/v_0 arr/v_cell ...)))
        (where (arr/v_cell ...) (cells/rank -1 arr/v_1))
        fold/l]
   [--> (in-hole E ((A () (append)) arr/v_0 arr/v_1))
        (in-hole E (op/append arr/v_0 arr/v_1))
        append]
   [--> (in-hole E ((A () (itemize)) arr/v))
        (in-hole E (op/itemize arr/v))
        itemize]
   [--> (in-hole E ((A () (head)) arr/v))
        (in-hole E (op/head arr/v))
        head]
   [--> (in-hole E ((A () (behead)) arr/v))
        (in-hole E (op/behead arr/v))
        behead]
   [--> (in-hole E ((A () (tail)) arr/v))
        (in-hole E (op/tail arr/v))
        tail]
   [--> (in-hole E ((A () (curtail)) arr/v))
        (in-hole E (op/curtail arr/v))
        curtail]
   [--> (in-hole E ((A () (shape-of)) (A (num_dim ...) (elt ...))))
        (in-hole E (A (num_rank) (num_dim ...)))
        (where num_rank (length/m (num_dim ...)))
        shape-of]
   [--> (in-hole E ((A () (reshape)) arr/v_0 arr/v_1))
        (in-hole E (op/reshape arr/v_0 arr/v_1))
        (side-condition (= 1 (term (rank arr/v_0))))
        reshape]
   [--> (in-hole E ((A () (op)) arr/v ...))
        (in-hole E (apply-op op (arr/v ...)))
        (side-condition (equal? (term (fun-rank op))
                                (term  ((rank arr/v) ...))))
        op]
   [--> (in-hole E ((A () ((λ [(var natural) ...] expr))) arr/v ...))
        (in-hole E (subst [(var arr/v) ...] expr))
        
        (side-condition (term (all ((at-rank? natural arr/v) ...))))
        apply]
   [--> (in-hole E ((A [num_fundim] []) arr/v ...))
        (in-hole E (A [num_fundim] []))
        ; handling empty array of functions (may have some issues typing this)
        arr/f-empty]
   [--> (in-hole E (arr/f arr/v ...))
        (in-hole E
                 (A (shape arr/f) (((scalar fun) arr_cell ...) ...)))
        ; require a nonempty array of functions
        (where (A (num_fundim ...) (fun_0 fun_1 ...)) arr/f)
        ; all functions in array must have same expected ranks
        (side-condition (term (all-equal? ((fun-rank fun_0)
                                           (fun-rank fun_1) ...))))
        ; all functions in array must have same expected ranks
        (side-condition (term (all-equal? ((fun-rank fun_0)
                                           (fun-rank fun_1) ...))))
        ; and that rank must be natural
        (where (natural_funrank ...)
               (fun-rank fun_0))
        ; ensure they are all overranked by the same amount
        (side-condition (term (same-overrank?
                               [(0 arr/f) (natural_funrank arr/v) ...])))
        (side-condition (term (all ((overrank? 0 arr/f)
                                    (overrank? natural_funrank arr/v) ...))))
        
        (where ((A () (fun)) ...) (cells/rank 0 arr/f))
        (where ((arr_cell ...) ...)
               (transpose ((cells/rank natural_funrank arr/v) ...)))
        arr/f-map]
   [--> (in-hole E (arr/f arr/v ...))
        (in-hole E (arr/f_natrank arr/v ...))
        (where (A (num_dim ...) (fun ...)) arr/f)
        ; don't naturalize if function array elts are already natural-ranked
        (side-condition (not (redex-match Arrays ((natural ...) ...)
                                          (term ((fun-rank fun) ...)))))
        (where (fun_natrank0 fun_natrank1 ...)
               ((naturalize-rank fun arr/v ...) ...))
        (where arr/f_natrank (A (num_dim ...) (fun_natrank0 fun_natrank1 ...)))
        ; don't naturalize a scalar containing a builtin where there are no
        ; overranked args
        ; TODO: explain in paper why naturalize builtins -- non-scalar arrays
        ;       of builtins need to be naturalized  to set up for lifting
        (side-condition (or (not (redex-match Arrays (A [] [op]) (term arr/f)))
                            (for/or [(x (term (fun-rank fun_natrank0)))
                                     (v (term (arr/v ...)))]
                              (term (overrank? ,x ,v)))))
        arr/f-naturalize]
   [--> (in-hole E (arr/f arr/v ...))
        (in-hole E (arr/f_lifted arr/v_lifted ...))
        ; require a nonempty array of functions
        (where (A (num_fundim ...) (fun_0 fun_1 ...)) arr/f)
        ; all functions in array must have same expected ranks
        (side-condition (term (all-equal? ((fun-rank fun_0)
                                           (fun-rank fun_1) ...))))
        ; must find natural rank
        (where (natural_funrank ...) (fun-rank fun_0))
        ; the effective expected rank of `apply' is now (0 num_funrank ...)
        ; ensure they don't all have the same overrank
        (side-condition (not (term (same-overrank?
                                    [(0 arr/f) (natural_funrank arr/v) ...]))))
        ; break arr/f into 0-cells, arr/v into num_funrank-cells
        (where (arr/f_lifted arr/v_lifted ...)
               (frame-lift ([0 arr/f] [natural_funrank arr/v] ...)))
        arr/f-lift]
   ; collapse an array-of-arrays into flat array
   [--> (in-hole E (A (num_frame-dim ...) (arr/pv ...)))
        ; append cell shape onto frame shape
        (in-hole E (A (num_frame-dim ... num_cell-dim ...) any_v))
        ; TODO: tighten this pattern condition?
        (where any_v ,(foldr append '() (term ((value arr/pv) ...))))
        ; all cells must have the same shape
        (where (num_cell-dim ...) (shape-of-all arr/pv ...))
        (where ((A (num ...) (base ...)) ...) (arr/pv ...))
        collapse]
   [--> (in-hole E (A (num_frame-dim ...) (arr/pv ...)))
        ; append cell shape onto frame shape
        (in-hole E (A (num_frame-dim ... num_cell-dim ...) any_v))
        ; TODO: tighten this pattern condition?
        (where any_v ,(foldr append '() (term ((value arr/pv) ...))))
        ; all cells must have the same shape
        (where (num_cell-dim ...) (shape-of-all arr/pv ...))
        (where ((A (num ...) (fun ...)) ...) (arr/pv ...))
        collapse2]
   [--> (in-hole E (A [] [box-val]))
        (in-hole E box-val)
        box-collapse]
   ))




;; macro to convert number to rank-0 array
(define-metafunction Arrays
  scalar : any -> arr
  [(scalar any) (A () (any))])
;; macro to make scalar array out of a λ term
(define-metafunction Arrays
  sλ : ([var num] ...) expr -> expr
  [(sλ ([var num] ...) expr) (scalar (λ ([var num] ...) expr))])


;; apply a function to a chain of arrays, i.e.
;; arr_0 `fun` arr_1 `fun` arr_2 `fun` arr_3 `fun` ... `fun` arr_n
(define-metafunction Arrays
  chain-apply/r : arr/f (arr ...) -> expr
  ;[(chain-apply/r fun ()) (id-element/r fun)]
  [(chain-apply/r arr/f (arr)) arr]
  [(chain-apply/r arr/f (arr_0 arr_1 ...))
   (arr/f arr_0 (chain-apply/r arr/f (arr_1 ...)))])
(define-metafunction Arrays
  chain-apply/l : arr/f (arr ...) -> expr
  ;[(chain-apply/l fun ()) (id-element/l fun)]
  [(chain-apply/l arr/f (arr)) arr]
  [(chain-apply/l arr/f (arr_0 ... arr_1))
   (arr/f (chain-apply/l arr/f (arr_0 ...)) arr_1)])
;; similar to chain-apply but for tree-shaped application to the arrays
(define-metafunction Arrays
  tree-apply : arr/f arr (arr ...) -> any
  [(tree-apply arr/f arr_base ()) arr_base]
  [(tree-apply arr/f arr_base (arr)) arr]
  [(tree-apply arr/f arr_base (arr_0 arr_1)) (arr/f arr_0 arr_1)]
  [(tree-apply arr/f arr_base (arr ...))
   #;((arr_0 ...) || (arr_1 ...))
   (arr/f (tree-apply arr/f arr_base (arr_0 ...))
          (tree-apply arr/f arr_base (arr_1 ...)))
   (where num_length (length/m (arr ...)))
   (where (arr_0 ...) (take/m (arr ...) ,(quotient (term num_length) 2)))
   (where (arr_1 ...) (drop/m (arr ...) ,(quotient (term num_length) 2)))])

;; get the neutral/identity element of a function
(define-metafunction Arrays
  id-element/r : fun -> arr
  [(id-element/r +) (scalar 0)]
  [(id-element/r -) (scalar 0)]
  [(id-element/r *) (scalar 1)]
  [(id-element/r /) (scalar 1)])
(define-metafunction Arrays
  id-element/l : fun -> arr
  [(id-element/l +) (scalar 0)]
  [(id-element/l *) (scalar 1)])

;; rewrite a function to eliminate negative/infinite arg ranks
(define-metafunction Arrays
  naturalize-rank : fun arr ... -> fun or #f
  [(naturalize-rank (λ [(var num) ...] expr) arr ...)
   (λ [(var natural) ...] expr)
   (where (natural ...) ((natural-cell-rank num arr) ...))]
  [(naturalize-rank op arr ...)
   (λ [(var natural) ...] ((scalar op) var ...))
   (where (num ...) (fun-rank op))
   (side-condition (for/or [(r (term (num ...)))]
                     (not (exact-nonnegative-integer? r))))
   (where (var ...) ,(for/list [(x (length (term (num ...))))]
                       (gensym)))
   (where (natural ...) ((natural-cell-rank num arr) ...))]
  [(naturalize-rank fun arr ...) #f])
(define-metafunction Arrays
  natural-cell-rank : num arr -> num
  [(natural-cell-rank natural arr) natural]
  [(natural-cell-rank +inf.0 arr) (rank arr)]
  [(natural-cell-rank num_neg arr) ,(+ (term (rank arr)) (term num_neg))])

;; get the shape of all given arrays, or return #f if not all are the same
(define-metafunction Arrays
  shape-of-all : arr ... -> (num ...) ∨ #f
  [(shape-of-all arr)
   (shape arr)]
  [(shape-of-all arr_0 arr_1 ...)
   (shape arr_0)
   (side-condition (equal? (term (shape arr_0))
                           (term (shape-of-all arr_1 ...))))]
  [(shape-of-all arr ...) #f])

;; map a function over the cells of its arguments
;; only use after all arrays have been lifted into same frame
(define-metafunction Arrays
  array-map : fun (arr ...) -> arr or #f
  [(array-map fun (arr_arg ...))
   ; break array into cells, construct array whose value is the computations
   ; from the function mapping
   (A (num_framedim ...)
      (cell-apply fun ((arr_cell ...) ...)))
   (where (num_fun-rank ...) (fun-rank fun))
   (where ((num_cell-dim ...) ...)
          ((take-right/m (shape arr_arg) num_fun-rank) ...))
   (where ((arr_cell ...) ...) ((cells/shape (num_cell-dim ...) arr_arg) ...))
   (where (num_framedim ...) ,(drop-right (first (term ((shape arr_arg) ...)))
                                          (first (term (num_fun-rank ...)))))
   ; make sure all things are being mapped over the same frame
   (where ((num_frame-dim ...) ...)
          ((drop-right/m (shape arr_arg) num_fun-rank) ...))
   (side-condition (term (all-equal? ((num_frame-dim ...) ...))))]
  ; did not meet conditions for mapping -> return #f
  [(array-map fun (arr_arg ...)) #f])

;; set up function application for each cell group (1st cells, 2nd cells, etc.)
(define-metafunction Arrays
  cell-apply : fun ((arr ...) ...) -> ((fun arr ...) ...)
  [(cell-apply fun ()) ()]
  [(cell-apply fun (() ...)) ()]
  [(cell-apply fun ((arr ...) ...))
   ; TODO: why cons? better to do this with pattern
   ,(cons (cons (term fun) (map first (term ((arr ...) ...))))
          (term (cell-apply fun ,(map rest (term ((arr ...) ...))))))])

;; apply a builtin operator
(define-metafunction Arrays
  apply-op : op (arr ...) -> arr
  [(apply-op + ((A () (num_1)) (A () (num_2))))
   (A () (,(+ (term num_1) (term num_2))))]
  [(apply-op - ((A () (num_1)) (A () (num_2))))
   (A () (,(- (term num_1) (term num_2))))]
  [(apply-op * ((A () (num_1)) (A () (num_2))))
   (A () (,(* (term num_1) (term num_2))))]
  [(apply-op / ((A () (num_1)) (A () (num_2))))
   (A () (,(/ (term num_1) (term num_2))))]
  [(apply-op < ((A () (num_1)) (A () (num_2))))
   (A () (,(< (term num_1) (term num_2))))]
  [(apply-op > ((A () (num_1)) (A () (num_2))))
   (A () (,(> (term num_1) (term num_2))))]
  [(apply-op <= ((A () (num_1)) (A () (num_2))))
   (A () (,(<= (term num_1) (term num_2))))]
  [(apply-op >= ((A () (num_1)) (A () (num_2))))
   (A () (,(>= (term num_1) (term num_2))))]
  [(apply-op = ((A () (num_1)) (A () (num_2))))
   (A () (,(= (term num_1) (term num_2))))]
  [(apply-op and ((A () (bool_1)) (A () (bool_2))))
   (A () (,(and (term bool_1) (term bool_2))))]
  [(apply-op or ((A () (bool_1)) (A () (bool_2))))
   (A () (,(or (term bool_1) (term bool_2))))]
  [(apply-op not ((A () (bool_1)) (A () (bool_2))))
   (A () (,(not (term bool_1) (term bool_2))))]
  [(apply-op exp ((A () (num))))
   (A () (,(exp (term num))))]
  [(apply-op log ((A () (num))))
   (A () (,(log (term num))))]
  [(apply-op append (arr_1 arr_2))
   (op/append arr_1 arr_2)]
  [(apply-op iota ((A (num_dim) (num_elt ...))))
   (box (A (num_elt ...) ,(for/list [(n (foldr * 1 (term (num_elt ...))))] n)))
   (where (natural ...) (num_elt ... ))]
  [(apply-op reduce arr_fun arr_base arr_arg)
   (tree-apply arr_fun arr_base (arr_cell ...))
   (where (arr_cell ...) (cells/rank -1 arr_arg))]
  #;[(apply-op reshape arr_newshape arr_elts)
     (op/reshape arr_newshape arr_elts)]
  )

;; extract or look up ranks of a function
(define-metafunction Arrays
  fun-rank : fun -> (num ...)
  [(fun-rank +) (0 0)]
  [(fun-rank -) (0 0)]
  [(fun-rank *) (0 0)]
  [(fun-rank /) (0 0)]
  [(fun-rank <) (0 0)]
  [(fun-rank >) (0 0)]
  [(fun-rank <=) (0 0)]
  [(fun-rank >=) (0 0)]
  [(fun-rank =) (0 0)]
  [(fun-rank and) (0 0)]
  [(fun-rank or) (0 0)]
  [(fun-rank not) (0 0)]
  [(fun-rank exp) (0)]
  [(fun-rank log) (0)]
  [(fun-rank append) (+inf.0 +inf.0)]
  [(fun-rank itemize) (+inf.0)]
  [(fun-rank shape-of) (+inf.0)]
  [(fun-rank reshape) (1 +inf.0)]
  [(fun-rank transpose) (1 +inf.0)]
  [(fun-rank head) (+inf.0)]
  [(fun-rank behead) (+inf.0)]
  [(fun-rank tail) (+inf.0)]
  [(fun-rank curtail) (+inf.0)]
  [(fun-rank iota) (1)]
  [(fun-rank (λ [(var num) ...] expr)) (num ...)]
  ;[(fun-rank reduce) (+inf.0)]
  [(fun-rank reduce) (+inf.0 +inf.0 +inf.0)]
  ;[(fun-rank (reduce expr)) (+inf.0)]
  [(fun-rank fold/r) (+inf.0 +inf.0 +inf.0)]
  ;[(fun-rank (fold/r expr)) (+inf.0)]
  [(fun-rank fold/l) (+inf.0 +inf.0 +inf.0)]
  ;[(fun-rank (fold/l expr)) (+inf.0)]
  )


;;; metafunctions for handling primitive operations that process arrays
(define-metafunction Arrays
  op/append : arr arr -> arr
  ; consider args as lists -- list entries must have matching shapes
  ; length of each list can be anything
  [(op/append (A (num_len1 num_elt-dim ...) (el-expr_val1 ...))
              (A (num_len2 num_elt-dim ...) (el-expr_val2 ...)))
   (A (,(+ (term num_len1) (term num_len2)) num_elt-dim ...)
      (el-expr_val1 ... el-expr_val2 ...))])
(define-metafunction Arrays
  op/itemize : arr -> arr
  [(op/itemize (A (num_dim ...) (el-expr_val ...)))
   (A (1 num_dim ...) (el-expr_val ...))])
(define-metafunction Arrays
  op/reshape : arr arr -> arr
  [(op/reshape (A (num) (num_new-dim ...))
               (A (num_old-dim ...) (el-expr ...)))
   (box (A (num_new-dim ...)
           ,(for/list ([i (term num_new-size)])
              (list-ref (term (el-expr ...)) (modulo i (term num_old-size))))))
   (where num_new-size ,(foldr * 1 (term (num_new-dim ...))))
   (where num_old-size ,(foldr * 1 (term (num_old-dim ...))))])
(define-metafunction Arrays
  op/transpose : arr arr -> arr
  [(op/transpose (A (num) (num_permute ...))
                 (A (num_old-dim ...) (el-expr ...)))
   (A (num_new-dim ...) (el-expr_new-val ...))
   (side-condition (permutation? (term num) (term (num_permute ...))))
   (side-condition (equal? (term num) (length (term (num_old-dim ...)))))
   (where num_size ,(foldr * 1 (term (num_old-dim ...))))
   (where (num_new-dim ...) ,(permute (term (num_permute ...))
                                      (term (num_old-dim ...))))
   (where
    (el-expr_new-val ...)
    ,(for/list ([i (term num_size)])
       (define deep/new-shape (flat->deep (term (num_new-dim ...)) i))
       (define deep/old-shape (permute (perm-inverse (term (num_permute ...)))
                                       deep/new-shape))
       (define flat/old-shape (deep->flat (term (num_old-dim ...))
                                          deep/old-shape))
       (list-ref (term (el-expr ...)) flat/old-shape)))])
(define-metafunction Arrays
  op/nub-sieve : arr -> arr
  [(op/nub-sieve (A (num_dim ...) (elt ...)))
   (A (num_dim ...) (nub-sieve (elt ...)))])
(define-metafunction Arrays
  nub-sieve : (any ...) -> (bool ...)
  [(nub-sieve (any_0 ... any_1 any_2 ... any_1))
   (any_nubbed ... #f)
   (where (any_nubbed ...) (nub-sieve (any_0 ... any_1 any_2 ...)))]
  [(nub-sieve (any_0 ... any_1))
   (any_nubbed ... #t)
   (where (any_nubbed ...) (nub-sieve (any_0 ...)))]
  [(nub-sieve ()) ()])
(define-metafunction Arrays
  op/head : arr -> arr
  [(op/head (A (num_len num_dim ...) (el-expr_val ...)))
   (A (num_dim ...)
      (take/m (el-expr_val ...)
              ,(foldr * 1 (term (num_dim ...)))))])
(define-metafunction Arrays
  op/behead : arr -> arr
  [(op/behead (A (num_len num_dim ...) (el-expr_val ...)))
   (A (,(sub1 (term num_len)) num_dim ...)
      (drop/m (el-expr_val ...)
              ,(foldr * 1 (term (num_dim ...)))))])
(define-metafunction Arrays
  op/tail : arr -> arr
  [(op/tail (A (num_len num_dim ...) (el-expr_val ...)))
   (A (num_dim ...)
      (take-right/m (el-expr_val ...)
                    ,(foldr * 1 (term (num_dim ...)))))])
(define-metafunction Arrays
  op/curtail : arr -> arr
  [(op/curtail (A (num_len num_dim ...) (el-expr_val ...)))
   (A (,(sub1 (term num_len)) num_dim ...)
      (drop-right/m (el-expr_val ...)
                    ,(foldr * 1 (term (num_dim ...)))))])

;; utility functions for builtin operators
; convert dimensions to the offset associated with each dimensions
(define (shape->offsets shape)
  (for/fold
      ([offlist '(1)]) ([x (in-range (sub1 (length shape)) 0 -1)])
    (cons (* (first offlist) (list-ref shape x)) offlist)))
; convert direct index into value part of array to an index based on the shape
(define (flat->deep shape flat-idx)
  (define offsets (shape->offsets shape))
  (define-values (leftover backwards-index)
    (for/fold ([fmod flat-idx]
               [deep-idx '()])
      ([off offsets])
      (values (remainder fmod off)
              (cons (floor (/ fmod off)) deep-idx))))
  (reverse backwards-index))
; convert an index based on the shape to direct index into value part of array
(define (deep->flat shape idxs)
  (cond [(equal? #() shape) 0]
        [else (foldr + 0 (map * idxs (shape->offsets shape)))]))
; apply a permutation to a list
(define (permute per xs)
  (for/list ([i per]) (list-ref xs i)))
; ensure that a given list properly represents a permutation of the right size
(define (permutation? n p)
  (equal? (sort p <)
          (sequence->list n)))
; find the inverse of a permutation
(define (perm-inverse p)
  (define (list-index x xs)
    (cond [(empty? xs) -1]
          [(equal? x (first xs)) 0]
          [else (add1 (list-index x (rest xs)))]))
  (for/list ([i (length p)])
    (list-index i p)))

;; capture-avoiding substitution
(define-metafunction Arrays
  subst : [(var expr) ...] el-expr -> el-expr
  [(subst [(var expr) ...] base) base]
  [(subst [(var expr) ...] op) op]
  [(subst [(var expr) ...] (box expr_c))
   (box (subst [(var expr) ...] expr_c))]
  [(subst [(var expr) ...] (A (num_sh ...) (el-expr_val ...)))
   (A (num_sh ...) ((subst [(var expr) ...] el-expr_val) ...))]
  [(subst [(var expr) ...] (op expr_arg ...))
   (op (subst [(var expr) ...] expr_arg) ...)]
  [(subst [(var expr) ...] (expr_fun expr_arg ...))
   ((subst [(var expr) ...] expr_fun) (subst [(var expr) ...] expr_arg) ...)]
  [(subst [(var_0 expr_0) ... (var_1 expr_1) (var_2 expr_2) ...] var_1) expr_1]
  [(subst [(var_0 expr_0) ...] var_1) var_1]
  [(subst [(var_sub expr_sub) ...] (λ [(var_arg num_arg) ...] expr_body))
   (λ [(var_arg num_arg) ...]
     (subst (shadow [(var_sub expr_sub) ...] (var_arg ...)) expr_body))])

;; eliminate some variables from a substitution list
(define-metafunction Arrays
  shadow : [(var expr) ...] (var ...) -> [(var expr) ...]
  ; empty sub list -> no change
  [(shadow [] ()) []]
  ; empty shadow list -> no change
  [(shadow [(var expr) ...] ()) [(var expr) ...]]
  ; first in shadow list is also in sub list -> remove from sub list, recur
  [(shadow [(var_sub0 expr_0) ... (var_sub1 expr_1) (var_sub2 expr_2) ...]
           (var_sub1 var_shadow ...))
   (shadow [(var_sub0 expr_0) ... (var_sub2 expr_2) ...]
           (var_sub1 var_shadow ...))]
  ; first in shadow list is not in sub list -> remove from shadow list, recur
  [(shadow [(var_sub expr) ...]
           (var_shadow0 var_shadow1 ...))
   (shadow [(var_sub expr) ...]
           (var_shadow1 ...))])


;; make sure array is well-shaped: product of shape must be length of value
;; only usable on array whose exact shape is known (shape piece normalized)
(define-metafunction Arrays
  well-shaped : arr -> bool
  [(well-shaped (A (num_s ...) (el-expr_v ...)))
   ,(= (foldr * 1 (term (num_s ...)))
       (length (term (el-expr_v ...))))])

;; make sure array is at desired rank
(define-metafunction Arrays
  at-rank? : num arr -> bool
  [(at-rank? num arr)
   #t
   (where num (rank arr))]
  [(at-rank? +inf.0 arr) #t]
  [(at-rank? num arr) #f])

;; make sure array is above desired rank
(define-metafunction Arrays
  overrank? : num arr -> bool
  [(overrank? +inf.0 arr) #f]
  [(overrank? num arr)
   #t
   (side-condition (< (term num) (term (rank arr))))]
  [(overrank? num arr) #f])

;; find how far above desired rank array is
(define-metafunction Arrays
  overrank : num arr -> num or #f
  [(overrank +inf.0 arr) 0]
  [(overrank num_neg arr)
   ,(- (term num_neg))
   (side-condition (negative? (term num_neg)))
   (side-condition (>= (- (term num_neg)) (term (rank arr))))]
  [(overrank num arr) ,(- (term num) (term (rank arr)))]
  [(overrank num arr) #f])

;; make sure all arrays are overranked by the same amount
(define-metafunction Arrays
  same-overrank? : [(num arr) ...] -> bool
  [(same-overrank? [(num arr) ...]) (all-equal? ((overrank num arr) ...))])


;; extract rank of array
(define-metafunction Arrays
  rank : arr -> num
  [(rank (A (num ...) (el-expr ...)))
   ,(length (term (num ...)))]
  [(rank (box expr)) 0])

;; extract shape of array
(define-metafunction Arrays
  shape : arr -> (num ...)
  [(shape (A (num ...) (el-expr ...))) (num ...)]
  [(shape (box expr)) ()])

;; extract value of array
(define-metafunction Arrays
  value : arr -> (el-expr ...)
  [(value (A (num ...) (el-expr ...))) (el-expr ...)])

;; grow argument arrays by duplication so they all have their desired ranks
;; cell ranks must be naturalized
(define-metafunction Arrays
  ; [(cell-rank array) ...]
  frame-lift : [(num arr) ...] -> (arr ...) or #f
  [(frame-lift []) ()]
  ; make sure arrays can be lifted into same frame
  ; (need prefix relation for frame shapes)
  ; "principal frame" comes from least-overranked array
  [(frame-lift [(num_cr arr) ...])
   ((cell-dup num_cr (num_pr-frame-dim ...) arr) ...)
   ; extract frame shapes
   (where ((num_fr ...) ...)
          ((drop-right/m (shape arr) num_cr) ...))
   ; find the longest one -- that is the principal frame
   (where (num_pr-frame-dim ...) (longest ((num_fr ...) ...)))
   ; all other frames must be prefixes of it
   (side-condition
    (term (all ((prefix? (num_fr ...) (num_pr-frame-dim ...)) ...))))]
  ; not a frame-liftable input (e.g. due to frame mismatch)
  [(frame-lift any) #f])

;; duplicate cells of given array to lift it into desired frame
(define-metafunction Arrays
  ; cell rank, frame shape, initial array
  cell-dup : num (num ...) arr -> arr
  ; All elements of a single cell should appear consecutively in value segment
  ; Just split value into chunks, repeat chunks right number of times, and
  ; update the shape.
  [(cell-dup num_cell-rank (num_frame-dim ...) arr)
   ; new array's shape is frame-portion ++ growth-portion ++ cell-shape
   ; new array's value comes from repeating the cells (number of copies is
   ; product of the "growth" portion of the shape)
   (A ,(append (term (drop-right/m (shape arr) num_cell-rank))
               (term (num_growth ...))
               (term (take-right/m (shape arr) num_cell-rank)))
      ,(foldr append '()
              (term ((repeat ,(foldr * 1 (term (num_growth ...)))
                             (el-expr_cell ...)) ...))))
   ; break the array's value segment into its cells
   (where ((el-expr_cell ...) ...)
          (cell-values (take-right/m (shape arr) num_cell-rank) arr))
   ; identify the part of the result shape that comes from lifting
   ; drop frame portion of array from left side of frame
   (where (num_growth ...)
          (drop/m (num_frame-dim ...)
                  ,(- (term (rank arr)) (term num_cell-rank))))
   ; require that the array actually be liftable into the frame
   ; i.e. frame portion of array must be prefix of given frame
   (side-condition (term (prefix? (drop-right/m (shape arr) num_cell-rank)
                                  (num_frame-dim ...))))])

;; repeat the list the given number of times
(define-metafunction Arrays
  repeat : num (any ...) -> (any ...)
  [(repeat 0 (any ...)) ()]
  [(repeat num (any ...))
   ,(append (term (any ...))
            (term (repeat ,(- (term num) 1) (any ...))))])

;; extract the value segments of an array's cells
(define-metafunction Arrays
  ; cell shape, array
  cell-values : (num ...) arr -> ((el-expr ...) ...)
  [(cell-values (num_cellshape ...) arr)
   ((el-expr ...) ...)
   (where ((A (num ...) (el-expr ...)) ...)
          (cells/shape (num_cellshape ...) arr))])

;; split an array into cells
(define-metafunction Arrays
  ; cell shape, array
  cells/shape : (num ...) arr -> (arr ...)
  [(cells/shape (num_cell-dim ...) (A (num_arr-dim ...) ())) ()]
  [(cells/shape (num_cell-dim ...) (A (num_arr-dim ...) (el-expr ...)))
   ,(cons (term (A (num_cell-dim ...) (take/m (el-expr ...) num_cellsize)))
          ; drop one cell's elements from array, and split remaining elements
          (term (cells/shape (num_cell-dim ...)
                             (A (num_arr-dim ...)
                                (drop/m (el-expr ...) num_cellsize)))))
   (where num_cellsize ,(foldr * 1 (term (num_cell-dim ...))))])
(define-metafunction Arrays
  ; cell rank, array
  cells/rank : num arr -> (arr ...)
  [(cells/rank natural arr)
   (cells/shape (take-right/m (shape arr) natural) arr)]
  [(cells/rank +inf.0 arr) (arr)]
  [(cells/rank num_neg arr)
   (cells/shape (drop/m (shape arr) ,(- (term num_neg))) arr)
   (side-condition (and (exact-integer? (term num_neg))
                        (negative? (term num_neg))))])

;; make sure all arrays can be lifted into required frame
(define-metafunction Arrays
  frame-match : [(num arr) ...] -> bool
  [(frame-match [(num arr) ...])
   (all [(prefix? (num_arr-dim ...) (num_frame-dim ...)) ...])
   (where ((num_arr-dim ...) ...) (frame-shapes [(num arr) ...]))
   (where (num_frame-dim ...) (longest (frame-shapes [(num arr) ...])))])

;; find the frame component of the arrays' shapes
(define-metafunction Arrays
  frame-shapes : [(num arr) ...] -> [(num ...) ...]
  [(frame-shapes [(num_cellrank (A (num_shape ...) (el-expr ...))) ...])
   ; can't just use
   ; [,(drop-right (term num_rank) (term (num_shape ...))) ...]
   ; because it doesn't associate num_rank with the last ...
   [(drop-right/m (num_shape ...) num_cellrank) ...]])

;; find longest list in a list of lists
(define-metafunction Arrays
  longest : [(any ...) ...] -> (any ...)
  [(longest [(any ...)]) (any ...)]
  [(longest [(any_1 ...) (any_2 ...) (any_3 ...) ...])
   (longer (any_1 ...) (longest [(any_2 ...) (any_3 ...) ...]))])
;; select longer of two lists
(define-metafunction Arrays
  longer : (any ...) (any ...) -> (any ...)
  [(longer (any_1 ...) (any_2 ...))
   (any_2 ...)
   (side-condition (> (length (term (any_2 ...)))
                      (length (term (any_1 ...)))))]
  [(longer (any_1 ...) (any_2 ...)) (any_1 ...)])

;; check that one input list is a prefix of the other
(define-metafunction Arrays
  prefix? : (any ...) (any ...) -> bool
  [(prefix? (any_1 ...) (any_1 ... any_2 ...)) #t]
  [(prefix? (any_1 ...) (any_2 ...)) #f])

;; check that one input list is a suffix of the other
(define-metafunction Arrays
  suffix? : (any ...) (any ...) -> bool
  [(suffix? (any_2 ...) (any_1 ... any_2 ...)) #t]
  [(suffix? (any_1 ...) (any_2 ...)) #f])

;; check that all list entries are the same
(define-metafunction Arrays
  all-equal? : (any ...) -> bool
  [(all-equal? ()) #t]
  [(all-equal? (any)) #t]
  [(all-equal? (any_1 any_1 any_2 ...))
   ,(and (term (all-equal? (any_1 any_2 ...))))]
  [(all-equal? any) #f])

; TODO: macro for generating metafunction from builting racket function
;; metafunction wrapper for existing take function
(define-metafunction Arrays
  take/m : (any ...) num -> (any ...)
  [(take/m (any ...) num) ,(take (term (any ...)) (term num))])

;; metafunction wrapper for existing drop function
(define-metafunction Arrays
  drop/m : (any ...) num -> (any ...)
  [(drop/m (any ...) num) ,(drop (term (any ...)) (term num))])

;; metafunction wrapper for existing drop-right function
(define-metafunction Arrays
  take-right/m : (any ...) num -> (any ...)
  [(take-right/m (any ...) num) ,(take-right (term (any ...)) (term num))])

;; metafunction wrapper for existing drop-right function
(define-metafunction Arrays
  drop-right/m : (any ...) num -> (any ...)
  [(drop-right/m (any ...) num) ,(drop-right (term (any ...)) (term num))])

;; metafunction wrapper for existing length function
(define-metafunction Arrays
  length/m : (any ...) -> num
  [(length/m (any ...)) ,(length (term (any ...)))])

;; metafunction wrapper for existing > function
(define-metafunction Arrays
  >/m : num num -> bool
  [(>/m num_1 num_2) ,(> (term num_1) (term num_2))])

;; make sure all list entries are true
(define-metafunction Arrays
  all : (bool ...) -> bool
  [(all (#t ...)) #t]
  [(all any) #f])

;; transpose ((x_1,1 x_1,2 ...) (x_2,1 x_2,2 ...) ...)
;; to ((x_1,1 x_2,1 ...) (x_1,2 x_2,2 ...) ...)
(define-metafunction Arrays
  transpose : ((any ...) ...) -> ((any ...) ...)
  [(transpose ()) ()]
  [(transpose (() ...)) ()]
  [(transpose ((any_0 any_1 ...) ...))
   ,(cons (term (any_0 ...))
          (term (transpose ((any_1 ...) ...))))])


(module+
 test
 
 ; three simple examples for how rank affects array lifting
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar +) (A (3 3) (1 2 3
                                 4 5 6
                                 7 8 9))
                     (A (3) (10 20 30)))))
  (term ((A (3 3) (11 12 13
                      24 25 26
                      37 38 39)))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((sλ ([x 1][y 1]) ((scalar +) x y)) (A (3 3) (1 2 3
                                                         4 5 6
                                                         7 8 9))
                                             (A (3) (10 20 30)))))
  (term ((A (3 3) (11 22 33
                      14 25 36
                      17 28 39)))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((sλ ([x -1][y 1]) ((scalar +) x y)) (A (3 3) (1 2 3
                                                          4 5 6
                                                          7 8 9))
                                              (A (3) (10 20 30)))))
  (term ((A (3 3) (11 22 33
                      14 25 36
                      17 28 39)))))
 
 ; reducing/folding along different axes/directions
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar reduce)
          (scalar +) (A [3] [0 0 0])
          (A (3 3) (1 2 3
                      4 5 6
                      7 8 9)))))
  (term ((A (3) (12 15 18)))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar fold/r)
          (scalar -) (scalar 0)
          (A (4) (1 1 1 1)))))
  (term ((A () (0)))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar fold/l)
          (scalar -) (scalar 0)
          (A (4) (1 1 1 1)))))
  (term ((A () (-4)))))
 
 
 ;; builtin whole-array operators:
 ; append
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar append) (A (3 2) (2 3 4 5 6 7))
                          (A (2 2) (0 1 -1 -2)))))
  (term ((A (5 2) (2 3 4 5 6 7 0 1 -1 -2)))))
 ; itemize
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar itemize) (A (3 2) (2 3 4 5 6 7)))))
  (term ((A (1 3 2) (2 3 4 5 6 7)))))
 ; transpose
 (check-equal?
  (term (op/transpose (A (3) (2 0 1)) (A (1 2 3) (90 80 70 60 50 40))))
  (term (A (3 1 2) (90 60 80 50 70 40))))
 ; head
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar head) (A (4) (2 5 4 1)))))
  (term ((A () (2)))))
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar head) (A (4 2) (3 6 5 9 7 1 0 8)))))
  (term ((A (2) (3 6)))))
 ; behead
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar behead) (A (4) (2 5 4 1)))))
  (term ((A (3) (5 4 1)))))
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar behead) (A (4 2) (3 6 5 9 7 1 0 8)))))
  (term ((A (3 2) (5 9 7 1 0 8)))))
 ; tail
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar tail) (A (4) (2 5 4 1)))))
  (term ((A () (1)))))
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar tail) (A (4 2) (3 6 5 9 7 1 0 8)))))
  (term ((A (2) (0 8)))))
 ; curtail
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar curtail) (A (4) (2 5 4 1)))))
  (term ((A (3) (2 5 4)))))
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar curtail) (A (4 2) (3 6 5 9 7 1 0 8)))))
  (term ((A (3 2) (3 6 5 9 7 1)))))
 ; shape-of
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar shape-of) (A (4 2) (3 6 5 9 7 1 0 8)))))
  (term ((A (2) (4 2)))))
 ; reshape
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar reshape) (A (2) (2 3)) (A (3 4) (2 9 4 3 5 2 7 4 2 1 0 9)))))
  (term ((box (A (2 3) (2 9 4 3 5 2))))))
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar reshape) (A (2) (3 3)) (A (4) (2 9 4 3)))))
  (term ((box (A (3 3) (2 9 4 3 2 9 4 3 2))))))
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar reshape) (A (2 2) (3 3 4 2)) (A (4) (2 9 4 3)))))
  (term ((A [2] [(box (A (3 3) (2 9 4 3 2 9 4 3 2)))
                 (box (A (4 2) (2 9 4 3 2 9 4 3)))]))))
 ; nub-sieve
 (check-equal?
  (term (op/nub-sieve (A (3 4) (2 9 4 3 5 2 7 4 2 1 0 9))))
  (term (A (3 4) (#t #t #t #t #t #f #t #f #f #t #t #f))))
 ; iota
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar iota) (A (3) (2 1 3)))))
  (term ((box (A (2 1 3) (0 1 2 3 4 5))))))
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((scalar iota) (A (2 2) (2 1 4 3)))))
  (term ((A (2) ((box (A (2 1) (0 1)))
                 (box (A (4 3) (0 1 2 3 4 5 6 7 8 9 10 11))))))))
 
 
 ; some terms which should not be reducible (due to shape mismatch)
 (check-equal?
  (apply-reduction-relation
   ->Array
   (term ((scalar +) (A (6) (1 2 3 4 5 6))
                     (A (3) (10 20 30)))))
  '())
 
 (check-equal?
  (apply-reduction-relation
   ->Array
   (term ((scalar +) (A (2 3) (1 2 3
                                 4 5 6))
                     (A (3) (10 20 30)))))
  '())
 
 ; make sure currying doesn't change how lifting works
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((sλ ([x 0]) (sλ ([y 0]) ((scalar +) x y))) (A () (5))) (A () (10)))))
  (apply-reduction-relation*
   ->Array
   (term ((sλ ([x 0] [y 0]) ((scalar +) x y)) (A () (5)) (A () (10))))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((sλ ([x 0]) (sλ ([y 0]) ((scalar +) x y)))
           (A () (5)))
          (A (2) (10 20)))))
  (apply-reduction-relation*
   ->Array
   (term ((sλ ([x 0] [y 0]) ((scalar +) x y)) (A () (5)) (A (2) (10 20))))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((sλ ([x 0]) (sλ ([y 0]) ((scalar +) x y)))
           (A (2 2) (1 2
                       3 4)))
          (A (2) (10 20)))))
  (apply-reduction-relation*
   ->Array
   (term ((sλ ([x 0] [y 0]) ((scalar +) x y))
          (A (2 2) (1 2
                      3 4))
          (A (2) (10 20))))))
 
 ; check that arrays of functions with non-natural rank get applied properly
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((A (2) ((λ ([x +inf.0]) ((scalar *) (scalar 10) x))
                  (λ ([x +inf.0]) ((scalar -) x (scalar 5)))))
          (A (4) (1 2 3 4)))))
  (term ((A (2 4) (10 20 30 40
                      -4 -3 -2 -1)))))
 
 ; currying with non-natural rank
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((sλ ([x 0]) (sλ ([y -1]) ((scalar +) x y)))
           (A (2) (1 2)))
          (A (2 2) (10 20 30 40)))))
  (apply-reduction-relation*
   ->Array
   (term ((sλ ([x 0][y -1]) ((scalar +) x y))
          (A (2) (1 2))
          (A (2 2) (10 20 30 40))))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((sλ ([x -1]) (sλ ([y 0]) ((scalar +) x y)))
           (A (2) (1 2)))
          (A (2 2) (10 20 30 40)))))
  (apply-reduction-relation*
   ->Array
   (term ((sλ ([x -1][y 0]) ((scalar +) x y))
          (A (2) (1 2))
          (A (2 2) (10 20 30 40))))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((sλ ([x 0]) (sλ ([y +inf.0]) ((scalar +) x y)))
           (A (2) (1 2)))
          (A (2 2) (10 20 30 40)))))
  (apply-reduction-relation*
   ->Array
   (term ((sλ ([x 0][y +inf.0]) ((scalar +) x y))
          (A (2) (1 2))
          (A (2 2) (10 20 30 40))))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((sλ ([x -1]) (sλ ([y +inf.0]) ((scalar +) x y)))
           (A (2) (1 2)))
          (A (2 2) (10 20 30 40)))))
  (apply-reduction-relation*
   ->Array
   (term ((sλ ([x -1][y +inf.0]) ((scalar +) x y))
          (A (2) (1 2))
          (A (2 2) (10 20 30 40))))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((sλ ([x +inf.0]) (sλ ([y 0]) ((scalar +) x y)))
           (A (2) (1 2)))
          (A (2 2) (10 20 30 40)))))
  (apply-reduction-relation*
   ->Array
   (term ((sλ ([x +inf.0][y 0]) ((scalar +) x y))
          (A (2) (1 2))
          (A (2 2) (10 20 30 40))))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((sλ ([x +inf.0]) (sλ ([y +inf.0]) ((scalar +) x y)))
           (A (2) (1 2)))
          (A (2 2) (10 20 30 40)))))
  (apply-reduction-relation*
   ->Array
   (term ((sλ ([x +inf.0][y +inf.0]) ((scalar +) x y))
          (A (2) (1 2))
          (A (2 2) (10 20 30 40))))))
 
 ; make sure naturalization works on builtin ops too
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((A [2] [append append])
          (A [4] [2 3 4 5])
          (A [4] [1 10 100 1000]))))
  (term ((A [2 8] [2 3 4 5 1 10 100 1000 2 3 4 5 1 10 100 1000]))))
 
 
 ; reducing within a box
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (box ([scalar +] (A [3] [9 8 7]) [scalar 4]))))
  (term ((box (A [3] [13 12 11])))))
 
 ; η-expanded box
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ([scalar (λ [(x 0)] (box x))] ([scalar +] [scalar 3] [scalar 4]))))
  (term ((box [scalar 7]))))
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ([scalar (λ [(x 1)] (box x))]
          ([scalar +] (A [3] [9 8 7]) [scalar 4]))))
  (term ((box (A [3] [13 12 11])))))
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ([scalar (λ [(x +inf.0)] (box x))]
          ([scalar +] (A [3] [9 8 7]) [scalar 4]))))
  (term ((box (A [3] [13 12 11])))))
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ([scalar (λ [(x 0)] (box x))]
          ([scalar +] (A [3] [9 8 7]) [scalar 4]))))
  (term ((A [3] [(box [scalar 13]) (box [scalar 12]) (box [scalar 11])]))))
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ([scalar (λ [(x -1)] (box x))]
          ([scalar +] (A [3] [9 8 7]) [scalar 4]))))
  (term ((A [3] [(box [scalar 13]) (box [scalar 12]) (box [scalar 11])]))))
 
 ; compose
 (define Array-compose
   (term (λ [(f +inf.0) (g +inf.0)]
           [scalar (λ [(x +inf.0)] (f (g x)))])))
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (([scalar (λ [(f +inf.0) (g +inf.0)]
                     [scalar (λ [(x +inf.0)] (f (g x)))])]
           [scalar (λ [(n 0)] ([scalar +] [scalar 1] n))]
           [scalar (λ [(n 0)] ([scalar *] [scalar 2] n))])
          [scalar 4])))
  (term ([scalar 9]))))
