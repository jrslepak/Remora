#lang racket

(require rackunit
         redex)

(define-language Arrays
  (expr (expr expr ...)
        fun
        arr
        base
        var)
  
  ; functions may be builtin or user-defined
  (fun (λ [(var num) ...] expr)
       op
       c-op)
  
  ; syntax for writing arrays
  ; also include nested vector representation?
  (arr (A (num ...) (expr ...))) ; flat representation, shape and values
  
  (arr/pv (A (num ...) (elt ...))) ; pseudo-value form -- no app forms inside
  (elt arr/pv val base)
  (arr/v arr/b arr/f) ; value form -- only base data or functions inside
  (arr/b (A (num ...) (base ...)))
  (arr/f (A (num ...) (fun ...)))
  
  ; builtin operators
  (op + - * /)
  ; curried builtins
  (c-op reduce
        fold/l
        fold/r)
  
  ; variables
  (var variable-not-otherwise-mentioned)
  
  ; base data types: numbers and booleans
  (base num bool)
  (num number)
  (bool #t #f)
  
  ; value forms
  (val (λ [(var num) ...] expr)
       op
       arr/v)
  
  ; evaluation contexts
  (E hole
     (E expr ...)
     (val val ... E expr ...)
     ;(A (E expr ...) (expr ...))
     ;(A (num ... E expr ...) (expr ...))
     (A (num ...) (E expr ...))
     (A (num ...) (val ... E expr ...))))


(define ->Array
  (reduction-relation
   Arrays
   #:domain expr
   [--> (in-hole E ((reduce fun) arr/pv))
        (in-hole E (tree-apply fun (arr/pv_cell ...)))
        (where (arr/pv_cell ...) (cells/rank -1 arr/pv))
        reduce]
   [--> (in-hole E ((fold/r fun arr/v) arr/pv))
        (in-hole E (chain-apply/r fun (arr/pv_cell ... arr/v)))
        (where (arr/pv_cell ...) (cells/rank -1 arr/pv))
        fold/r]
   [--> (in-hole E ((fold/l fun arr/v) arr/pv))
        (in-hole E (chain-apply/l fun (arr/v arr/pv_cell ...)))
        (where (arr/pv_cell ...) (cells/rank -1 arr/pv))
        fold/l]
   [--> (in-hole E (op arr ...))
        (in-hole E (apply-op op (arr ...)))
        (side-condition (equal? (term (fun-rank op))
                                (term  ((rank arr) ...))))
        op]
   [--> (in-hole E ((λ [(var num) ...] expr) arr/v ...))
        (in-hole E (subst [(var arr/v) ...] expr))
        (side-condition (term (all ((at-rank? num arr/v) ...))))
        apply]
   [--> (in-hole E (arr/f arr/v ...))
        (in-hole E 
                 (A (shape arr/f) ((fun arr/v_cell ...) ...)))
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
        (where ((arr/v_cell ...) ...)
               (transpose ((cells/rank natural_funrank arr/v) ...)))
        arr/f-map]
   [--> (in-hole E (arr/f arr/v ...))
        (in-hole E (arr/f_natrank arr/v ...))
        (where (A (num_dim ...) (fun ...)) arr/f)
        
        (side-condition (not (redex-match Arrays ((natural ...) ...)
                                          (term ((fun-rank fun) ...)))))
        (where (fun_natrank ...) ((naturalize-rank fun arr/v ...) ...))
        (where arr/f_natrank (A (num_dim ...) (fun_natrank ...)))
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
   [--> (in-hole E (fun arr/v ...))
        (in-hole E (array-map fun (arr/v ...)))
        (where (natural_rank ...) (fun-rank fun))
        ; arrays must all be overranked and by the same (nonzero) amount
        (side-condition (term (all ((overrank? natural_rank arr/v) ...))))
        (side-condition (term (same-overrank? [(natural_rank arr/v) ...])))
        (side-condition (< 0 (length (term (arr/v ...)))))
        ; require that mapping produce an array rather than #f (map error)
        (side-condition (term (array-map fun (arr/v ...))))
        map]
   [--> (in-hole E (fun arr/v ...))
        (in-hole E (fun arr/v_lifted ...))
        ; function must have natural rank
        (where (natural_rank ...) (fun-rank fun))
        ; arrays must be overranked by different amounts
        (side-condition (not (term (same-overrank?
                                    [(natural_rank arr/v) ...]))))
        (where (arr/v_lifted ...) (frame-lift ([natural_rank arr/v] ...)))
        (side-condition (< 0 (length (term (arr/v ...)))))
        lift]
   ; convert function with non-natural rank to natural rank (based on its args)
   [--> (in-hole E (fun arr/v ...))
        (in-hole E (fun_natrank arr/v ...))
        (side-condition (not (andmap exact-nonnegative-integer?
                                     (term (fun-rank fun)))))
        (where fun_natrank (naturalize-rank fun arr/v ...))
        naturalize]
   ; collapse an array-of-arrays into flat array
   [--> (in-hole E (A (num_frame-dim ...) (arr/pv ...)))
        ; append cell shape onto frame shape
        (in-hole E (A (num_frame-dim ... num_cell-dim ...) any_v))
        ; TODO: tighten this pattern condition?
        (where any_v ,(foldr append '() (term ((value arr/pv) ...))))
        ; all cells must have the same shape
        (where (num_cell-dim ...) (shape-of-all arr/pv ...))
        (where ((A (num ...) (base ...)) ...) (arr/pv ...))
        collapse]))




;; macro to convert number to rank-0 array
(define-metafunction Arrays
  scalar : num -> arr
  [(scalar num) (A () (num))])


;; apply a function to a chain of arrays, i.e.
;; arr_0 `fun` arr_1 `fun` arr_2 `fun` arr_3 `fun` ... `fun` arr_n
(define-metafunction Arrays
  chain-apply/r : fun (arr ...) -> expr
  [(chain-apply/r fun ()) (id-element/r fun)]
  [(chain-apply/r fun (arr)) arr]
  [(chain-apply/r fun (arr_0 arr_1 ...))
   (fun arr_0 (chain-apply/r fun (arr_1 ...)))])
(define-metafunction Arrays
  chain-apply/l : fun (arr ...) -> expr
  [(chain-apply/l fun ()) (id-element/l fun)]
  [(chain-apply/l fun (arr)) arr]
  [(chain-apply/l fun (arr_0 ... arr_1))
   (fun (chain-apply/l fun (arr_0 ...)) arr_1)])
;; similar to chain-apply but for tree-shaped application to the arrays
(define-metafunction Arrays
  tree-apply : fun (arr ...) -> any
  [(tree-apply fun (arr)) arr]
  [(tree-apply fun (arr_0 arr_1)) (fun arr_0 arr_1)]
  [(tree-apply fun (arr ...))
   #;((arr_0 ...) || (arr_1 ...))
   (fun (tree-apply fun (arr_0 ...))
        (tree-apply fun (arr_1 ...)))
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
  naturalize-rank : fun arr ... -> fun
  [(naturalize-rank (λ [(var num) ...] expr) arr ...)
   (λ [(var natural) ...] expr)
   (where (natural ...) ((natural-cell-rank num arr) ...))])
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
   (A () (,(/ (term num_1) (term num_2))))])

;; extract or look up ranks of a function
(define-metafunction Arrays
  fun-rank : fun -> (num ...)
  [(fun-rank +) (0 0)]
  [(fun-rank -) (0 0)]
  [(fun-rank *) (0 0)]
  [(fun-rank /) (0 0)]
  [(fun-rank (λ [(var num) ...] expr)) (num ...)]
  [(fun-rank reduce) +inf.0]
  [(fun-rank (reduce expr)) +inf.0])

;; capture-avoiding substitution
(define-metafunction Arrays
  subst : [(var expr) ...] expr -> expr
  [(subst [(var expr) ...] base) base]
  [(subst [(var expr) ...] op) op]
  [(subst [(var expr) ...] c-op) c-op]
  [(subst [(var expr) ...] (A (num_sh ...) (expr_val ...)))
   (A (num_sh ...) ((subst [(var expr) ...] expr_val) ...))]
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
  [(well-shaped (A (num_s ...) (expr_v ...)))
   ,(= (foldr * 1 (term (num_s ...)))
       (length (term (expr_v ...))))])

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
  [(rank (A (num ...) (expr ...)))
   ,(length (term (num ...)))])

;; extract shape of array
(define-metafunction Arrays
  shape : arr -> (num ...)
  [(shape (A (num ...) (expr ...))) (num ...)])

;; extract value of array
(define-metafunction Arrays
  value : arr -> (expr ...)
  [(value (A (num ...) (expr ...))) (expr ...)])

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
                             (expr_cell ...)) ...))))
   ; break the array's value segment into its cells
   (where ((expr_cell ...) ...)
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
  cell-values : (num ...) arr -> ((expr ...) ...)
  [(cell-values (num_cellshape ...) arr)
   ((expr ...) ...)
   (where ((A (num ...) (expr ...)) ...)
          (cells/shape (num_cellshape ...) arr))])

;; split an array into cells
(define-metafunction Arrays
  ; cell shape, array
  cells/shape : (num ...) arr -> (arr ...)
  [(cells/shape (num_cell-dim ...) (A (num_arr-dim ...) ())) ()]
  [(cells/shape (num_cell-dim ...) (A (num_arr-dim ...) (expr ...)))
   ,(cons (term (A (num_cell-dim ...) (take/m (expr ...) num_cellsize)))
          ; drop one cell's elements from array, and split remaining elements
          (term (cells/shape (num_cell-dim ...)
                       (A (num_arr-dim ...)
                          (drop/m (expr ...) num_cellsize)))))
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
  [(frame-shapes [(num_cellrank (A (num_shape ...) (expr ...))) ...])
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
   (term (+ (A (3 3) (1 2 3
                      4 5 6
                      7 8 9))
            (A (3) (10 20 30)))))
  (term ((A (3 3) (11 12 13
                   24 25 26
                   37 38 39)))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((λ ([x 1][y 1]) (+ x y)) (A (3 3) (1 2 3
                                             4 5 6
                                             7 8 9))
                                   (A (3) (10 20 30)))))
  (term ((A (3 3) (11 22 33
                   14 25 36
                   17 28 39)))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((λ ([x -1][y 1]) (+ x y)) (A (3 3) (1 2 3
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
   (term ((reduce +)
          (A (3 3) (1 2 3
                    4 5 6
                    7 8 9)))))
  (term ((A (3) (12 15 18)))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((λ ([x 1]) ((reduce +) x))
          (A (3 3) (1 2 3
                    4 5 6
                    7 8 9)))))
  (term ((A (3) (6 15 24)))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((fold/r - (scalar 0))
          (A (4) (1 1 1 1)))))
  (term ((A () (0)))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((fold/l - (scalar 0))
          (A (4) (1 1 1 1)))))
  (term ((A () (-4)))))
 
 ; some terms which should not be reducible (due to shape mismatch)
 (check-equal?
  (apply-reduction-relation
   ->Array
   (term (+ (A (6) (1 2 3 4 5 6))
            (A (3) (10 20 30)))))
  '())
 
 (check-equal?
  (apply-reduction-relation
   ->Array
   (term (+ (A (2 3) (1 2 3
                      4 5 6))
            (A (3) (10 20 30)))))
  '())
 
 ; make sure currying doesn't change how lifting works
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((λ ([x 0]) (λ ([y 0]) (+ x y))) (A () (5))) (A () (10)))))
  (apply-reduction-relation*
   ->Array
   (term ((λ ([x 0] [y 0]) (+ x y)) (A () (5)) (A () (10))))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((λ ([x 0]) (λ ([y 0]) (+ x y)))
           (A () (5)))
          (A (2) (10 20)))))
  (apply-reduction-relation*
   ->Array
   (term ((λ ([x 0] [y 0]) (+ x y)) (A () (5)) (A (2) (10 20))))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((λ ([x 0]) (λ ([y 0]) (+ x y)))
           (A (2 2) (1 2
                     3 4)))
          (A (2) (10 20)))))
  (apply-reduction-relation*
   ->Array
   (term ((λ ([x 0] [y 0]) (+ x y))
          (A (2 2) (1 2
                    3 4))
          (A (2) (10 20))))))
 
 ; check that arrays of functions with non-natural rank get applied properly
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((A (2) ((λ ([x +inf.0]) (* (scalar 10) x))
                  (λ ([x +inf.0]) (- x (scalar 5)))))
          (A (4) (1 2 3 4)))))
  (term ((A (2 4) (10 20 30 40
                   -4 -3 -2 -1)))))
 
 ; currying with non-natural rank
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((λ ([x 0]) (λ ([y -1]) (+ x y)))
           (A (2) (1 2)))
          (A (2 2) (10 20 30 40)))))
  (apply-reduction-relation*
   ->Array
   (term ((λ ([x 0][y -1]) (+ x y))
          (A (2) (1 2))
          (A (2 2) (10 20 30 40))))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((λ ([x -1]) (λ ([y 0]) (+ x y)))
           (A (2) (1 2)))
          (A (2 2) (10 20 30 40)))))
  (apply-reduction-relation*
   ->Array
   (term ((λ ([x -1][y 0]) (+ x y))
          (A (2) (1 2))
          (A (2 2) (10 20 30 40))))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((λ ([x 0]) (λ ([y +inf.0]) (+ x y)))
           (A (2) (1 2)))
          (A (2 2) (10 20 30 40)))))
  (apply-reduction-relation*
   ->Array
   (term ((λ ([x 0][y +inf.0]) (+ x y))
          (A (2) (1 2))
          (A (2 2) (10 20 30 40))))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((λ ([x -1]) (λ ([y +inf.0]) (+ x y)))
           (A (2) (1 2)))
          (A (2 2) (10 20 30 40)))))
  (apply-reduction-relation*
   ->Array
   (term ((λ ([x -1][y +inf.0]) (+ x y))
          (A (2) (1 2))
          (A (2 2) (10 20 30 40))))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((λ ([x +inf.0]) (λ ([y 0]) (+ x y)))
           (A (2) (1 2)))
          (A (2 2) (10 20 30 40)))))
  (apply-reduction-relation*
   ->Array
   (term ((λ ([x +inf.0][y 0]) (+ x y))
          (A (2) (1 2))
          (A (2 2) (10 20 30 40))))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (((λ ([x +inf.0]) (λ ([y +inf.0]) (+ x y)))
           (A (2) (1 2)))
          (A (2 2) (10 20 30 40)))))
  (apply-reduction-relation*
   ->Array
   (term ((λ ([x +inf.0][y +inf.0]) (+ x y))
          (A (2) (1 2))
          (A (2 2) (10 20 30 40)))))))

