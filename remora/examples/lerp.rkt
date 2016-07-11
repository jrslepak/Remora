#lang remora/dynamic

(provide lerp)

;;; Defining and using a function

;;; find the number part way between two boundaries
(def (lerp (lo 0) (hi 0) (α 0))
  (+ (* α hi)
     (* (- 1 α) lo)))

;;; three fifths of the way from -1 to 1
(lerp -1 1 3/5)

;;; try several "middle" points
(lerp -1 1 [0 1/3 2/3 1])

;;; take the midpoint along multiple axes
(lerp [0 1] [7 4] 0.5)

;;; find midpoints of three lines, formed by three low (x,y,z) points
;;; and one high (x,y,z) point
;;; reranking to 1,1,0 means to treat (x,y,z) coordinate (a rank-1
;;; structure) as the fundamental unit in lifting lerp
(#r(1 1 0)lerp
   [[0 0 2] [0 1 1] [1 0 0]]
   [7 4 5]
   0.5)

