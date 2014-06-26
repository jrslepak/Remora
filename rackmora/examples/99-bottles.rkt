#lang rackmora
(def countdown (+ 1 (R_reverse (R_unsafe-unbox (R_iota [99])))))
(display
 (R_foldr
  string-append
  ""
  (format "~v bottles of beer on the wall, ~v bottles of beer.\nTake one down, pass it around, ~v bottles of beer on the wall.\n\n"
          countdown countdown (sub1 countdown))))
