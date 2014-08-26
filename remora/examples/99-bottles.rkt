#lang remora/dynamic
(def countdown (+ 1 (reverse (unsafe-unbox (iota [99])))))
(display
 (foldr
  string-append
  ""
  (format "~v bottles of beer on the wall, ~v bottles of beer.\nTake one down, pass it around, ~v bottles of beer on the wall.\n\n"
          countdown countdown (sub1 countdown))))
