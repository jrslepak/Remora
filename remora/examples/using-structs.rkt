#lang remora/dynamic

(struct book (title pages isbn))
(define SHELF-CONTENTS
  (book ["Types and Programming Languages"
         "Principles of Program Analysis"
         "Compiling with Continuations"
         "A Theory of Objects"]
        [623 452 260 396]
        ["978-0-262-16209-8"
         "978-3-642-08474-4"
         "0-521-03311-X"
         "0-387-94775-2"]))

(define LONG-BOOK-ISBNS
  (book-isbn (filter (< 400 (book-pages SHELF-CONTENTS)) SHELF-CONTENTS)))

(struct stanley-cup (year city name))
(define last-four
  (stanley-cup [2015 2016 2017 2018]
               ["Chicago" "Pittsburgh" "Pittsburgh" "Washington"]
               ["Blackhawks" "Penguins" "Penguins" "Capitals"]))



