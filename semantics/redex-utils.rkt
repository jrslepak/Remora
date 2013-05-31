#lang racket

(require redex)
(provide deterministic-reduce)

(define (deterministic-reduce reln expr)
  (define next-exprs
    ; redex-match sometimes produces multiple copies of the same matching,
    ; leading to multiple copies of the same reduction result
    (remove-duplicates
     (apply-reduction-relation/tag-with-names reln expr)))
  (cond [(= 0 (length next-exprs)) expr] ; no results -- term is fully reduced
        [(= 1 (length next-exprs)) ; one result -- continue reducing
         (deterministic-reduce reln
                               ; extract the first term from the results list
                               (second (first next-exprs)))]
        ; multiple results -- raise error
        [else (error 'deterministic-reduce
                     "\n~v steps to any of\n~v"
                     expr
                     next-exprs)]))
