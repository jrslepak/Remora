#lang racket/base

(require "semantics.rkt"
         syntax/parse
         (for-syntax syntax/parse
                     (except-in racket/base apply box unbox)
                     (rename-in racket/base [apply racket-apply])
                     racket/list
                     racket/syntax))

(provide Rλ
         define-primop
         remora
         fn
         all
         alit
         array
         apply/shape
         :
         box
         unbox
         vec
         rerank
         def
         defstruct)




(begin-for-syntax
  (struct remora-macro (transformer))
  (define-syntax-class CONST
    #:description "Remora constant"
    #:literals (quote)
    (pattern bool:boolean)
    (pattern numlit:number)
    (pattern strlit:str)
    (pattern charlit:char)
    (pattern (quote sexp)))
  (define-syntax-class RANK
    #:description "Remora argument rank"
    #:literals (all)
    #;(pattern 'all)
    (pattern all)
    (pattern cell-rank:nat))
  (define-syntax-class ATOM
    #:description "Remora atom"
    #:literals (fn all)
    (pattern const:CONST)
    (pattern (fn ((var:id r:RANK) ...) body ...)))
  (define-syntax-class ALITERAL
    #:description "Remora array literal"
    #:literals (array alit)
    (pattern (array piece:ALITERAL ...))
    (pattern (alit (dim:nat ...) elt:ATOM ...))))


(define-syntax (Rλ stx)
  (syntax-parse stx
    [(_ ((var:id rank:RANK) ...) body ...+)
    #'(rem-proc (λ (var ...) body ...)
                 (list rank ...))]))
(define-syntax (define-primop stx)
  (syntax-parse stx
    [(_ (funname:id (var:id rank:RANK) ...) body ...+)
     #'(define funname
         (rem-array (vector) (vector (Rλ ((var rank) ...)
                                         body ...))))]))

(define-syntax (remora? stx)
  (syntax-parse stx
    [(_ const:CONST) #'"remora constant"]
    [(_ rank:RANK) #'"remora rank"]
    [(_ atom:ATOM) #'"remora atom"]
    [(_ aliteral:ALITERAL) #'"remora array literal"]
    [(_ otherwise) #'"not remora"]))

(define-syntax (remora-const? stx)
  (syntax-parse stx
    [(_ const:CONST) #'#t]
    [(_ otherwise) #'#f]))
(define-syntax (remora-rank? stx)
  (syntax-parse stx
    [(_ rank:RANK) #'#t]
    [(_ otherwise) #'#f]))
(define-syntax (remora-atom? stx)
  (syntax-parse stx
    [(_ atom:ATOM) #'#t]
    [(_ otherwise) #'#f]))
(define-syntax (remora-array-literal? stx)
  (syntax-parse stx
    [(_ aliteral:ALITERAL) #'#t]
    [(_ otherwise) #'#f]))

(define-syntax (define-remora-syntax stx)
  (syntax-parse stx
    [(_ (macro-name:id macro-arg:id) macro-defn:expr)
     #'(define-syntax macro-name
         (remora-macro
          (λ (macro-arg)
            macro-defn)))]))

;;; transform a Remora expression into Racket code
;;; remora macros must explicitly recur on subterms that should be Remora code
(define-syntax (remora stx)
  (syntax-parse stx
    #:literals (fn alit array apply apply/shape box unbox vec require provide)
    ;; require and provide apparently need to be recognized specially, as
    ;; redefining them breaks lots of things
    [(_ (require subterms ...)) #'(require subterms ...)]
    [(_ (provide subterms ...)) #'(provide subterms ...)]
    ;; a bare ATOM in EXP position is converted to a scalar containing that ATOM
    [(_ bare-atom:ATOM)
     (begin
       #;(displayln "converting bare atom to scalar")
       #'(rem-array (vector) (vector (remora-atom bare-atom))))]
    ;; check whether head is another Remora form (possibly a remora-macro)
    [(_ (head tail ...))
     #:declare head (static remora-macro? "remora macro")
     (begin
       #;(printf "expanding remora macro ~v\n" (syntax->datum #'head))
       ((remora-macro-transformer (syntax-local-value #'head))
        #'(head tail ...)))]
    ;; if not, this is function application
    [(_ (head tail ...))
     ((remora-macro-transformer (syntax-local-value #'apply))
      #'(apply head tail ...))]
    ;; identifiers get a dynamic check/coercion to convert Racket values
    ;; into Remora scalars
    [(_ var:id) #'(racket->remora var)]
    ;; multiple subterms are treated as having an implicit begin
    [(_ subterm ...) #'(begin (remora subterm) ...)]))
;;; transform a Remora atom into Racket code
(define-syntax (remora-atom stx)
  (syntax-parse stx
    #:literals (fn)
    [(_ const:CONST) #'const]
    [(_ (fn ((arg:id rank:RANK) ...) body:expr ...))
     ((remora-macro-transformer (syntax-local-value #'fn))
      #'(fn ((arg rank) ...) body ...))]
    [(_ otherwise)
     (error "could not handle atom:" #'otherwise)]))


(define-remora-syntax (fn stx)
  (syntax-parse stx
    [(_ ((var:id rank:RANK) ...) body ...+)
    #'(rem-proc (λ (var ...) (remora body) ...)
                 (list #;rank (syntax->rank-value rank) ...))]))
;;; Need to provide some definition for `all` in order to use it as a literal
(define-syntax all
  (syntax-id-rules ()
    [_ 'all]))
;;; Transform surface syntax for a rank into the behind-the-scenes value.
(define-syntax (syntax->rank-value stx)
  (syntax-parse stx
    [(_ finite-rank:nat) #'finite-rank]
    [(_ all) (syntax 'all)]))

;;; (alit (nat ...) atom ...)
;;;  (rem-array (vector nat ...) (vector atom ...))
;;; TODO: automated test
(define-remora-syntax (alit stx)
  (syntax-parse stx
    [(_ (dim:nat ...) elt:ATOM ...)
     #'(rem-array (vector dim ...)
                  (vector (remora-atom elt) ...))]))

;;; (apply expr1 expr2 ...)
;;;  (apply-rem-array expr expr ...)
;;; TODO: automated test
(define-remora-syntax (apply stx)
  (syntax-parse stx
    [(_ fun arg ...)
     #'(remora-apply (remora fun)
                     (remora arg) ...)]))

;;; (apply/shape expr0 expr1 expr2 ...)
;;;  (apply-rem-array (rem-array->vector expr0) expr expr ...)
;;; TODO: automated test
(define-remora-syntax (apply/shape stx)
  (syntax-parse stx
    [(_ shp fun arg ...)
     #'(remora-apply (remora fun)
                     #:result-shape (rem-array->vector (remora shp))
                     (remora arg) ...)]))
(define-remora-syntax (: stx)
  (syntax-parse stx
    [(_ shp fun arg ...)
     #'(remora-apply (remora fun)
                     #:result-shape (rem-array->vector (remora shp))
                     (remora arg) ...)]))
;;; or should the shape only get evaluated if it turns out to be needed?
;;; if it's by-need, will need to make it a thunk and have apply-rem-array
;;; force it whenn needed

;;; (box expr)
;;;  (rem-box expr)
;;; TODO: automated test
(define-remora-syntax (box stx)
  (syntax-parse stx
    [(_ contents) #'(rem-box (remora contents))]))

;;; (unbox var some-box expr)
;;;  (let ([var (rem-box-contents some-box)]) expr)
;;; TODO: automated test
(define-remora-syntax (unbox stx)
  (syntax-parse stx
    [(_ var:id some-box body)
     #'(let ([var (rem-box-contents (remora some-box))]) (remora body))]))

;;; (vec expr ...)
;;;  (build-vec expr ...)
;;; TODO: automated test
(define-remora-syntax (vec stx)
  (syntax-parse stx
    [(_ piece ...) #'(build-vec (remora piece) ...)]))

;;; (array . array-literals)
;;; "smart constructor"
;;; if all exps are (alit ...) with same shape, gather them into one alit form
;;; disallow (alit ...)s with mismatching forms
;;; otherwise, (apply build-array exps)
;;; TODO: automated test
(define-remora-syntax (array stx)
  (syntax-parse stx
    #:literals (alit)
    [(_ (alit (dim:nat ...) elt:ATOM ...) ...+)
     #:when (all-equal? (syntax->datum (syntax ((dim ...) ...))))
     #:with (old-dims ...) (first (syntax-e #'((dim ...) ...)))
     #:with outer-dim (datum->syntax
                       stx
                       (length (syntax->datum #'((elt ...) ...))))
     #:with (joined-elts ...) (datum->syntax
                               stx
                               (racket-apply append
                                             (syntax->datum #'((elt ...) ...))))
     #'(alit (outer-dim old-dims ...) joined-elts ...)]
    [(_ (alit (dim:nat ...) elt:ATOM ...) ...+)
     #:when (not (all-equal? (syntax->datum (syntax ((dim ...) ...)))))
     (error "array literal components with mismatched shapes at\n"
            (syntax-source stx)
            (syntax-line stx)
            (syntax-column stx))]
    [(_ piece ...)
     #'(remora (vec piece ...))]))
(begin-for-syntax
  (define (all-equal? xs)
    (cond [(<= (length xs) 1) #t]
          [else (and (equal? (first xs) (second xs))
                     (all-equal? (rest xs)))])))

;;; (def name defn-or-expr ... expr)
(define-remora-syntax (def stx)
  (syntax-parse stx
    [(_ (funname:id (var:id rank:RANK) ...) body ...+)
     #'(remora (def funname (fn ((var rank) ...) body ...)))]
    [(_ name:id defn-or-expr )
     #'(define name (remora defn-or-expr) )]))

;;; (struct name (field ...+))
(define-remora-syntax (defstruct stx)
  (syntax-parse stx
    [(_ structname:id (fieldname:id ...+))
     #'(struct structname (fieldname ...) #:transparent)]))

#;
(define-remora-syntax (require stx)
  (syntax-parse stx
    [(_ subterm ...) #'(require subterm ...)]))
#;
(define-remora-syntax (provide stx)
  (syntax-parse stx
    [(_ subterm ...) #'(provide subterm ...)]))


;;; sugar for reranking by eta-expansion
;;; operates on function arrays (as names only stand for arrays, not functions),
;;; but constructs a scalar
(define-remora-syntax (rerank stx)
  (syntax-parse stx
    [(_ (new-rank:RANK ...) original-function)
     #:with (param ...) (for/list ([i (length (syntax-e #'(new-rank ...)))])
                          (generate-temporary))
     #'(remora
        (alit () (fn ((param new-rank) ...)
                     (original-function param ...))))]))
