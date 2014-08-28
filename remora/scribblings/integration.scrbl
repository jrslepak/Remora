#lang scribble/manual
@require[(for-label (except-in racket/base
                               box unbox)
                    remora/dynamic/main)
         scribble/eval]
@;@declare-exporting[remora/dynamic]
@title{Integration with Racket code}

@section{Importing and exporting}

@code[]{#lang remora/dynamic} includes Racket's @racket[require] and
@racket[provide] to allow programs to be split into modules and use of
pre-existing Racket procedures.
When a Racket procedure is applied in Remora, it is automatically converted
into a Remora procedure with arity matching the supplied number of arguments
and an expected rank of 0 for all arguments.
For example, Racket's built-in @racket[gcd] can be applied to Remora arrays:
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
(gcd 15 [10 11 12])
}



@section{Using Remora as a Library}
@defmodule[remora/dynamic/main]
A Racket program can embed small pieces of Remora code.
Primitive operations (@italic{i.e.}, those provided by the
@racket[remora/dynamic] library) have ``@code[]{R_}'' prepended to their names
to avoid conflict with Racket's own built-in procedures.

@defform[(remora expr ...)]{
Evaluate each @racket[expr] in turn as a @code[]{#lang remora/dynamic}
expression, returning the result of the last one.
Within a @racket[remora] form, only @code[]{remora/dynamic} syntax can be used,
though the reader extensions (@code[#:lang "remora/dynamic"]{#A},
@code[#:lang "remora/dynamic"]{#r}, and bracketed arrays) are not available.
The result of a @racket[remora] form is a @racket[rem-array] struct.
}

@defstruct[rem-array ([shape (vectorof exact-nonnegative-integer?)]
                      [data vector?])
                     #:transparent]{
The array's dimensions are stored as the @racket[shape] field, and the atoms are
stored as the @racket[data] field.
The constructor is protected by a dependent contract which requires that the
number of atoms be equal to the product of the dimensions.
}

Procedures can also be constructed directly.
@defstruct[rem-proc ([body procedure?]
                     [ranks (listof (or/c exact-nonnegative-integer? 'all))])
                     #:transparent]{
Wrap the given Racket procedure as a @code[]{remora/dynamic} procedure with the
given expected ranks.
The @racket[body] must be a procedure which consumes and produces
Remora arrays.
}

@interaction[
(require remora/dynamic/main)
(define elts-sum
  (rem-proc
   (λ (arr)
     (rem-array #()
                (vector (for/sum ([i (rem-array-data arr)]) i))))
   '(all)))
(remora
 (elts-sum (alit (2 3) 1 2 3 4 5 6)))
]
