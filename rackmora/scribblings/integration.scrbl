#lang scribble/manual
@require[(for-label (except-in racket/base
                               box unbox)
                    rackmora)]
@;@declare-exporting[rackmora]
@title{Integration with Racket code}

@section{Importing and exporting}

@code[]{#lang rackmora} includes Racket's @racket[require] and
@racket[provide] to allow programs to be split into modules and use of
pre-existing Racket procedures.
When a Racket procedure is applied in rackmora, it is automatically converted
into a rackmora procedure with arity matching the supplied number of arguments
and an expected rank of 0 for all arguments.
For example, Racket's built-in @racket[gcd] can be applied to rackmora arrays:
@codeblock[#:keep-lang-line? #f]{
#lang rackmora
(gcd 15 [10 11 12])
}



@section{Using rackmora as a Library}
@defmodule[rackmora]
A Racket program can embed small pieces of rackmora code.
Primitive operations (@italic{i.e.}, those provided by the @racket[rackmora]
library) have ``@code[]{R_}'' prepended to their names to avoid conflict with
Racket's own built-in procedures.

@defform[(remora expr ...)]{
Evaluate each @racket[expr] in turn as a @code[]{#lang rackmora} expression,
returning the result of the last one.
Within a @racket[remora] form, only @code[]{rackmora} syntax can be used,
though the reader extensions (@code[#:lang "rackmora"]{#A},
@code[#:lang "rackmora"]{#r}, and bracketed arrays) are not available.
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
