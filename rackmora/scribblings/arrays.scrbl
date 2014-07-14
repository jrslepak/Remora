#lang scribble/manual
@;@require[(for-label rackmora)]
@title{Arrays}

All computation in array-oriented programming is performed on arrays. An array
is a collection of ``atoms'' arranged in a rectangular, or ``regular'', layout.
Atoms are basic data such as numbers, strings, and booleans.

An array can have any natural number of axes. This number is called the array's
``rank.'' A vector extends along only one axis, so it is a rank 1 array. A
matrix has two axes, making it a rank 2 array. This rule generalizes to
higher-rank structures, though they do not have common names. The degenerate
case of an array has zero axes---a scalar has rank 0 (note: a scalar contains
one atom, but an atom is not itself a scalar).

The ``shape'' of an array is a sequence whose elements gives the array's size
along each axis. For example, a 4 × 7 matrix (4 rows, 7 columns) has shape
@code[#:lang "rackmora"]{[4 7]}, whereas an individual row in that matrix has
shape @code[#:lang "rackmora"]{[7]}. Five such matrices could be collected into
an array with shape @code[#:lang "rackmora"]{[5 4 7]}. Regularity requires that
each plane in this 5 × 4 × 7 array have four rows and that each of those rows
have seven entries. A scalar (recall, rank 0 array) must have the empty
sequence @code[#:lang "rackmora"]{[]} as its shape.

The product of an array's dimensions gives the number of ``atoms'' the array
contains. Our 4 × 7 matrix contains 28 atoms. For a scalar, this product is
1, the empty product, so a scalar contains one atom.

Expressions in @racket{#lang rackmora} produce arrays, and variables can only
be bound to arrays. As a syntactic convenience, atoms in expression position
are converted to scalar array literals.


@defform[(alit (natural ...) atom ...)]{
Array literal: Produces an array of the given atoms arranged according
to the given shape, @racket[[natural ...]].

The 2 × 3 matrix whose rows are @code[#:lang "rackmora"]{[1 0 2]} and
@code[#:lang "rackmora"]{[3 2 1]} can be described with the array literal
@racket[(alit (3 2) 1 0 2 3 2 1)].

@;{TODO: variables are rendering weird in 'code' fragments}
In @tt{#lang rackmora}, @code[#:lang "rackmora"]{#A(dimension ...)(atom ...)}
is read as @racket[(alit (dimension ...) atom ...)], so the above example can
be written @code[#:lang "rackmora"]{#A(3 2)(1 0 2 3 2 1)}.
}

@defform[(vec expr ...+)]{
Array constructor: All @racket[expr]s must evaluate to arrays with the same
shape. The number of fragments given is prepended to the fragments' shape to
form the resulting array's shape. The resulting array contains the concatenated
contents of the given fragments. This form is more flexible than @racket[alit]
as atoms which appear in expression position are automatically promoted to
scalars.

@racket[(vec (alit (3) 1 0 2) (alit (3) 3 2 1))] and
@racket[(vec (vec 1 0 2) (vec 3 2 1))] both produce the same 2 × 3
matrix as @racket[(alit (3 2) 1 0 2 3 2 1)].
}

@defform[(array fragment ...+)]{
"Smart" constructor: If the fragments are atoms, this form becomes a rank 1
@racket[alit] form whose sole dimension is the number of atoms given. If the
fragments are non-atoms, this form expands to a @racket[vec] form.

In @tt{#lang rackmora}, @tt{[expr ...]} is read as @racket[(array expr ...)]
}

@defform[(def id expr)]{
Definition form: binds @racket[id] to the result of @code{expr}. Recall that
expressions produce arrays, so @code{(def x 3)} binds @code{x} to the scalar
@code[#:lang "rackmora"]{#A()(3)}, not the atom 3. Therefore
@code[#:lang "rackmora"]{[x x]} produces @code[#:lang "rackmora"]{#A(2)(3 3)},
and @code[#:lang "rackmora"]{#A(2)(x x)} is not permitted.
}
