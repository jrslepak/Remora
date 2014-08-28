#lang scribble/manual
@require[(for-label (except-in racket/base
                               box unbox)
                    remora/dynamic/main)]

@defmodulelang[remora/dynamic]
@title{Function Application}

A function application form applies an array of functions to arrays of
arguments. All atoms in the function array must be functions with the same arity
and expected ranks. The expected rank for each argument array determines how the
array will be split into a ``frame'' of ``cells.'' The cells are the sub-arrays
whose rank is that expected rank. The frame is the array structure around those
cells.

@section{Example: Scalar-vector addition}

Consider the expression
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
(+ 1 [10 20 30])
}

@code[#:lang "remora/dynamic"]{+} expects rank 0 arguments.
The first argument has rank 0, so it is a single cell in a scalar frame ---
a frame with shape @code[#:lang "remora/dynamic"]{[]}.
The second argument has rank 1, so we split it into three cells,
@code[#:lang "remora/dynamic"]{10}, @code[#:lang "remora/dynamic"]{20}, and
@code[#:lang "remora/dynamic"]{30}.
These three cells exist in a frame with shape
@code[#:lang "remora/dynamic"]{[3]}.

The function array itself also splits into a frame of cells, with an expected
cell rank of 0.
In this case, we have a single function cell in a scalar frame.

In order to evaluate this application, the function and argument frames must be
brought into agreement.
Semantically, this is done by replicating cells and appending additional
dimensions.
This can only succeed if every frame is a prefix of some particular frame, which
we call the ``principal frame.''
This rule is known as ``prefix agreement.''
Here, the principal frame is @code[#:lang "remora/dynamic"]{[3]}, from the
@code[#:lang "remora/dynamic"]{[10 20 30]} argument.
The other frames (from the function and first argument) are both
@code[#:lang "remora/dynamic"]{[]}, which is a prefix of any frame.

In our example, the single function cell is replicated to form the array
@code[#:lang "remora/dynamic"]{[+ + +]},
and the single cell of the first argument is similarly replicated to form
@code[#:lang "remora/dynamic"]{[1 1 1]}.

We then have a new expression where all frames are equal:
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
([+ + +] [1 1 1] [10 20 30])
}

Evaluation proceeds with cell-wise application:
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
[(+ 1 10) (+ 1 20) (+ 1 30)]
}

Computing each result cell gives us our final result:
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
[11 21 31]
}

@section{Example: Vector-matrix addition}
The frames in this application form also follow prefix agreement:
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
(+ [[1 2] [3 4] [5 6]] [10 20 30])
}

Here, the frames are @code[#:lang "remora/dynamic"]{[]},
@code[#:lang "remora/dynamic"]{[3 2]}, and @code[#:lang "remora/dynamic"]{[3]}.
It may be easier to see how the vector's cells replicate using array literal
notation. We require two copies of each scalar cell of
@code[#:lang "remora/dynamic"]{#A(3)(10 20 30)}, which turns it into
@code[#:lang "remora/dynamic"]{#A(3 2)(10 10 20 20 30 30)}, or
@code[#:lang "remora/dynamic"]{[[10 10] [20 20] [30 30]]}.

This leads to a result of
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
[[11 12] [23 24] [35 36]]
}
which may be more easily read as
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
[[11 12]
 [23 24]
 [35 36]]
}
We have added a column vector to the matrix.

@section{Example: Function with rank 1 arguments}

The first argument to the @code[#:lang "remora/dynamic"]{base} function is a
vector of numbers describing a place value interpretation.
At each position in the vector is a number indicating how many distinct
``digits'' are allowed at the corresponding column.
The second argument is a vector of these digits, to be interpreted according to
the place values specified by the first.
For example, converting 1 hour, 40 minutes, 15 seconds to the number of seconds:
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
(base [24 60 60] [1 40 15])
}

The expected rank for both arguments is 1.
This leads to different lifting behavior than @code[#:lang "remora/dynamic"]{+}:
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
(base [24 60 60] [[1 40 15] [3 8 10]])
}

The argument shapes are @code[#:lang "remora/dynamic"]{[3]} and
@code[#:lang "remora/dynamic"]{[2 3]}. Since the expected rank for both
arguments is 1, the last 1 dimension of each argument's shape forms the cell
shape:
@code[#:lang "remora/dynamic"]{[3]} and @code[#:lang "remora/dynamic"]{[3]}.
The pieces that remain, @code[#:lang "remora/dynamic"]{[]} and
@code[#:lang "remora/dynamic"]{[2]}, are the frame shapes.
The cell to replicate here is the vector
@code[#:lang "remora/dynamic"]{[24 60 60]},
not the individual scalars, @code[#:lang "remora/dynamic"]{[24]},
@code[#:lang "remora/dynamic"]{[60]}, and @code[#:lang "remora/dynamic"]{[60]}.
So we expand the first argument to the 2 × 3 array
@code[#:lang "remora/dynamic"]{[[24 60 60] [24 60 60]]}.

Notice that in both vector-matrix examples, the new dimension appears at the end
of the frame portion of the shape.


@section{Example: Applying multiple functions}

Sometimes it is convenient to split a complex number into its polar
representation. We can build an array containing the magnitude and phase angle
by applying an array containing the appropriate functions:
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
([magnitude angle] 1+2i)
}

The function array's frame is @code[#:lang "remora/dynamic"]{[2]}, and the
argument's frame is @code[#:lang "remora/dynamic"]{[]}.
Expanding the argument gives
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
([magnitude angle] [1+2i 1+2i])
}

Cell-wise evaluation leaves the two functions' results in the
@code[#:lang "remora/dynamic"]{[2]} frame:
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
[2.23606797749979 1.1071487177940904]
}

@section{Reranking}

Suppose we want our vector-matrix addition to treat the vector as a row instead
of as a column.
Recall that when an array in an application form is expanded, the new dimensions
appear at the end of the frame portion of its shape.
If we want to use the vector as a row, the new dimension must appear in position
0 instead of position 1.
So what we want is a version of @code[#:lang "remora/dynamic"]{+} which expects
rank 1 arguments instead of rank 0.
We can write such a function:
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
(fn ((x 1) (y 1)) (+ x y))
}

This is common enough to warrant its own syntactic sugar:

@defform[(rerank (rank ...) expr)]{
Wrap a function in a function of different rank.
Equivalent to @code[#:lang "remora/dynamic"]{(fn ((x rank) ...) (expr x ...))}
with fresh variables @code[#:lang "remora/dynamic"]{x ...}.
This causes the function produced by evaluating
@code[#:lang "remora/dynamic"]{expr} to be lifted using the expected argument
ranks @code[#:lang "remora/dynamic"]{rank ...} rather than its own.

In @tt{#lang remora/dynamic}, @code[#:lang "remora/dynamic"]{#r(rank ...)expr}
is read as @racket[(rerank (rank ...) expr)].
}

Returning to vector-matrix addition, @code[#:lang "remora/dynamic"]{#r(1 1)+}
operates on vector cells.
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
(#r(1 1)+ [[1 2] [3 4] [5 6]] [10 20])
}

The first argument is a @code[#:lang "remora/dynamic"]{[3]} frame around cells
of shape @code[#:lang "remora/dynamic"]{[2]},
and the second is a @code[#:lang "remora/dynamic"]{[]} frame, also around cells
of shape @code[#:lang "remora/dynamic"]{[2]}.
The second argument expands into a @code[#:lang "remora/dynamic"]{[3]} frame by
replicating its vector cell, becoming
@code[#:lang "remora/dynamic"]{[[10 20] [10 20] [10 20]]}.
So we have 
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
(#r(1 1)+ [[1 2] [3 4] [5 6]] [[10 20] [10 20] [10 20]])
}
which evaluates to 
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
[[11 22] [13 24] [15 26]]
}


@;{TODO: reranking can also "reset" a function array's rank, like in
(#r(0)[magnitude angle] [1+i 3 0-i])
}

@;{TODO: apply/shape}
