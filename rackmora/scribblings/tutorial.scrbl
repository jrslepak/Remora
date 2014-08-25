#lang scribble/manual
@require[scribble/eval
         scribble/core
         racket/sandbox
         rackmora/lang/reader
         (for-label rackmora/lang/language
                    #;rackmora)]
@defmodulelang[rackmora]

@title[#:version "" #:date ""]{Remora Tutorial}

@(sandbox-output 'string)
@(printf "sandbox-output is now ~v\n" (sandbox-output))
@(define example-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'rackmora/lang/language)))

@;{TODO: using #:lang option with `code' keeps hyperlinks from generating}

Much of Remora is not too different from other functional languages.

@;{
@interaction[#:eval example-evaluator
(+ 1 3)
((compose sqrt add1) 3)
]
@interaction-eval-show[#:eval example-evaluator (+ 2 6)]
}

@nested[#:style 'code-inset]{
@racketinput0[(+ 1 3)]
@racket[#,(racketresultfont (tt "4"))]}

@nested[#:style 'code-inset]{
@racketinput0[((compose sqrt add1) 3)]
@racket[#,(racketresultfont (tt "2"))]}

@nested[#:style 'code-inset]{
@racketinput0[(foldr + 0 [0 1 2 3])]
@racket[#,(racketresultfont (tt "6"))]}

Wait, something is a little different there.
@code[#:lang "rackmora"]{[0 1 2 3]} is an array, not a list.
It's made of four ``@deftech{atoms},'' laid out in a row of length four.
We could write out this structure more explicitly as:

@codeblock[#:keep-lang-line? #f]{
#lang rackmora
#A(4)(0 1 2 3)
}

Instead of a single row of four, we could have two rows of two:
@tabular[#:sep @hspace[3]
         (list
          (append
           (list
            @codeblock[#:keep-lang-line? #f]{#lang rackmora
                                             [[0 1] [2 3]]})
           (list "or" @hspace[2])
           (list
            @codeblock[#:keep-lang-line? #f]{#lang rackmora
                                             #A(2 2)(0 1 2 3)})))]

But what we had before wasn't really @emph{one} row of four.
It doesn't have any rows, just the four atoms.
One row of four would be
@tabular[#:sep @hspace[3]
         (list
          (append
           (list
            @codeblock[#:keep-lang-line? #f]{#lang rackmora
                                             [[0 1 2 3]]})
           (list "or" @hspace[2])
           (list
            @codeblock[#:keep-lang-line? #f]{#lang rackmora
                                             #A(1 4)(0 1 2 3)})))]

This is like the difference between a line and a narrow rectangle.
A rectangle has a length and a width, while a line has just a length.
The line doesn't have a very small width --- it has no width at all.
The line extends along just one @deftech[#:normalize? #f]{axis},
and the rectangle extends along two axes.
How many axes an array has is called its @deftech{rank}.
The @deftech{shape} of an array is how far it extends along each axis.
@code[#:lang "rackmora"]{[0 1 2 3]}, or
@code[#:lang "rackmora"]{#A(4)(0 1 2 3)}, is a @tech{rank}-1 array with shape
@code[#:lang "rackmora"]{[4]}.
@code[#:lang "rackmora"]{[[0 1] [2 3]]}, or
@code[#:lang "rackmora"]{#A(2 2)(0 1 2 3)}, is a @tech{rank}-2 array with shape
@code[#:lang "rackmora"]{[2 2]}.

Actually, we've been working with arrays since the beginning:
@racketblock[(+ 1 3)]

These three things are @tech{rank}-0 arrays --- each one has shape
@code[#:lang "rackmora"]{[]}.
``@racket[+]'' is the name of an array whose only atom is the addition function.
@racket[1] in the above expression doesn't really stand for the number one,
but this @racket[1] does:
@codeblock[#:keep-lang-line? #f]{
#lang rackmora
#A()(1)
}
That notation is an @deftech{array literal}.
It gets some numbers to describe the shape and some atoms.
We cannot have @code[#:lang "rackmora"]{#A()((+ 1 3))} because
@code[#:lang "rackmora"]{(+ 1 3)} is not an atom (it is an expression).
Inside an array literal is the only place we talk about atoms.
Even in @code[#:lang "rackmora"]{[0 1 2 3]}, these are expressions.
This means @code[#:lang "rackmora"]{[(+ 1 3)]} is allowed ---
it produces @code[#:lang "rackmora"]{[4]}, or @code[#:lang "rackmora"]{#A()(4)}.

If the pieces of @code[#:lang "rackmora"]{(+ 1 3)} are arrays,
what happens when we use different arrays?
@racketinput[(+ [10 20 30] 3)]
@racketblock[#,(racketresultfont (tt "[13 23 33]"))]

Function application in Remora does a bit more than we saw before.
Addition and 3 were repeated with each of 10, 20, and 30, as if we'd written
@codeblock[#:keep-lang-line? #f]{
#lang rackmora
[(+ 10 3) (+ 20 3) (+ 30 3)]
}

We'd get the same result from
@codeblock[#:keep-lang-line? #f]{
#lang rackmora
([+ + +] [10 20 30] [3 3 3])
}

In that version, there's some similarity the arrays' shapes all share.
Each is a @deftech{frame} of @code[#:lang "rackmora"]{[3]}
built around the fundamental unit we're computing on.
We call those units @deftech{cells}.
In our vector-and-scalars expression,
@code[#:lang "rackmora"]{(+ [10 20 30] 3)},
the @tech{frames} are @code[#:lang "rackmora"]{[]},
@code[#:lang "rackmora"]{[3]}, and @code[#:lang "rackmora"]{[]}.
They're not all the same, but they're close enough:
We can add extra axes at the right ends of lower-@tech{rank}ed @tech{frames} in
order to make them the same as the highest-@tech{rank}ed @tech{frame}.
Adding more axes to an array means its @tech{cells} get replicated too:
@code[#:lang "rackmora"]{+} becomes @code[#:lang "rackmora"]{[+ + +]}, and
@code[#:lang "rackmora"]{3} becomes @code[#:lang "rackmora"]{[3 3 3]}.

Let's try higher-@tech{rank} arrays:
@nested[#:style 'code-inset]{
@racketinput0[(+ [[1 2 3] [4 5 6]] [10 20])]
@racket[#,(racketresultfont (tt "[[11 12 13] [24 25 26]]"))]}
Now, the @tech{frames} are @code[#:lang "rackmora"]{[]},
@code[#:lang "rackmora"]{[2 3]}, and @code[#:lang "rackmora"]{[2]}.
That means the shape of @code[#:lang "rackmora"]{+} needs to be extended with
@code[#:lang "rackmora"]{[2 3]},
and the shape of @code[#:lang "rackmora"]{[10 20]} needs to be exteneded with
@code[#:lang "rackmora"]{[3]}.

@code[#:lang "rackmora"]{+} has only one @tech{cell}, so we make 2 × 3 = 6
copies of it: @code[#:lang "rackmora"]{[[+ + +] [+ + +]]}.
@code[#:lang "rackmora"]{[10 20]} has two @tech{cells},
@code[#:lang "rackmora"]{10} and @code[#:lang "rackmora"]{20}.
We make 3 copies of each: @code[#:lang "rackmora"]{[[10 10 10] [20 20 20]]}.

So the matrix-vector addition is the same as the all-matrix expressions
@codeblock[#:keep-lang-line? #f]{
#lang rackmora
([[+ + +] [+ + +]]
 [[1 2 3] [4 5 6]]
 [[10 10 10] [20 20 20]])
}
and
@codeblock[#:keep-lang-line? #f]{
#lang rackmora
[[(+ 1 10) (+ 2 10) (+ 3 10)]
 [(+ 4 20) (+ 5 20) (+ 6 20)]]
}

We effectively treated @code[#:lang "rackmora"]{[10 20]} as a column by adding
another @tech{axis} to its shape after the @code[#:lang "rackmora"]{2}.
Not all operations behave like @code[#:lang "rackmora"]{+} in this regard.
The @code[#:lang "rackmora"]{base} operator interprets a vector of digits
according to a given radix vector.
@nested[#:style 'code-inset]{
@racketinput0[(base [8 8] [3 1])]
@racket[#,(racketresultfont (tt "25"))]}
In octal, the digits @code[#:lang "rackmora"]{[3 1]} represent the decimal
number @code[#:lang "rackmora"]{25}.
The fundamental unit in each of @code[#:lang "rackmora"]{base}'s arguments is
a vector, a @tech{rank}-1 array.
So we say that @code[#:lang "rackmora"]{base} has an @deftech{expected rank} of
1 for both arguments,
whereas @code[#:lang "rackmora"]{+} had 0.
@tech{Expected rank} is the property that determines how an argument array is
split into a @tech{frame} of @tech{cells}.

With @code[#:lang "rackmora"]{+}, the @tech{cells} were scalars,
and with @code[#:lang "rackmora"]{base}, the @tech{cells} are vectors:
@nested[#:style 'code-inset]{
@racketinput0[(base [[8 8 8] [7 24 60]] [2 6 7])]
@racket[#,(racketresultfont (tt "[183 3274]"))]}
The @tech{frames} are @code[#:lang "rackmora"]{[]},
@code[#:lang "rackmora"]{[2]}, and @code[#:lang "rackmora"]{[]}.
The last axis of each argument is the shape of its @tech{rank}-1
@techlink{cells}.

Expanding the second argument's frame to match the first makes it a
@code[#:lang "rackmora"]{[2]} frame of @code[#:lang "rackmora"]{[3]} cells.
Its shape becomes @code[#:lang "rackmora"]{[2 3]}.
This effectively treats the vector as a row, whereas
@code[#:lang "rackmora"]{+} treated it as a column.

What if we wanted to add a matrix and a row vector?
We need a function which expects @tech{rank}-1 arguments and adds them.
A function is written like this:
@codeblock[#:keep-lang-line? #f]{
#lang rackmora
(fn ((x 1) (y 1)) (+ x y))
}

@code[#:lang "rackmora"]{x} and @code[#:lang "rackmora"]{y} are the names of the
function's arguments.
They are each marked with a @code[#:lang "rackmora"]{1} to indicate that our
function expects @tech{rank} 1 for each argument.
The function body @code[#:lang "rackmora"]{(+ x y)} simply adds the two
@tech{rank}-1 arguments.
Even if the function is applied to higher-ranked arguments, inside the function
body, @code[#:lang "rackmora"]{x} and @code[#:lang "rackmora"]{y} always refer
to rank-1 cells of those arguments.

For convenience, we'll give this function a name:
@codeblock[#:keep-lang-line? #f]{
#lang rackmora
(def vec+ (fn ((x 1) (y 1)) (+ x y)))
}
The @code[#:lang "rackmora"]{def} form takes a name and an expression and binds
the name to the result of that expression.

Now let's add a matrix and a row vector:
@nested[#:style 'code-inset]{
@racketinput0[(vec+ [[1 2 3] [4 5 6]] [10 20 30])]
@racket[#,(racketresultfont (tt "[[11 22 33] [14 25 36]]"))]}
Frames are @code[#:lang "rackmora"]{[]}, @code[#:lang "rackmora"]{[2]}, and
@code[#:lang "rackmora"]{[]}, just like when we used
@code[#:lang "rackmora"]{base}.

We changed how the vector was expanded into a matrix by @deftech{reranking}
@code[#:lang "rackmora"]{+}.
That is, we made a version of @code[#:lang "rackmora"]{+} with different
expected ranks for its arguments.
Reranking is common enough in array-oriented programming that it gets special
shorthand in Remora:
@codeblock[#:keep-lang-line? #f]{
#lang rackmora
#r(1 1)+
}
That's the same function as @code[#:lang "rackmora"]{vec+}, written in reranking
shorthand.
It gives the same result:
@;{TODO: this lets reader macros appear in racketinput0 but doesn't generate
   hyperlinks for reranked function names}
@nested[#:style 'code-inset]{
@racketinput0[#,(code #:lang "rackmora"
                      "(#r(1 1)+ [[1 2 3] [4 5 6]] [10 20 30])")]
@racket[#,(racketresultfont (tt "[[11 22 33] [14 25 36]]"))]}

All this talk about expected rank has focused on the argument arrays, but the
function array is a frame of cells too.
It doesn't have to be a scalar, with just one function.
@nested[#:style 'code-inset]{
@racketinput0[([magnitude angle] 3+4i)]
@racket[#,(racketresultfont (tt "[5 0.9272952180016122]"))]}

The expected rank for the function array is 0, no matter what functions it
contains.
So our frames here are @code[#:lang "rackmora"]{[2]} and
@code[#:lang "rackmora"]{[]}.
Each function cell gets applied to the one argument cell.
Suppose we want both @code[#:lang "rackmora"]{magnitude} and
@code[#:lang "rackmora"]{angle} applied to each of two complex numbers.
Directly applying @code[#:lang "rackmora"]{[magnitude angle]} is not what we
want.
That would apply @code[#:lang "rackmora"]{magnitude} to one scalar cell and
@code[#:lang "rackmora"]{angle} to the other.

Instead, we can make @code[#:lang "rackmora"]{[magnitude angle]} into a single
function (really, an array containing one function) which expects scalar cells:
@nested[#:style 'code-inset]{
@racketinput0[#,(code #:lang "rackmora"
                      "(#r(0)[magnitude angle] [3+4i -5-12i])")]
@racket[#,(racketresultfont
           (tt "[[5 0.9272952180016122] [13 -1.965587446494658]]"))]}

The frames here are @code[#:lang "rackmora"]{[]} for the function position and
@code[#:lang "rackmora"]{[2]} for the argument.
So the whole function is replicated and applied to each of
@code[#:lang "rackmora"]{3+4i} and @code[#:lang "rackmora"]{-5-12i}.

Some functions don't expect arguments of a specific rank.
They have expected rank @code[#:lang "rackmora"]{all}, meaning the argument is
always considered to have a scalar frame.
One such function is @code[#:lang "rackmora"]{head}.
It extracts the first @deftech{item}, that is a sub-array with one less
@tech{axis}.
@nested[#:style 'code-inset]{
@racketinput0[(head [0 1 2 3])]
@racket[#,(racketresultfont (tt "0"))]}

@nested[#:style 'code-inset]{
@racketinput0[(head [[0 1] [2 3]])]
@racket[#,(racketresultfont (tt "[0 1]"))]}

@nested[#:style 'code-inset]{
@racketinput0[(head [[[0 1] [2 3]] [[4 5] [6 7]]])]
@racket[#,(racketresultfont (tt "[[0 1] [2 3]]"))]}

From a vector, we get the first element.
From a matrix, we get the first row.
From a rank-3 array, we get the first plane, and so on.

Instead of getting a row from a matrix, we could get a column by getting the
first element of each row, that is of each rank-1 cell.
@nested[#:style 'code-inset]{
@racketinput0[#,(code #:lang "rackmora"
                      "(#r(1)head [[0 1] [2 3]])")]
@racket[#,(racketresultfont (tt "[0 2]"))]}

Another @code[#:lang "rackmora"]{all}-ranked function is
@code[#:lang "rackmora"]{append}, which joins two arrays along their major axis.
@nested[#:style 'code-inset]{
@racketinput0[(append [[0 1] [2 3]] [[10 20] [30 40]])]
@racket[#,(racketresultfont (tt "[[0 1] [2 3] [10 20] [30 40]]"))]}

@nested[#:style 'code-inset]{
@racketinput0[(append [0 1] [2 3])]
@racket[#,(racketresultfont (tt "[0 1 2 3]"))]}

Reranking @code[#:lang "rackmora"]{append} lets us join arrays along a different
axis:
@nested[#:style 'code-inset]{
@racketinput0[#,(code #:lang "rackmora"
                      "(#r(1 1)append [[0 1] [2 3]] [[10 20] [30 40]])")]
@racket[#,(racketresultfont (tt "[[0 1 10 20] [2 3 30 40]]"))]}

@code[#:lang "rackmora"]{#r(1 1)append} concatenates vectors.
Applying it to two matrices concatenates corresponding vector cells and
reassembles them in the vector frame to produce a new matrix.

For most functions, the output shape is determined by the argument shapes.
When we append a @code[#:lang "rackmora"]{[2]}-vector and a
@code[#:lang "rackmora"]{[3]}-vector, we know we will get a
@code[#:lang "rackmora"]{[5]}-vector.
Adding a @code[#:lang "rackmora"]{[3 6]}-matrix and a
@code[#:lang "rackmora"]{[3]}-vector will always produce a
@code[#:lang "rackmora"]{[3 6]}-matrix.

Some functions' output shape depends on the actual values of the input atoms,
not just shapes.
If we apply such a function to a multi-celled array, we could get result cells
with differing shapes.
We can't put those together in a single array.
There is no valid shape that describes that array.
An array can only have one size along each axis.

In order to make such functions safe to use on higher-ranked arguments, they
produce @deftech{boxes}.
@nested[#:style 'code-inset]{
@racketinput0[(iota [2 3])]
@racket[#,(racketresultfont (tt "(box [[0 1 2] [3 4 5]])"))]}

@nested[#:style 'code-inset]{
@racketinput0[(iota [4 2])]
@racket[#,(racketresultfont (tt "(box [[0 1] [2 3] [4 5] [6 7]])"))]}

A box is a scalar datum which may contain an array of any shape.
Producing boxes makes it safe to lift @code[#:lang "rackmora"]{iota}:
@nested[#:style 'code-inset]{
@racketinput0[(iota [[2 3] [4 2]])]
@racket[#,(racketresultfont
           (tt (string-append "[(rem-box [[0 1 2] [3 4 5]]) "
                              "(rem-box [[0 1] [2 3] [4 5] [6 7]])]")))]}

The result has shape @code[#:lang "rackmora"]{[2]}.
Its items are boxes.
The first box contains a @code[#:lang "rackmora"]{[2 3]}-array, and the second
contains a @code[#:lang "rackmora"]{[4 2]}-array.
The @code[#:lang "rackmora"]{unbox} form allows the contents of a box to be
bound to a variable:
@nested[#:style 'code-inset]{
@racketinput0[(unbox nats (iota [5])
                (foldr * 1 (add1 nats)))]
@racket[#,(racketresultfont (tt "120"))]}

We temporarily had a vector with ``unknown'' length while computing 5!.
Folding eliminates the unknown length by producing a scalar.
This means we could safely replace @code[#:lang "rackmora"]{5} with an unknown
natural number.
Our unknown-length vector is folded into a scalar, making it safe to return
without boxing it.
This means we could write a factorial function:
@codeblock[#:keep-lang-line? #f]{
#lang rackmora
(fn ((n 0))
  (unbox nats (iota [n])
    (foldr * 1 (add1 nats))))
}
However, if we write a function that adds 1 to a box's contents, safety demands
that the function return a box.
Otherwise, applying it to an argument like
@code[#:lang "rackmora"]{(iota [[2 3] [4 2]])} would fail.
