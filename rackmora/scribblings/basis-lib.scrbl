#lang scribble/manual
@require[(for-label rackmora/lang/language
                    #;rackmora)]
@;@declare-exporting[rackmora]
@defmodule[rackmora]
@title{Built-in functions}

@;{
@defproc[(id [x 'all]) ★]{Identity function}
@defproc[(+ [x 0] [y 0]) 0]{Addition}
@defproc[(- [x 0] [y 0]) 0]{Subtraction}
@defproc[(* [x 0] [y 0]) 0]{Multiplication}
@defproc[(/ [x 0] [y 0]) 0]{Division}
}

@defproc[(and [x 0] [y 0]) 0]{Logical AND}
@defproc[(or [x 0] [y 0]) 0]{Logical Or}

@defproc[(signum [x 0]) 0]{
Find the sign of a nonzero number, or more generally,
normalize a complex number to unit magnitude.}


@defproc[(logb [b 0] [x 0]) 0]{
Logarithm of @racket[x] with base @racket[b]}

@defproc[(ln [x 0]) 0]{
Logarithm of @racket[x] with base @italic[]{e} (Euler's number)}

@defproc[(log [x 0]) 0]{
Logarithm of @racket[x] with base 10}

@defproc[(lg [x 0]) 0]{
Logarithm of @racket[x] with base 2}

@;{TODO: define "item" earlier in the tutorial, make uses of it link there}
@defproc[(head [x 'all]) ★]{
Extract the first item of @racket[x]}

@defproc[(behead [x 'all]) ★]{
Remora the first item of @racket[x]}

@defproc[(tail [x 'all]) ★]{
Extract the last item of @racket[x]}

@defproc[(curtail [x 'all]) ★]{
Remora the last item of @racket[x]}

@defproc[(take [n 0] [x 'all]) ★]{
Extract the first @racket[n] items of @racket[x]}

@defproc[(take-right [n 0] [x 'all]) ★]{
Extract the last @racket[n] items of @racket[x]}

@defproc[(drop [n 0] [x 'all]) ★]{
Remove the first @racket[n] items of @racket[x]}

@defproc[(drop-right [n 0] [x 'all]) ★]{
Remove the last @racket[n] items of @racket[x]}

@defproc[(reverse [x 'all]) ★]{
Reverse the items of @racket[x]}

@defproc[(rotate [x 'all] [n 0]) ★]{
Cyclically shift the items of @racket[x] by @racket[n] spaces}

@defproc[(append [x 'all] [y 'all]) ★]{
Construct an array whose items are the items of @racket[x] followed by the items
of @racket[y].
The items of both arguments are required to have matching shape.}

@defproc[(deal [count 0] [x 'all]) ★]{
Randomly choose @racket[count] items from @racket[x] without replacement.
}

@defproc[(base [radix 1] [digits 1]) 0]{
Interpret @racket[digits] as a sequence of digits according to the place-value
described by @racket[radix].
For example,
@code[#:lang "rackmora"]{(base [8 8 8] [2 7 3])}
treats 2, 7, and 3 as a sequence of octal digits, returning 187.
Not all columns are required to permit the same number of digits.
The digits could be interpreted as days, hours, minutes, and seconds using the
radix @code[#:lang "rackmora"]{[7 24 60 60]}.
}

@defproc[(antibase [radix 1] [x 0]) 1]{
Reverses the calculation of @code[#:lang "rackmora"]{base}.
@code[#:lang "rackmora"]{x} is converted to a sequence of digits.
The number of digits specified in @code[#:lang "rackmora"]{radix} prevents 
representation of numbers larger than the cumulative product of
@code[#:lang "rackmora"]{radix}.
If x is too large, the result is as if @code[#:lang "rackmora"]{x} were reduced
modulo that product.
}

@defproc[(foldl [op 'all] [init 'all] [x 'all]) ★]{
Left-associative fold over @code[#:lang "rackmora"]{x} using
@code[#:lang "rackmora"]{op}, with @code[#:lang "rackmora"]{init} as the base
case.
}

@defproc[(foldr [op 'all] [init 'all] [x 'all]) ★]{
Right-associative fold over @code[#:lang "rackmora"]{x} using
@code[#:lang "rackmora"]{op}, with @code[#:lang "rackmora"]{init} as the base
case.
}

@defproc[(scan [op 'all] [init 'all] [x 'all]) ★]{
Similar to a left-associative fold, but returns an array whose items are the
intermediate results while folding.
}

@defproc[(shape-of [x 'all]) 1]{
Construct a vector whose elements are the dimensions of
@code[#:lang "rackmora"]{x}
}

@defproc[(reshape [new-shape 1] [x 'all]) ★]{
Construct an array whose dimensions are the atoms of
@code[#:lang "rackmora"]{new-shape} and whose atoms are the atoms of
@code[#:lang "rackmora"]{x} (with the sequence repeated as many times as
necessary).
}

@defproc[(iota [new-shape 1]) ★]{
Construct an array whose dimensions are the atoms of
@code[#:lang "rackmora"]{new-shape} and whose atoms are the first @italic{n}
natural numbers, where @italic{n} is the product of the atoms of
@code[#:lang "rackmora"]{new-shape}.
}

@defproc[(nub [x 'all]) ★]{
Remove all but the first occurrence of each item from
@code[#:lang "rackmora"]{x}.
}

@defproc[(nub-sieve [x 'all]) 1]{
Construct a vector of booleans which indicate whether the corresponding item
in @code[#:lang "rackmora"]{x} is the first occurrence of that item.
}

@defproc[(ravel [x 'all]) 1]{
Construct a vector whose atoms are the atoms of @code[#:lang "rackmora"]{x}.
}

@defproc[(itemize [x 'all]) ★]{
Construct an array whose sole item is @code[#:lang "rackmora"]{x}, @italic{i.e.}
an array with the same atoms as @code[#:lang "rackmora"]{x} but whose shape has
a @racket[1] added to the beginning. This result is equivalent to
@code[#:lang "rackmora"]{[x]}.
}

@defproc[(length [x 'all]) 0]{
Extract the first element of @code[#:lang "rackmora"]{x}'s shape, the length
along its major axis.
}

@defproc[(equal [x 'all] [y 'all]) 0]{
Return @racket[#t] if @code[#:lang "rackmora"]{x} and
@code[#:lang "rackmora"]{y} have the same atoms and shape.
Return @racket[#f] otherwise.
}

@defproc[(show [x 'all]) void]{
Print a whole array. Using Racket's built-in functions such as @racket[print]
will cause them to be applied to each scalar individually.
}

@defproc[(read [port 0]) ★]{
Read an array from the designated input port.
}

@defproc[(filter [bools 1] [x 'all]) ★]{
Keep or discard each item of @code[#:lang "rackmora"]{x} according to the
corresponding element of @code[#:lang "rackmora"]{bools}.
}

@defproc[(window [l 0] [x 'all]) ★]{
Construct an array whose items are the first @code[#:lang "rackmora"]{l}
rotations of @code[#:lang "rackmora"]{x}.
This is an array with length @code[#:lang "rackmora"]{l} on its major axis and
subsequences of the items of @code[#:lang "rackmora"]{x} laid out along its
second axis.
}

@defproc[(select [bool 0] [x 'all] [y 'all]) ★]{
If @code[#:lang "rackmora"]{bool} is true, return @code[#:lang "rackmora"]{x}.
Otherwise, return @code[#:lang "rackmora"]{y}.
}

@defproc[(unsafe-unbox [b 0]) ★]{
Extract the contents of the box @code[#:lang "rackmora"]{b}.
This is present for historical reasons.
It is not safe for use on an aggregate of boxes because their contents may not
have matching shapes.
}

