#lang scribble/manual
@require[(for-label #;(except-in racket/base
                               box unbox
                               #%module-begin
                               #%top-interaction)
                    remora/dynamic/main
                    #;remora/dynamic)]

@;defmodulelang[remora/dynamic]
@declare-exporting[remora/dynamic/main]
@title{Boxes: handling irregular data}

A box wraps an array of arbitrary shape in a scalar structure.
This allows irregular data with some additional processing to ensure that
aggregate lifting of functions is only done with regular data.

@defform[(box expr)]{
Construct a box around the array produced by
@code[#:lang "remora/dynamic"]{expr}.
}

Suppose we wish to find the means of several samples of differing size.
The samples themselves can only be put together in a single array if they are
boxed:
@;{TODO: figure out why this only generates proper hyperlinks with
   #lang racket instead of #lang remora/dynamic}
@codeblock[#:keep-lang-line? #f]{
#lang racket
[(box [2 7 1 8 2 8])
 (box [3 1 4 1 5])
 (box [2 2 2 3 2 0 0])]
}

In order to operate on the boxes' contents, we must use the
@code[#:lang "remora/dynamic"]{unbox} form:
@defform[(unbox id box-expr body)]{
Evaluate @code[#:lang "remora/dynamic"]{body} with
@code[#:lang "remora/dynamic"]{id} bound to the contents of
@code[#:lang "remora/dynamic"]{box-expr}.
}

We can use this to write a function which finds the mean of a boxed vector:
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
(def box-mean
  (fn ((some-box 0))
    (unbox xs some-box
      (/ (foldr + 0 xs)
         (tally xs)))))
}
Note that @code[#:lang "remora/dynamic"]{box-mean} expects a scalar argument (a
single box) but operates on the box's contents as an aggregate an produces an
unboxed scalar.
Applying @code[#:lang "remora/dynamic"]{box-mean} to our vector of boxed vectors
gives @code[#:lang "remora/dynamic"]{[14/3 14/5 11/7]}

Some functions produce arrays whose shape depends on the contents of the
arguments.
For example, @code[#:lang "remora/dynamic"]{iota} consumes a vector describing
an array's shape and produces an array of that shape.
Using such functions on higher-ranked arguments could produce result cells of
differing shapes, which would form an irregular array when collected together.
