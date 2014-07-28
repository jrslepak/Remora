#lang scribble/manual
@require[(for-label (except-in racket/base
                               box unbox)
                    rackmora)]

@defmodulelang[rackmora]
@title{Functions}

@defform[(fn ((id rank) ...) body ...+)]{
An array function expects each argument to have a particular rank. This rank is
basic unit the function operates on, and the function implicitly lifts to
operate on arguments of higher rank. For example, @racket[+] expects rank 0
arguments, and @racket[antibase] expects its first argument to be a rank 1 array
of digits. The operators are implicitly lifted to higher-rank arguments. Some
functions expect rank @racket[all] for certain arguments. This means the
function is never lifted over that argument---it consumes any argument as its
basic unit, no matter the rank.

The function body is one or more expressions, which are evaluated in sequence.
The result of the final expression is returned as the result of the function.

Functions are atoms, so a @racket[fn] form can be used as an element of an
@racket[alit] form or in expression position where it will be converted to a
rank 0 array.
}