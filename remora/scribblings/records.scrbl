#lang scribble/manual
@require[racket/sandbox
         scribble/eval
         (for-label remora/dynamic
                    (only-in remora/dynamic/lang/language
                             define λ))]
@declare-exporting[remora/dynamic/main]

@(sandbox-output 'string)
@(define example-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'remora/dynamic/lang/language)))

@title{Records}
Records are heterogeneous aggregate data.
Computing on record data is done by extracting individual elements,
in constrast to arrays,
which are normally consumed by lifting a function over every cell.
In Remora, a record is a collection of named fields, each containing an atom.
A record is itself an atom as well.

To write out a record datum,
each field's name and value are paired together in parenthesis,
with the entire collection of fields enclosed in brases.
For example, we might write a city's location and name like this:

@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
{(name "Boston") (lat 42.36) (long -71.06)}}

A record can have many fields, only one field, or no fields at all.
@code[#:lang "remora/dynamic"]{{(answer 42)}} and
@code[#:lang "remora/dynamic"]{{}} are valid records.

Since records are atoms, they can be used as record fields:

@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
{(airline "AA") (departure {(hour 10) (min 30) (airport "BOS")})
                (arrival   {(hour 12) (min 15) (airport "ORD")})}}

Working on particular elements of a record is done with @tech{lenses}.
A @deftech{lens} is a function which "focuses" on
a particular piece of a data structure
and can be used to look at or change that piece.
For Remora records, lenses are built by the
@code[#:lang "remora/dynamic"]{lens} form.
@defform[(lens fname)]{
Construct a lens for record field @code[#:lang "remora/dynamic"]{fname}.
This does not depend on the particular record or collection of fields.
The resulting lens will work on any record which has a field with that name.
}

There are three operations on lenses, called
@code[#:lang "remora/dynamic"]{view},
@code[#:lang "remora/dynamic"]{set}, and
@code[#:lang "remora/dynamic"]{over}.
They are curried functions, so they can easily be used in partial application.
For example, @code[#:lang "remora/dynamic"]{(view (lens lat))} is a function
which extracts the @code[#:lang "remora/dynamic"]{lat} field from a record,
and @code[#:lang "remora/dynamic"]{((over (lens lat)) add1)} is a function which
increases a record's @code[#:lang "remora/dynamic"]{lat} field by 1.

@defproc[(view [L 0]) 0]{
Build a procedure which extracts
the field focused by the lens @code[#:lang "remora/dynamic"]{L}
from an argument record.
That is, @code[#:lang "remora/dynamic"]{((view L) R)}
is equal to the field of @code[#:lang "remora/dynamic"]{R}
focused by @code[#:lang "remora/dynamic"]{L}.

@code[#:lang "remora/dynamic"]{(view (compose (lens fname ...)))}
can also be written as
@code[#:lang "remora/dynamic"]{#_(fname ...)}.}

@;{Extract from @code[#:lang "remora/dynamic"]{R} the field
focused by the lens @code[#:lang "remora/dynamic"]{L}.}

@defproc[(set [L 0]) 0]{
Build a curried procedure which takes
a new value @code[#:lang "remora/dynamic"]{V}
and then a record @code[#:lang "remora/dynamic"]{R}
and produces a new version of @code[#:lang "remora/dynamic"]{R}
with the field focused by the lens @code[#:lang "remora/dynamic"]{L}
changed to @code[#:lang "remora/dynamic"]{V}.
That is, @code[#:lang "remora/dynamic"]{(((set L) V) R)}
is equal to @code[#:lang "remora/dynamic"]{R} except that
@code[#:lang "remora/dynamic"]{L}'s focused field has changed to
@code[#:lang "remora/dynamic"]{V}.

@code[#:lang "remora/dynamic"]{(set (compose (lens fname ...)))}
can also be written as
@code[#:lang "remora/dynamic"]{#=(fname ...)}.}

@;{Build a version of @code[#:lang "remora/dynamic"]{R}
with the field focused by the lens @code[#:lang "remora/dynamic"]{L}
changed to @code[#:lang "remora/dynamic"]{V}.}

@defproc[(over [L 0]) 0]{
Build a curried procedure which takes
a unary function @code[#:lang "remora/dynamic"]{F}
and then a record @code[#:lang "remora/dynamic"]{R}
and produces a new version of @code[#:lang "remora/dynamic"]{R}
with the field focused by the lens @code[#:lang "remora/dynamic"]{L}
changed by applying the function @code[#:lang "remora/dynamic"]{F}.
That is, @code[#:lang "remora/dynamic"]{(((over L) F) R)}
is equal to @code[#:lang "remora/dynamic"]{R} except that
@code[#:lang "remora/dynamic"]{L}'s focused field has changed
from its original value @code[#:lang "remora/dynamic"]{V} to
@code[#:lang "remora/dynamic"]{(F V)}.

@code[#:lang "remora/dynamic"]{(over (compose (lens fname ...)))}
can also be written as
@code[#:lang "remora/dynamic"]{#^(fname ...)}.}

@;{Extract from @code[#:lang "remora/dynamic"]{R} the field
focused by the lens @code[#:lang "remora/dynamic"]{L}.}

Composing multiple lenses narrows the focus.
In the airline flight record above, we could focus on the departure airport
with @code[#:lang "remora/dynamic"]{(compose (lens departure) (lens airport))}.

@nested[#:style 'code-inset]{
 @racketinput0[((view (compose (lens departure) (lens airport)))
               {(airline "AA")
                (departure {(hour 10) (min 30) (airport "BOS")})
                (arrival   {(hour 12) (min 15) (airport "ORD")})})]
@racket[#,(racketresultfont "\"BOS\"")]}

Alternatively, using the syntactic sugar for lens operations:
@nested[#:style 'code-inset]{
 @racketinput0[(#,(tt "#_")(departure airport)
               {(airline "AA")
                (departure {(hour 10) (min 30) (airport "BOS")})
                (arrival   {(hour 12) (min 15) (airport "ORD")})})]
@racket[#,(racketresultfont "\"BOS\"")]}


@section{Arrays of records}
By collecting multiple records into an array,
we can make a table where different columns contain different types of data,
but each individual column is homogeneous.
The record constructor is an ordinary Remora function,
so it lifts to handle array arguments
using the same machinery as other rank-polymorphic functions.
This means we can build a table either by rows:

@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
[{(loc "Dallas") (day 28) (month 3) (year 2019) (hi 74) (lo 57)}
 {(loc "Dublin") (day 1) (month 4) (year 2019) (hi 11) (lo 5)}
 {(loc "Nome") (day 31) (month 3) (year 2019) (hi 31) (lo 26)}
 {(loc "Tunis") (day 31) (month 3) (year 2019) (hi 21) (lo 12)}]}

or by columns:

@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
{(loc ["Dallas" "Dublin" "Nome" "Tunis"])
 (day [28 1 31 31])
 (month [3 4 3 3])
 (year 2019)
 (hi [74 11 31 21])
 (lo [57 5 26 12])}}

Both of these expressions produce the same array,
which we will call @code[#:lang "remora/dynamic"]{temp-readings}
in our running example.
Note that in the column-wise construction,
where we lift the record constructor over a 4-vector frame,
we can still include scalar-frame arguments.

Functions for operating on records also lift.
When we @code[#:lang "remora/dynamic"]{view} a particular field,
that lifts to extracting a particular column from the table:
@nested[#:style 'code-inset]{
 @racketinput0[(#,(tt "#_")(loc) temp-readings)]
@;{@racket[#,(racketresultfont "[\"Dallas\" \"Dublin\" \"Nome\" \"Tunis\"]")]}
  @racket[["Dallas" "Dublin" "Nome" "Tunis"]]
}

Similarly, applying a function @code[#:lang "remora/dynamic"]{over} a field
turns into updating the entire column:
@nested[#:style 'code-inset]{
 @racketinput0[((#,(tt "#^")(loc) string-upcase) temp-readings)]}
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
[{(loc "DALLAS") (day 28) (month 3) (year 2019) (hi 74) (lo 57)}
 {(loc "DUBLIN") (day 1) (month 4) (year 2019) (hi 11) (lo 5)}
 {(loc "NOME") (day 31) (month 3) (year 2019) (hi 31) (lo 26)}
 {(loc "TUNIS") (day 31) (month 3) (year 2019) (hi 21) (lo 12)}]}

Data cleaning often requires changing only certain entries in a column,
such as in @code[#:lang "remora/dynamic"]{temp-readings},
where some temperatures (those from US cities)
are given in Fahrenheit and others in Celsius.
We only need a function that updates a single record.
Assuming we already have a function @code[#:lang "remora/dynamic"]{in-usa?}
which recognizes whether a location is in the US
and a temperature conversion function @code[#:lang "remora/dynamic"]{f->c},
we can @code[#:lang "remora/dynamic"]{view} the location field
to determine whether to use @code[#:lang "remora/dynamic"]{f->c}
or the identity function @code[#:lang "remora/dynamic"]{id}
as the temperature-fixing function.
Then we apply our chosen @code[#:lang "remora/dynamic"]{fix-temp}
to the @code[#:lang "remora/dynamic"]{hi}
and @code[#:lang "remora/dynamic"]{lo} fields:

@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
(define (normalize-temps (w 0))
  (define fix-temp
    (select (in-usa? (#_(loc) w)) f->c id))
  ((#^(hi) fix-temp) ((#^(lo) fix-temp) w)))}

Function application automatically lifts
@code[#:lang "remora/dynamic"]{normalize-temps}
over the entire table.

@nested[#:style 'code-inset]{
 @racketinput0[(define fixed-readings (normalize-temps temp-readings))]
 @racketinput0[fixed-readings]}
@codeblock[#:keep-lang-line? #f]{
#:lang remora/dynamic
[{(loc "Dallas") (day 28) (month 3) (year 2019) (hi 23.33) (lo 13.89)}
 {(loc "Dublin") (day 1) (month 4) (year 2019) (hi 11) (lo 5)}
 {(loc "Nome") (day 31) (month 3) (year 2019) (hi -0.56) (lo -3.33)}
 {(loc "Tunis") (day 31) (month 3) (year 2019) (hi 21) (lo 12)}]}

We can also look at just the US-originating rows using
@code[#:lang "remora/dynamic"]{filter*},
which takes a boolean vector (effectively a bit mask) identifying
the vector elements to keep.
We can build a mask for US temperature readings:

@nested[#:style 'code-inset]{
 @racketinput0[(define us-locs (in-usa? (#,(tt "#_")(loc) fixed-readings)))]
 @racketinput0[us-locs]
 @racket[[#t #f #t #f]]
 @racketinput0[(filter* us-locs fixed-readings)]
}
@codeblock[#:keep-lang-line? #f]{
#:lang remora/dynamic
(box
 [{(loc "Dallas") (day 28) (month 3) (year 2019) (hi 23.33) (lo 13.89)}
  {(loc "Nome") (day 31) (month 3) (year 2019) (hi -0.56) (lo -3.33)}])}

Lifting @code[#:lang "remora/dynamic"]{filter*} over multiple masks
gives us a way to partition the table into multiple pieces:
@nested[#:style 'code-inset]{
 @racketinput0[(filter* [us-locs (not us-locs)] fixed-readings)]
}
@codeblock[#:keep-lang-line? #f]{
#:lang remora/dynamic
[(box
  [{(loc "Dallas") (day 28) (month 3) (year 2019) (hi 23.33) (lo 13.89)}
   {(loc "Nome") (day 31) (month 3) (year 2019) (hi -0.56) (lo -3.33)}])
 (box
  [{(loc "Dublin") (day 1) (month 4) (year 2019) (hi 11) (lo 5)}
   {(loc "Tunis") (day 31) (month 3) (year 2019) (hi 21) (lo 12)}])]}

Hierarchical column nesting is the lifted form of record nesting.
@codeblock[#:keep-lang-line? #f]{
#lang remora/dynamic
(define flights
  (append
   {(airline "AA")
    (departure {(hour [7 9 10]) (min [0 2 20]) (airport "BOS")})
    (arrival   {(hour [8 11 12]) (min [57 0 15]) (airport "ORD")})}
   {(airline "JB")
    (departure {(hour [8 11]) (min [47 18]) (airport "BOS")})
    (arrival   {(hour [10 13]) (min [45 14]) (airport "ORD")})}))}

Each row has a @code{departure} entry and an @code{arrival} entry,
which both contain their own respective times and locations.
Asking for the departure time uses nested lenses:
one for @code{departure}, composed with each of @code{hour} and @code{min}.
This code lifts @code[#:lang "remora/dynamic"]{view} over a 2-vector of lenses,
so we then wrap the resulting 2-vector of field accessors
as a scalar function before applying it.
@nested[#:style 'code-inset]{
 @racketinput0[(~(0)(view (compose (lens departure) [(lens hour) (lens min)])) flights)]
}
@codeblock[#:keep-lang-line? #f]{
#:lang remora/dynamic
[[7 0]
 [9 2]
 [10 20]
 [8 47]
 [11 18]]}



