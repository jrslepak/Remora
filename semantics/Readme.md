# Semantic model
This is the PLT Redex model developed for the Remora array language. Syntax and reduction semantics for the untyped calculus are defined in `language.rkt`. Syntax and type judgment for the typed calculus are in `dependent-lang.rkt`, and `typed-reduction.rkt` defines reduction semantics.

The files' `test` submodules include code examples. These tests/examples can also be run individually via DrRacket's REPL. Redex also allows the user to get a step-by-step view of a reduction or typing derivation.

To see a reduction step-by-step, run `(traces ->Array expr-to-reduce)`. For example,

```
(traces
 ->Array
 (term ((scalar +) (A (3 3) (1 2 3
                             4 5 6
                             7 8 9))
                   (A (3) (10 20 30)))))
```

should show all reduction steps in the maxtrix-vector addition. For long reductions, Redex may choose not to generate the entire reduction graph right away. A "Reduce" button at the bottom of the "Traces" window instructs Redex to generate more nodes in the graph.

To see a typing derivation, run `(show-derivations (build-derivations judgment-to-render))`. This requires a judgment that actually holds. Thus

```
(show-derivations
 (build-derivations
  (type-of () () () (A (3 2) (4 1 6 2 3 5)) type)))
```

produces a derivation tree, while 

```
(show-derivations
 (build-derivations
  (type-of () () () (A (3 3) (4 1 6 2 3 5)) type)))
```

produces an error.
