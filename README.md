# sicp-interpreter-ocaml
Work In Progress - an implementation of
[Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/index.html)
Chapter 4 in Ocaml.

I decided to implement section 4.3 (a non-deterministic evaluator) in a statically typed
language to aid my understanding of the continuations used to implement backtracking. These
branches show the various stages of development:
- `v0` - A hastily written Scheme/Lisp interpreter.
- `v1` - I decided that `v0` was doing too much work in the parsing stage. `v1` removes the overuse of
variant types and sticks to using lists as the main data structure.
- `non_deterministic_first_attempt` - Aborted attempt to add non-determinism with continuations for back-tracking.
- `cps` - Rewrote `v1` to use continuation passing style (without adding backtracking/non-determinism).
- `non_deterministic` - Added failure continuations to `cps` and implemented back-tracking/non-determinism.
- `cps_monadic` - Refactored `cps` to use a monadic continuation interface.
- `master` - currently based-off `non_deterministic`
