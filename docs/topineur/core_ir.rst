Core IR Reference
=================

Overview
--------

The Topineur compiler lowers surface syntax to a compact core IR shared with
the legacy Lisp pipeline.  This document captures the invariants each pass must
preserve so that downstream stages—ANF, closure conversion, and code generation—
remain correct while the project migrates to Topineur.

Module Layout
-------------

* ``Module`` is the compilation unit.  Each module records:

  - ``moduleName``: canonical identifier (e.g. ``Topineur.Std.Core``).
  - ``imports``: already-resolved dependency list.
  - ``declarations``: ordered sequence of ``Decl`` values (functions, objects,
    traits, constants).

* Declarations must be dependency-sorted; mutual recursion is encoded via
  explicit ``LetRec`` nodes inside function bodies.

Core Expressions
----------------

* Expression nodes are already normalized for ANF:

  - ``CoreLambda params body`` where ``body`` is an expression that has been
    alpha-renamed and contains no implicit captures.
  - ``CoreVar`` only references names introduced by direct bindings.
  - ``CoreApp`` applies a function value to positional arguments; effect rows
    and linearity metadata travel alongside the function value.
  - ``CoreLet`` introduces non-recursive bindings; recursive bindings use
    ``CoreLetRec`` with an explicit binding map.
  - ``CoreMatch`` handles pattern matching; constructors and literals are
    lowered to simple guards to simplify code generation.

* Every binding site records a ``SourceSpan`` so diagnostics can reference the
  original Topineur code even after several lowering passes.

Objects and Traits
------------------

* Objects desugar into record literals where every field is represented as a
  slot.  The receiver (``self``) is threaded explicitly when compiling methods.

* Trait implementations emit:

  - A concrete dictionary record containing closures for each trait method.
  - A witness structure linking the concrete type to its trait dictionary.
  - Optional specialization hooks when the implementation captures type
    arguments.

Effects and Linearity
---------------------

* Effect rows become an ordered list of capability tags stored in lambda
  annotations.  Call sites must only invoke lambdas whose effect rows are a
  subset of their ambient permission set.

* Linear bindings are tracked through ``CoreConsume`` instructions.  Each linear
  variable must appear exactly once in a consume position before the end of its
  scope.  Branches (``CoreIf``/``CoreMatch``) are required to consume the same
  linear variables on every path; the checker enforces this invariant.

Testing Expectations
--------------------

* Desugaring tests snapshot the generated core trees for ``examples/topineur``.
* Backend tests execute the same desugared core nodes through ANF and code
  generation to ensure runtime stability.
* Any change to the core IR must update this document and the migration
  timeline.
