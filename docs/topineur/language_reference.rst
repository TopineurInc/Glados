Language Reference
==================

This chapter summarises the core constructs of Topineur.  It is intentionally
compact so that it can serve as an entry point for implementers and readers of
the specification.

Object Types
------------

Objects are immutable values that bundle state and methods.  Their syntax is
reminiscent of record definitions with attached member declarations.

.. code-block:: topineur

   object type Point {
     x: Float
     y: Float

     def translate(dx: Float, dy: Float): Point =
       Point { x = x + dx, y = y + dy }

     def repr(): String =
       "(" ++ show(x) ++ ", " ++ show(y) ++ ")"
   }

The constructor syntax `Point { ... }` evaluates all fields before creating the
value.  Because objects are immutable, repeated method calls cannot observe
internal mutations unless those methods declare explicit effects.

Traits and Typeclasses
----------------------

Traits offer reusable behaviour that can be mixed into object types, whereas
typeclasses provide ad-hoc polymorphism over families of types.  Topineur allows
both forms to coexist; use traits for object-style composition and typeclasses
for algebraic laws.

.. code-block:: topineur

   trait Drawable {
     def draw(ctx: Canvas): !{IO} Unit
   }

   impl Drawable for Point {
     def draw(ctx: Canvas): !{IO} Unit =
       ctx.strokeCircle(x, y, 2.0)
   }

The notation `!{IO}` annotates the effect row of a method.  A compiler can use
this to enforce that higher-level APIs acknowledge the possibility of running
input/output effects.

Ownership and Controlled Mutation
---------------------------------

To model mutable state safely, Topineur introduces linear capabilities.  A value
of type ``!lin T`` must be consumed exactly once, guaranteeing unique ownership.

.. code-block:: topineur

   object type MutableCounter {
     value: Int

     def inc(): !{State} MutableCounter =
       MutableCounter { value = value + 1 }
   }

   let counter: !lin MutableCounter = MutableCounter { value = 0 }
   let counter = counter.inc()  // counter is re-bound; no aliases remain

The runtime can detect when an update happens through a linear capability and
optimise it into an in-place mutation without violating the language model.

Effect System
-------------

Effects are tracked through unordered sets called effect rows.  Functions and
methods declare the exact effects they may perform.  Effect inclusion is
structural: a function requiring `!{IO, Network}` cannot be passed to a caller
that only tolerates `!{IO}`.

Important properties:

* Effects compose via union.  A call site must acknowledge the union of callee
  effects.
* Effect masks allow higher-order abstractions to re-expose a subset of effects
  explicitly.
* Pure computations omit the annotation, making purity the default.

Evaluation Strategy
-------------------

Topineur evaluates expressions strictly unless a type or expression is marked
as lazy.  Laziness is opt-in via wrappers such as ``Lazy[T]`` or the ``lazy``
keyword on expressions.  This keeps performance predictable while allowing
declarative deferred computation where it adds value.

Pattern Matching and Interoperability
------------------------------------

Objects can be converted to algebraic data types explicitly using generated
``toADT``/``fromADT`` coercions.  This keeps control flow constructs such as
``match`` precise and maintainable.

.. code-block:: topineur

   match user.toADT() {
     case Guest -> handleGuest()
     case Registered(id, data) -> handleRegistered(id, data)
   }

Because conversions are explicit, cross-module boundaries remain transparent,
which is especially helpful when embedding Topineur into larger polyglot
systems.
