Overview
========

Motivation
----------

Topineur addresses the long-standing tension between pure functional languages
and mainstream object-oriented ecosystems.

* Pure languages such as Haskell offer mathematical reasoning, rich typeclass
  abstractions, and explicit control of effects, yet modelling encapsulated,
  stateful domains can feel unnatural or verbose.
* Classical OOP languages make encapsulation straightforward, but pervasive
  mutation and implicit effects complicate reasoning and introduce latent bugs.

**Goal:** provide *objects as immutable values*, *pure methods by default*, and
*explicit, tractable effects* while still supporting pragmatic, controlled
mutation where it matters.  Topineur keeps equational reasoning intact and lets
teams represent intricate domains with familiar object shapes.  Source files use
the ``.top`` extension to make the paradigm stand out from its Lisp roots.

Core Principles
---------------

1. Objects are first-class values (records with data and behaviour) that are
   immutable by default.
2. Methods are pure unless they explicitly declare the effects they need.
3. Mutation is permitted only through linear capabilities that guarantee
   single-ownership semantics.
4. Polymorphism combines typeclasses (algebraic behaviour) and traits (object
   composition) without forcing a single model.
5. Composition and delegation replace deep inheritance; mixins remain pure and
   stateless.
6. Effects are tracked through effect rows, making side-effects inspectable and
   auditable.
7. Evaluation is strict by default with opt-in laziness for targeted use.
8. Conversion between algebraic data types (ADT) and object types is explicit,
   encouraging clarity at module boundaries.

Guiding Ideas
-------------

* Encourage data-oriented designs that feel like objects without the pitfalls
  of uncontrolled mutation.
* Provide predictable interoperability with existing functional or object
  modules by keeping conversion explicit and well-typed.
* Make effect discipline a first-class tool for testability and security.
* Treat ownership as a language-level concept so that mutation becomes safe,
  compositional, and optimisable by the runtime.
