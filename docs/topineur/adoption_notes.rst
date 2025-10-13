Adoption Notes
==============

Strengths
---------

* Pure-by-default semantics make formal reasoning and property testing
  approachable, even for teams used to object-oriented design.
* Controlled mutation through linear capabilities prevents accidental aliasing
  while still enabling efficient updates where they matter.
* Explicit effects improve observability, security review, and sandboxing.
* Composition-first design encourages modular architectures and discourages
  brittle inheritance hierarchies.

Challenges
----------

* The combination of effect rows, linearity, and subtyping introduces a learning
  curve.  Allocate onboarding time for engineers new to these concepts.
* Tooling must be aware of effect annotations and ownership; legacy IDE support
  may require extensions.
* Runtime optimisation (especially in-place updates) demands careful
  engineering to avoid surprising pauses or contention.

Migration Strategy
------------------

1. **Identify Pure Domains:** start by modelling immutable domain objects where
   mutation is not required.  This builds confidence in the syntax and semantics.
2. **Wrap Impure Services:** encapsulate existing side-effectful services behind
   traits and annotate the necessary effect rows.
3. **Introduce Linear Capabilities Gradually:** begin with narrow scopes (for
   example, counters or connection pools) before rewriting large mutable
   subsystems.
4. **Leverage Actors:** use actors to migrate stateful services that currently
   rely on shared locks or global variables.
5. **Audit Effects:** integrate effect inspection into CI so that pull requests
   cannot accidentally broaden publicly exposed effect sets.
