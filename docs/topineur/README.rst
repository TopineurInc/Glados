Topineur Documentation
======================

This directory contains the complete documentation for the Topineur programming
language — an object-functional language with explicit effects and safe
ownership.

Document Map
------------

Start Here
~~~~~~~~~~

**overview.rst**
  High-level introduction to Topineur's motivation, core principles, and
  guiding ideas. Read this first to understand why Topineur exists.

**transformation_plan.rst**
  Complete roadmap for transforming the project from Lisp to Topineur. This is
  the master plan for the migration, with detailed phases and timelines.

Language Specification
~~~~~~~~~~~~~~~~~~~~~~

**grammar.rst** (MANDATORY for Part 2)
  Formal grammar in Extended Backus-Naur Form (EBNF). Defines the complete
  syntax of Topineur including operator precedence, syntactic sugar, and
  parsing rules.

**language_reference.rst**
  Technical reference covering object types, traits, effects, ownership, and
  the type system. Compact and precise — intended for implementers.

User Documentation
~~~~~~~~~~~~~~~~~~

**user_manual.rst**
  Comprehensive guide for writing Topineur programs. Covers all language
  features with examples, standard library, common patterns, and performance
  tips. Start here if you want to learn the language.

Compiler Documentation
~~~~~~~~~~~~~~~~~~~~~~

**compilation.rst** (MANDATORY for Part 2)
  Detailed description of the compilation pipeline from source to bytecode.
  Explains all 12 compilation phases, intermediate representations, bytecode
  format, and VM architecture.

Security Documentation
~~~~~~~~~~~~~~~~~~~~~~

**security_review.rst** (MANDATORY for Part 2)
  Analysis of Topineur's security features compared to inspiration languages
  (Haskell, Rust, ML, Scala). Covers memory safety, type safety, effect
  tracking, linear types, and security best practices.

Application Documentation
~~~~~~~~~~~~~~~~~~~~~~~~~

**use_cases.rst**
  Real-world scenarios where Topineur excels: reactive systems, financial
  applications, data pipelines, embedded systems, and distributed systems.

**adoption_notes.rst**
  Practical guide for teams considering Topineur. Covers migration strategies,
  training, tooling, and comparisons with other languages.

Tooling Documentation
~~~~~~~~~~~~~~~~~~~~~

**tooling.rst**
  Development tools ecosystem: compiler flags, IDE support (VSCode extension),
  debugger, profiler, package manager, and build tools.

**runtime_and_effects.rst**
  Runtime system details: effect handlers, concurrency model (actors),
  garbage collection, and performance characteristics.

Reading Order by Goal
----------------------

For Implementers
~~~~~~~~~~~~~~~~

If you're implementing the Topineur compiler/VM, read in this order:

1. ``overview.rst`` — Understand the vision
2. ``transformation_plan.rst`` — Follow the implementation roadmap
3. ``grammar.rst`` — Implement the parser
4. ``language_reference.rst`` — Understand type system and effects
5. ``compilation.rst`` — Implement the compiler pipeline
6. ``security_review.rst`` — Understand safety guarantees

For Language Users
~~~~~~~~~~~~~~~~~~

If you're learning to write Topineur programs, read in this order:

1. ``overview.rst`` — Understand the philosophy
2. ``user_manual.rst`` — Learn the language features
3. ``use_cases.rst`` — See real-world examples
4. ``tooling.rst`` — Set up your development environment

For Project Evaluators (Defense)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you're evaluating this project for Part 2 of GLaDOS:

**MANDATORY documents:**

1. ``grammar.rst`` — Formal grammar (BNF) ✓
2. ``compilation.rst`` — Compilation process ✓
3. ``security_review.rst`` — Security analysis ✓

**Highly recommended:**

4. ``user_manual.rst`` — User documentation
5. ``transformation_plan.rst`` — Implementation strategy
6. ``overview.rst`` — Core principles

Documentation Quality Standards
--------------------------------

All documentation in this directory follows these standards:

**Accessibility**

- Written in clear, simple English
- Structured with logical headings
- Includes table of contents for long documents
- Uses consistent formatting
- Screen reader friendly (proper heading hierarchy)
- No unnecessary jargon

**Completeness**

- Covers all language features
- Includes examples for every concept
- Provides error message guidance
- Explains design decisions

**Accuracy**

- Kept in sync with implementation
- Reviewed for technical correctness
- Tested examples actually compile

**Navigability**

- Cross-references between documents
- Clear document map (this file)
- Consistent terminology
- Index of key concepts

Key Concepts Index
------------------

Find information on key topics:

**Effect System**
  - Overview: ``overview.rst`` section "Core Principles"
  - Reference: ``language_reference.rst`` section "Effect System"
  - User guide: ``user_manual.rst`` section "Effect System"
  - Security: ``security_review.rst`` section "Effect Safety"
  - Compilation: ``compilation.rst`` section "Phase 5: Effect Checking"

**Linear Types / Ownership**
  - Overview: ``overview.rst`` section "Core Principles"
  - Reference: ``language_reference.rst`` section "Ownership and Controlled Mutation"
  - User guide: ``user_manual.rst`` section "Linear Types and Ownership"
  - Security: ``security_review.rst`` section "Linear Types and Ownership"
  - Compilation: ``compilation.rst`` section "Phase 6: Linearity Checking"

**Object Types**
  - Overview: ``overview.rst`` section "Guiding Ideas"
  - Reference: ``language_reference.rst`` section "Object Types"
  - User guide: ``user_manual.rst`` section "Object Types"
  - Grammar: ``grammar.rst`` section "Object Definitions"
  - Compilation: ``compilation.rst`` section "Phase 8: Object Desugaring"

**Traits**
  - Reference: ``language_reference.rst`` section "Traits and Typeclasses"
  - User guide: ``user_manual.rst`` section "Traits and Implementations"
  - Grammar: ``grammar.rst`` section "Trait Definitions"
  - Compilation: ``compilation.rst`` section "Phase 7: Trait Resolution"

**Pattern Matching**
  - User guide: ``user_manual.rst`` section "Pattern Matching"
  - Grammar: ``grammar.rst`` section "Control Flow"
  - Security: ``security_review.rst`` section "Exhaustive Pattern Matching"

**Actors / Concurrency**
  - Reference: ``language_reference.rst`` (mentioned)
  - User guide: ``user_manual.rst`` section "Concurrency with Actors"
  - Security: ``security_review.rst`` section "Safe Concurrency with Actors"
  - Runtime: ``runtime_and_effects.rst``

**Type System**
  - Reference: ``language_reference.rst`` section "Effect System"
  - Grammar: ``grammar.rst`` section "Type System"
  - Compilation: ``compilation.rst`` section "Phase 4: Type Checking"

Part 2 Requirements Checklist
------------------------------

This section verifies that all Part 2 documentation requirements are met:

✓ **Formal grammar (BNF)** — ``grammar.rst``
  Complete EBNF grammar with operator precedence, syntactic sugar, and examples.

✓ **User manual** — ``user_manual.rst``
  Comprehensive guide with getting started, all features, standard library,
  and common patterns.

✓ **Compilation process** — ``compilation.rst``
  All 12 compilation phases explained, intermediate representations documented,
  bytecode format specified.

✓ **Security review** — ``security_review.rst``
  Analysis of inspiration languages (Haskell, Rust, ML, Scala), security
  features implemented, known limitations, and best practices.

✓ **Accessibility** — All documents
  Clear language, logical structure, screen reader friendly, no unnecessary
  barriers.

✓ **Language evolution** — ``transformation_plan.rst``
  Shows clear evolution from Part 1 (Lisp) to Part 2 (Topineur) across all
  four axes: security, parsing, evaluation, and documentation.

Contributing to Documentation
------------------------------

When updating documentation:

1. Keep RST formatting consistent
2. Test code examples to ensure they compile
3. Update cross-references when moving content
4. Maintain the document map (this file)
5. Run accessibility checks
6. Keep language clear and simple

**Style guide:**

- Use present tense ("Topineur provides" not "Topineur will provide")
- Use active voice ("The compiler checks types" not "Types are checked")
- Define terms before using them
- Include examples for abstract concepts
- Link to other documents when referencing concepts

Documentation Tools
-------------------

**Recommended tools for viewing:**

- Sphinx (generates HTML from RST)
- rst2html (quick preview)
- Any text editor (RST is human-readable)

**Generating HTML documentation:**

.. code-block:: bash

   cd docs/topineur
   sphinx-build -b html . _build/html
   open _build/html/index.html

**Checking links:**

.. code-block:: bash

   sphinx-build -b linkcheck . _build/linkcheck

Version History
---------------

- v0.1 (2025-10-14) — Initial Topineur documentation suite
  - Created formal grammar, user manual, compilation guide, security review
  - Migrated from Lisp documentation
  - Added transformation plan

Related Documentation
---------------------

**Project-level documentation** (in ``/docs``):

- ``ARCHITECTURE.md`` — Overall project structure (needs update for Topineur)
- ``DEVELOPMENT.md`` — Build and test workflows
- ``INSTRUCTIONS.md`` — VM instruction reference (needs update for Topineur)

**External resources:**

- Topineur examples: ``/examples/topineur/``
- VSCode extension: ``/vscode/topineur/``
- Test suite: ``/test/`` (to be updated for Topineur)

Contact and Feedback
--------------------

For questions or feedback about the documentation:

- File an issue on the project repository
- Suggest improvements via pull requests
- Report errors or unclear sections

License
-------

This documentation is released under the same license as the GLaDOS project
(MIT License). See ``LICENSE`` in the project root.
