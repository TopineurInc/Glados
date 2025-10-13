Tooling
=======

Effect Inspector
----------------

The effect inspector produces call graphs annotated with effect rows.  Typical
workflows:

* audit whether a public API unintentionally escalates permissions;
* highlight functions that can be rewritten as pure helpers;
* surface opportunities to narrow effect sets and improve reuse.

Refactoring Support
-------------------

Because methods are pure by default, refactorings such as extraction or
movement preserve semantics as long as the effect annotations remain identical.
IDE integrations should refuse to complete a refactor that would broaden an
effect row without explicit developer confirmation.

Interop Toolchain
-----------------

Topineur offers straightforward bindings to ecosystems that already consume
objects.  Generated shims follow three rules:

1. Methods marked pure compile to simple function exports.
2. Methods with effect rows produce wrappers that reify those effects for the
   host runtime (for instance, returning ``Promise`` objects on JavaScript).
3. Linear capabilities cannot cross the FFI boundary without being wrapped in a
   safe handle that enforces single-use semantics.

REPL and Debugging
------------------

The interactive shell keeps history of object snapshots.  Developers can diff
two immutable object values to see how method calls transform state.  When
linear capabilities are consumed, the REPL marks the prior value as invalid to
mirror compile-time guarantees.

VS Code Extension
-----------------

An editor bundle lives under ``vscode/topineur``.  Package it with ``vsce`` or
load it through VS Code's extension development host to obtain syntax
highlighting, ``.top`` file association, and basic auto-closing behaviour.  The
grammar file (``syntaxes/topineur.tmLanguage.json``) is the canonical place to
extend highlighting rules as the language evolves.
