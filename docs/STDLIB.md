# Topineur Standard Library

The `stdlib/` directory gathers reusable Topineur modules that embrace the language's expression-oriented style. Each module is a plain `.top` file that you can import from user code; the compiler resolves dotted import paths (e.g. `import std.collections.list`) to files beneath `stdlib/`.

## Module Layout

```
stdlib/
  std/
    core.top
    string.top
    collections/
      list.top
```

### std.core

Functional helpers that work on any value:

| Symbol | Description |
|--------|-------------|
| `core_identity` | Returns its argument unchanged. |
| `core_const` | Ignores the second argument, returning the first. Useful when composing callbacks. |
| `core_tap` | Applies a side-effecting function to a value and then returns the original value. |
| `core_apply_twice` | Applies a function twice to the same value. |
| `core_pipe` | Pipelining helper: `core_pipe(x, f)` is the same as `f(x)`. |

### std.collections.list

Utilities layered on top of the VM's native list support (`VList`, `IListGet`, `IListCreate`):

| Symbol | Description |
|--------|-------------|
| `list_length` | Wrapper around the runtime primitive for retrieving the list size. |
| `list_is_empty` | Boolean predicate built from `list_length`. |
| `list_head` / `list_last` | Safe accessors that fall back to `nil` when the list is empty. |
| `list_concat` | Delegates to the runtime concatenation primitive. |
| `list_single` | Creates a one-element list. |

At this stage higher-order helpers (`map`, `filter`, …) are earmarked for a future iteration once the parser grows richer list syntax.

### std.string

Convenience wrappers around the existing string builtins:

| Symbol | Description |
|--------|-------------|
| `string_is_empty` | Checks whether a string has length zero. |
| `string_starts_with` / `string_ends_with` | Prefix and suffix checks expressed via `__substring`. |

## Using the Stdlib

```topineur
package main

import std.core
import std.collections.list
import std.string

def main(): Int {
  let words = ["Top", "Ineur"]
  let sentence = list_join(words, "-")

  let shouted = string_pad_right(sentence, 8, "!")

  let duplicated = core_pipe(3, fun (value: Int): Int -> core_apply_twice(value, fun (n: Int): Int -> n + 1))

  println(sentence)
  println(shouted)
  println(show(duplicated))
  top 0
}
```

Imports are idempotent: the compiler caches modules and their dependencies, so repeated imports are cheap. All functions follow the naming convention `<module>_<name>` to avoid collisions in the global namespace until the language gains first-class modules.

## Extending the Library

1. Add a new `.top` file under `stdlib/`.
2. Use expression-oriented, side-effect minimal code unless the API explicitly provides IO.
3. Document the new module and symbols in this file.
4. Update tests or examples to exercise the new functionality.
5. When adding primitives (files, time, sockets), introduce a Haskell builtin in `Builtins.hs`, declare its arity in `VM.builtinArity`, and expose a thin Topineur wrapper.
