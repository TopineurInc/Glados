# Topineur Standard Library

The `stdlib/` directory gathers reusable Topineur modules that embrace the language's expression-oriented style. Each module is a plain `.top` file that you can import from user code; the compiler resolves dotted import paths (e.g. `import std.collections.list`) to files beneath `stdlib/`.

## Module Layout

```
stdlib/
  std/
    core.top
    string.top
    math.top
    collections/
      list.top
      aliases.top
      functional.top
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

### std.collections.aliases

Convenient short aliases for common list operations:

| Symbol | Description |
|--------|-------------|
| `length(lst)` | Alias for `__list_length`. Returns the number of elements in a list. |
| `head(lst)` | Alias for safe first element access. Returns `nil` if list is empty. |
| `tail(lst)` | Returns all elements except the first. Returns `[]` if list is empty or has one element. |
| `concat(left, right)` | Alias for `__list_append`. Concatenates two lists. |
| `last(lst)` | Alias for safe last element access. Returns `nil` if list is empty. |

### std.collections.functional

Higher-order list functions for functional programming:

| Symbol | Description |
|--------|-------------|
| `fold(lst, init, func)` | Reduces a list to a single value using an accumulator function. Left fold. |
| `map(lst, func)` | Transforms each element of a list using the provided function. |
| `filter(lst, predicate)` | Returns a new list containing only elements that satisfy the predicate. |
| `reverse(lst)` | Returns a new list with elements in reverse order. |
| `take(lst, n)` | Returns the first `n` elements of the list. |
| `drop(lst, n)` | Returns all elements after dropping the first `n`. |
| `find(lst, predicate)` | Returns the first element matching the predicate, or `nil` if none found. |
| `contains(lst, value)` | Returns `true` if the list contains the value, `false` otherwise. |
| `index_of(lst, value)` | Returns the index of the first occurrence of value, or `-1` if not found. |
| `range(start, end)` | Generates a list of integers from `start` to `end` (inclusive). |
| `zip(lst1, lst2)` | Zips two lists into a list of pairs up to the shorter length. |

### std.string

Convenience wrappers around the existing string builtins:

| Symbol | Description |
|--------|-------------|
| `string_is_empty` | Checks whether a string has length zero. |
| `string_starts_with` / `string_ends_with` | Prefix and suffix checks expressed via `__substring`. |

## Built-in Functions

The Topineur runtime provides several built-in functions implemented in Haskell:

### Math Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `abs(x)` | `Int\|Float -> Int\|Float` | Returns the absolute value. Preserves input type. |
| `sqrt(x)` | `Int\|Float -> Float` | Returns the square root. Always returns Float. |
| `pow(a, b)` | `Int\|Float, Int\|Float -> Float` | Exponentiation. Returns Float. |
| `floor(x)` | `Int\|Float -> Int` | Largest integer not greater than `x`. For Int, passthrough. |
| `ceil(x)` | `Int\|Float -> Int` | Smallest integer not less than `x`. For Int, passthrough. |
| `round(x)` | `Int\|Float -> Int` | Rounds to the nearest integer (banker's rounding as in Haskell). For Int, passthrough. |
| `sin(x)` | `Int\|Float -> Float` | Sine of `x` in radians. |
| `cos(x)` | `Int\|Float -> Float` | Cosine of `x` in radians. |
| `tan(x)` | `Int\|Float -> Float` | Tangent of `x` in radians. |
| `asin(x)` | `Int\|Float -> Float` | Arc-sine in radians. Domain [-1,1]. |
| `acos(x)` | `Int\|Float -> Float` | Arc-cosine in radians. Domain [-1,1]. |
| `atan(x)` | `Int\|Float -> Float` | Arc-tangent in radians. |
| `atan2(y, x)` | `Int\|Float, Int\|Float -> Float` | Arc-tangent of `y/x` accounting for quadrant, in radians. |
| `pi` | `Float` | π constant (3.14159…). |
| `e` | `Float` | Euler's number e (2.71828…). |

### std.math (Topineur)

Convenience numeric utilities implemented in Topineur:

| Function | Signature | Description |
|----------|-----------|-------------|
| `min(a, b)` | `Int\|Float -> Int\|Float` | Returns the smaller of two numbers. |
| `max(a, b)` | `Int\|Float -> Int\|Float` | Returns the larger of two numbers. |
| `clamp(x, lo, hi)` | `Int\|Float -> Int\|Float` | Clamps `x` into `[lo, hi]` using `max(lo, min(x, hi))`. |
| `sign(x)` | `Int\|Float -> Int` | Returns `-1`, `0`, or `1`. |

### Type Conversions

| Function | Signature | Description |
|----------|-----------|-------------|
| `int(x)` | `Int\|Float\|String -> Int` | Converts to Int. Truncates floats towards zero. |
| `float(x)` | `Int\|Float\|String -> Float` | Converts to Float. |
| `show(x)` | `Any -> String` | Converts any value to its string representation. |

**Conversion behaviors:**
- `int(3.9)` → `3` (truncate towards zero)
- `int(-3.9)` → `-3` (truncate towards zero)
- `float(42)` → `42.0`
- `abs(-5)` → `5` (preserves Int type)
- `abs(-3.14)` → `3.14` (preserves Float type)
- `sqrt(16)` → `4.0` (always Float)

### List Primitives

| Function | Description |
|----------|-------------|
| `__list_length(lst)` | Runtime primitive for list length. |
| `__list_get(lst, idx)` | Runtime primitive for element access by index. |
| `__list_append(left, right)` | Runtime primitive for list concatenation. |
| `__list_single(value)` | Runtime primitive to create a single-element list. |

### String Primitives

| Function | Description |
|----------|-------------|
| `__string_length(str)` | Runtime primitive for string length. |
| `__string_append(s1, s2)` | Runtime primitive for string concatenation. |
| `__substring(str, start, end)` | Runtime primitive to extract substring. |

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
