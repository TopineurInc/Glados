# GLaDOS Example Programs

This directory contains example Lisp programs demonstrating GLaDOS capabilities.

## Basic Examples

### `simple.lisp`
Simple arithmetic expression.
```lisp
(+ 2 3)
```
**Expected output:** `5`

### `simple_sum.lisp`
Combining multiple function calls.
```lisp
(+ (factorial 5) (double 10))
```
**Expected output:** `140`

## Recursive Functions

### `factorial.lisp`
Classic factorial function (definition only).
```lisp
(define (fact n)
  (if (eq? n 0)
      1
      (* n (fact (- n 1)))))
```

### `factorial_test.lisp`
Factorial computation with test case.
```lisp
(fact 5)  ; Returns 120
```

## Advanced Examples

### `showcase.lisp`
Multiple algorithms working together:
- Factorial
- Fibonacci
- Power
- GCD
- LCM
- Collatz conjecture
- Sum of squares

**Expected output:** `106`

### `complex_demo.lisp`
Demonstrates:
- Fibonacci
- Power function
- GCD (Euclidean algorithm)
- Ackermann function

**Expected output:** `165`

### `mutual_recursion.lisp`
Mutually recursive functions (is-even/is-odd, Hofstadter sequences).

**Expected output:** `17` or `16` depending on version

### `mega_demo.lisp`
Large-scale demonstration with 11+ functions:
- Factorial
- Power
- Sum operations
- GCD
- Collatz length
- Ackermann function
- Tower of Hanoi
- Triangular numbers

**Expected output:** `139`

### `ultimate_fixed.lisp`
Ultimate stress test combining:
- `factorial(6)` = 720
- `fib(8) * 2` = 42
- `power(3, 5)` = 243
- `gcd(144, 60)` = 12
- `sum-to(15)` = 120
- `is-even(100) * 10` = 10
- Complex nested computations

**Expected output:** `1264`

### `palindrome_research.lisp`
Full-featured benchmark analysing palindromic numbers in a range. Exercises:
- String manipulation (`string-length`, `substring`, `string-append`)
- Boolean logic (`and`, `or`, `not`)
- Tail-recursive accumulation across large search spaces
- Conditional reporting with `cond`, `if`, `when`

**Expected output:** `52351` (plus a detailed textual report)

## Running Examples

```bash
# Run a single example
./glados examples/simple.lisp

# View bytecode
./glados --disasm examples/factorial.lisp

# Run all examples
for f in examples/*.lisp; do
  echo "=== $f ==="
  ./glados "$f"
done
```

## Note on Multi-argument Operators

Currently, operators like `+` and `*` are strictly binary. Use nested expressions:
- ❌ `(+ 1 2 3)` - Not supported
- ✅ `(+ 1 (+ 2 3))` - Correct
