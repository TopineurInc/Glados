# GLaDOS Test Suite

Automated testing suite for the GLaDOS Lisp compiler.

## Test Scripts

### `tester.py` - Complete Test Suite
Runs 36 comprehensive tests covering all GLaDOS features.

**Usage:**
```bash
python3 tests/tester.py           # Run all tests
python3 tests/tester.py -v        # Verbose mode
```

**Test Coverage:**
- ✅ Basic arithmetic operations (+, -, *, div, mod)
- ✅ Conditional expressions (if, eq?, <, >)
- ✅ Recursive functions (factorial, fibonacci, power, gcd)
- ✅ Mutual recursion (is-even/is-odd)
- ✅ Higher-order functions
- ✅ Complex nested expressions

### `tester_detailed.py` - Detailed Analysis
Provides in-depth analysis of failing tests with bytecode inspection.

**Usage:**
```bash
python3 tests/tester_detailed.py --quick      # Quick pattern analysis
python3 tests/tester_detailed.py --analysis   # Full bytecode analysis
```

## Current Test Results

**Success Rate: 100%** (36/36 tests passing)

All core features are working correctly:
- ✅ Factorial(5) = 120
- ✅ Fibonacci(10) = 55
- ✅ Power(3, 4) = 81
- ✅ GCD(48, 18) = 6
- ✅ Mutual recursion works perfectly
- ✅ Complex nested function calls

## Example Programs

See the `examples/` directory for demonstration programs:
- `simple.lisp` - Basic arithmetic
- `factorial_test.lisp` - Factorial function
- `showcase.lisp` - Multiple algorithms
- `mutual_recursion.lisp` - Mutually recursive functions
- `ultimate_fixed.lisp` - Complex multi-function computation
