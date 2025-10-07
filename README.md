# GLaDOS - Lisp Compiler & Virtual Machine

A complete Lisp compiler implementation with bytecode generation and stack-based VM execution.

## Quick Start

```bash
# Build
make

# Run a program
./glados examples/factorial_test.lisp

# View bytecode
./glados --disasm examples/factorial_test.lisp

# Run tests
python3 tests/tester.py
```

## Features

- ✅ Full Lisp support (define, lambda, if, recursion)
- ✅ Mutually recursive functions
- ✅ Tail call optimization
- ✅ Stack-based bytecode VM
- ✅ Comprehensive test suite (36/36 tests passing)

## Examples

```lisp
; Factorial
(define (fact n)
  (if (eq? n 0)
      1
      (* n (fact (- n 1)))))
(fact 5)  ; Result: 120
```

```lisp
; Fibonacci
(define (fib n)
  (if (eq? n 0) 0
      (if (eq? n 1) 1
          (+ (fib (- n 1)) (fib (- n 2))))))
(fib 10)  ; Result: 55
```

More examples in `examples/` directory.

## Documentation

- **[docs/ARCHITECTURE.md](docs/ARCHITECTURE.md)** - Compiler architecture and design
- **[docs/INSTRUCTIONS.md](docs/INSTRUCTIONS.md)** - VM instruction set reference
- **[docs/CAHIER_DES_CHARGES.md](docs/CAHIER_DES_CHARGES.md)** - Cahier des charges du sous-ensemble Lisp supporté
- **[examples/README.md](examples/README.md)** - Example programs guide
- **[tests/README.md](tests/README.md)** - Testing documentation

## Project Structure

```
glados/
├── src/              # Compiler & VM source code
│   ├── AST.hs           # Data type definitions
│   ├── SExprParser.hs   # Parser
│   ├── Compiler.hs      # Compilation pipeline
│   ├── CodeGen.hs       # Bytecode generator
│   └── VM.hs            # Virtual machine
├── app/              # Main executable
├── examples/         # Example Lisp programs
├── tests/            # Test suite (Python)
└── docs/             # Technical documentation
```

## Testing

Run the comprehensive test suite:

```bash
python3 tests/tester.py           # Full test suite
python3 tests/tester_detailed.py  # Detailed analysis
```

**Current status:** ✅ 100% (36/36 tests passing)

## License

See LICENSE file for details.
