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

## SFML Builtins (Preview)

Experimental SFML bindings are available with the following builtins:

- **Windows**
  - `sfml-create-window` `(width height title)`
  - `sfml-window-clear` `(window r g b)`
  - `sfml-window-draw` `(window shape)`
  - `sfml-window-display` `(window)`
  - `sfml-window-is-open` `(window)`
  - `sfml-close-window` `(window)`
- **Shapes**
  - `sfml-create-rectangle` `(width height)`
  - `sfml-create-circle` `(radius)`
  - `sfml-shape-set-position` `(shape x y)`
  - `sfml-shape-set-fill-color` `(shape r g b)`
- **Helpers**
  - `sfml-draw-square` `(window size x y r g b)` — one-shot demo loop

Simple render loop:

```lisp
(define (render-loop window shape)
  (if (sfml-window-is-open window)
      (begin
        (sfml-window-clear window 25 25 35)
        (sfml-window-draw window shape)
        (sfml-window-display window)
        (render-loop window shape))
      (sfml-close-window window)))

(define (main)
  (let ((window (sfml-create-window 800 600 "SFML demo"))
        (rect (sfml-create-rectangle 180 120)))
    (begin
      (sfml-shape-set-position rect 310 240)
      (sfml-shape-set-fill-color rect 255 120 0)
      (render-loop window rect))))
```

> **Note**
> Building or running the SFML builtins requires the native CSFML libraries (`csfml-system`, `csfml-window`, `csfml-graphics`, `csfml-audio`, `csfml-network`) to be installed on your machine.

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
