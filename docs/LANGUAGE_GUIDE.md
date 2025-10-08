# Language Guide

GLaDOS was born as the answer to the EPITECH project **B-FUN-500_GLaDOS**: build your own language and runtime in Haskell. This guide is the formal description of the language we ended up with. It covers the programming model, the grammar, the evaluation rules, and the tooling you need to be productive.

---

## 0. Philosophy & paradigms

| Trait | Description |
|-------|-------------|
| **Paradigm** | Expression-oriented, higher-order, dynamically typed Lisp. |
| **Evaluation** | Strict call-by-value (applicative order). Arguments are evaluated before function bodies run. |
| **Scoping** | Lexical binding with automatic alpha-renaming; closures capture by value. |
| **Side effects** | Allowed through builtins (`print`, `input`, `format`, …). Pure functions remain first-class. |
| **Tail calls** | Tail positions compile to `ITailCall`, so infinite recursion in tail form is safe. |
| **Truthiness** | Only `#f` is false. `nil` aliases `#f`. Everything else is true. |

Think of it as “Scheme-with-training-wheels”: we keep the minimal core but favour explicitness so the compilation pipeline stays approachable.

---

## 1. Grammar reference (EBNF)

The reader accepts symbolic S-expressions with comments introduced by `;`. Whitespace is insignificant outside strings.

```ebnf
program      ::= form*

form         ::= definition | expression

definition   ::= "(" "define" defn-body ")"

defn-body    ::= "(" identifier+ ")" expression   (* function definition sugar *)
               | identifier expression            (* plain binding *)

expression   ::= literal
               | identifier
               | quote
               | lambda
               | if
               | begin
               | let
               | letrec
               | application

literal      ::= integer | boolean | string
integer      ::= ["-"] digit+
boolean      ::= "#t" | "#f"
string       ::= '"' character* '"'
quote        ::= "(" "quote" datum ")"
datum        ::= literal | identifier | "(" datum* ")"      (* raw S-expression *)

lambda       ::= "(" "lambda" "(" identifier* ")" expression ")"
if           ::= "(" "if" expression expression expression ")"
begin        ::= "(" "begin" expression+ ")"
let          ::= "(" "let" "(" binding+ ")" expression+ ")"
letrec       ::= "(" "letrec" "(" binding+ ")" expression+ ")"
binding      ::= "(" identifier expression ")"

application  ::= "(" expression expression* ")"   (* function call *)
identifier   ::= initial subsequent*
initial      ::= letter | "+" | "-" | "*" | "/" | "<" | ">" | "=" | "?" | "!"
subsequent   ::= initial | digit | "-"
comment      ::= ";" any* ( "\n" | "\r" | EOF )
```

Syntactic sugar:

- `'datum` rewrites to `(quote datum)`
- `(define (f args) body)` rewrites to `(define f (lambda (args) body))`
- The built-in macros `when`, `unless`, and `cond` expand to `if` forms.

Minimal “hello world”:

```lisp
(define (main)
  (format #t "Hello from GLaDOS!~%"))
```

---

## 2. Data model

| Runtime type | Literal form | Notes |
|--------------|--------------|-------|
| Integer (`VInt`) | `42`, `-7` | Arbitrary precision integers backed by Haskell `Integer`. |
| Boolean (`VBool`) | `#t`, `#f`, `t`, `nil` | Only `#f` (and `nil`) are falsey. |
| String (`VString`) | `"hello\nworld"` | Escape sequences: `\"`, `\\`, `\n`, `\t`, `\r`. |
| Closure (`VClosure`) | produced by `lambda` | Captures free variables lexically. |
| Builtin (`VBuiltin`) | provided by runtime | See §6 for the catalogue. |

Lists are represented as quoted S-expressions (`'(1 2 3)`) or via Church-encoded pairs (see §7). Native list cells are not part of the core runtime yet.

---

## 3. Scoping, bindings, and environments

- **Lexical scope**: bindings introduced by `lambda`, `let`, and `letrec` are visible in their static textual region.
- **`define` at top level**: captured in an implicit `letrec`; mutually recursive definitions work naturally.
- **Shadowing**: local bindings shadow outer ones. The compiler alpha-renames everything to prevent accidental capture during closure conversion.
- **Closures**: created with `lambda`. Free variables are copied into the closure environment when the function is defined, not when it runs.
- **Recursion**: supported through `define`, `letrec`, or self-referential lambdas (`(define fact (lambda (n) ... fact ...))`).

---

## 4. Evaluation order

1. Evaluate the operator expression (the function position in a call).
2. Evaluate arguments left-to-right.
3. Apply the resulting function:
   - Builtin → call the Haskell implementation.
   - Closure → allocate a frame, bind parameters, run the body.
4. Return values replace the call expression. Tail positions reuse the current frame.

Special forms (`if`, `lambda`, `quote`, `define`, `let`, `letrec`, `begin`) control evaluation explicitly and do **not** evaluate all arguments eagerly. For example, `if` only evaluates the selected branch.

---

## 5. Core syntax & semantics

### 5.1 Definitions

```lisp
(define answer 42)              ; binds a constant
(define (square x) (* x x))     ; sugar for lambda binding
```

- Top-level `define`s are available to subsequent forms.
- Inner `define`s (inside `letrec` or function bodies) behave like `letrec`.

### 5.2 Conditionals

```lisp
(if cond then else)
```

- `cond` is evaluated once.
- If it yields `#f`, the else branch runs; otherwise the then branch.
- Use the `cond` macro for multi-branch dispatch (expands to nested `if`).

### 5.3 Sequencing

```lisp
(begin expr₁ expr₂ … exprₙ)
```

- Evaluates each expression in order.
- Returns the result of `exprₙ`.
- Useful for composing side-effecting calls (`display`, `print`, …).

### 5.4 Local bindings

```lisp
(let ((name value) …) body)       ; non-recursive
(letrec ((self expr) …) body)     ; recursive/mutually recursive
```

- `let` evaluates all value expressions, binds them simultaneously, then runs the body.
- `letrec` is for recursive definitions; bindings are visible within their own value expressions.

### 5.5 Functions & closures

```lisp
(lambda (arg₁ … argₙ) body)
```

- Creates a closure capturing free variables.
- Functions are first-class: pass them as arguments, store in variables, return them.
- Tail-call optimisation kicks in automatically when the last expression of a function is a call whose result is returned directly.

### 5.6 Quoting & symbols

```lisp
'(1 2 3)        ; data, not code
'foo            ; symbol literal
`(1 ,(+ 1 1) 3) ; quasiquote not implemented (write macros instead)
```

Currently only plain `quote` is supported; quasiquote is a possible future extension.

### 5.7 Comments

Any `;` outside a string starts a comment until end-of-line. Comments are discarded by the reader.

---

## 6. Builtin library

### Arithmetic & comparison
`+`, `-`, `*`, `div`, `mod`, `eq?`, `<`, `>`.

### Boolean logic
`not`, `and`, `or`.

### Strings & I/O
`display`, `print`, `input`, `read-line`, `string-length`, `string-append`, `substring`, `string->number`, `number->string`, `format`.

### Constants
`t` → `#t`, `nil` → `#f`.

All builtins signal informative `Type error:` messages when misused. They run in IO, so expect side effects (printing, reading) where relevant.

---

## 7. Working with lists & data structures

Native list cells are not provided yet; you can implement them using Church encoding:

```lisp
(define (cons a b)
  (lambda (pick) (pick a b)))

(define (car pair) (pair (lambda (x y) x)))
(define (cdr pair) (pair (lambda (x y) y)))
```

For convenience in tests and examples we often rely on quoted S-expressions (`'(a b c)`) and pattern-match on them with macros.

---

## 8. Macros

`defaultMacroEnv` ships with:

| Macro | Intent | Expansion |
|-------|--------|-----------|
| `when` | Guarded execution | `(if cond body #f)` |
| `unless` | Negated guard | `(if cond #f body)` |
| `cond` | Multi-clause branching | Nested `if`s with optional `else` |

Add new macros by editing `src/MacroExpander.hs`. Macros manipulate raw S-expressions, so they can introduce new syntactic sugar without touching the parser.

---

## 9. Error handling

- **Compile time**: parsing, macro expansion, desugaring, renaming, or closure conversion errors surface as `SyntaxError` / `ParseError` with source positions when available.
- **Runtime**: the VM reports `StackUnderflow`, `UndefinedFunction`, `TypeError`, or generic `RuntimeError`. Builtins use descriptive messages (`"Type error: string-append expects two strings"`).
- Execution stops on the first error. The CLI propagates failures with exit code `84`, per the subject specification.

---

## 10. Patterns, idioms & style

- Prefer pure functions; isolate IO in thin wrappers that call `display`, `print`, or `input`.
- For state machines, use tail-recursive helpers instead of mutation.
- Compose macros to introduce domain-specific sugar—e.g. a `match` macro over quoted lists.
- Use `letrec` to define mutually recursive helpers without polluting the global namespace.
- When debugging, inspect the generated bytecode with `./glados --disasm program.lisp` to ensure your code expands the way you expect.

---

## 11. Inspiration & security notes

The language takes cues from Scheme (minimalism, lexical scope) and Common Lisp (formatting functions, `t` / `nil`). We deliberately avoid mutable state and unsafe primitives to keep evaluation predictable. Input functions (`input`, `read-line`) operate on standard IO only; there is no direct filesystem or network access, which keeps the runtime sandboxed unless the host application exposes more builtins.

If you extend the language with new primitives, keep the `Type error` discipline and fail fast on invalid inputs—the VM assumes builtins preserve its invariants.

---

That’s the authoritative description of GLaDOS-the-language. Pair it with `docs/ARCHITECTURE.md` to understand how the compiler lowers these constructs to bytecode, and `docs/DEVELOPMENT.md` for build/testing workflows. Happy hacking!
