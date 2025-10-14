Topineur Security Review
========================

This document analyzes the security features of Topineur and compares them to
its inspiration languages: Haskell, Rust, ML, and Scala.

Executive Summary
-----------------

Topineur achieves language-based security through:

1. **Type safety** — prevents undefined behavior and type confusion
2. **Effect system** — makes side effects explicit and auditable
3. **Linear types** — prevents use-after-free, double-free, and data races
4. **Immutability by default** — reduces state-related bugs
5. **No null references** — eliminates null pointer exceptions
6. **Memory safety** — garbage collection prevents manual memory errors

**Security posture:** Topineur provides strong safety guarantees comparable to
Rust (ownership) and Haskell (purity), while maintaining the ergonomics of ML
and Scala.

Threat Model
------------

We consider the following security threats:

**Memory safety threats:**
  - Buffer overflows
  - Use-after-free
  - Double-free
  - Dangling pointers
  - Uninitialized memory access

**Type safety threats:**
  - Type confusion attacks
  - Integer overflow/underflow
  - Unchecked casts

**Concurrency threats:**
  - Data races
  - Deadlocks
  - Use-after-move in concurrent contexts

**Information security threats:**
  - Information leakage through side channels
  - Unauthorized side effects
  - Capability leakage

Inspiration Languages Review
----------------------------

Haskell
~~~~~~~

**Security strengths:**

- **Purity by default:** Functions without IO in their type cannot perform side
  effects, making information flow auditable.
- **Strong type system:** Prevents type confusion and invalid operations.
- **Lazy evaluation:** Can prevent unnecessary computation, but also introduces
  space leaks.
- **No null:** Uses ``Maybe`` type for optional values.

**Security weaknesses:**

- **Unsafe operations available:** ``unsafePerformIO``, ``unsafeCoerce`` can
  bypass all guarantees.
- **Space leaks:** Lazy evaluation can cause unbounded memory consumption.
- **No aliasing control:** Shared references can lead to unexpected mutations
  when using ``IORef`` or ``STRef``.

**Lessons for Topineur:**

✓ Adopt effect system (``IO`` monad)
✓ Make purity the default
✓ Use strict evaluation by default (avoid space leaks)
✓ Do not provide ``unsafe*`` operations (at least not without explicit opt-in)

Rust
~~~~

**Security strengths:**

- **Ownership and borrowing:** Prevents use-after-free, double-free, and data
  races at compile time.
- **Lifetimes:** Track reference validity.
- **No garbage collection:** Deterministic memory management.
- **Strong type system:** Prevents type confusion.
- **Minimal runtime:** Small attack surface.

**Security weaknesses:**

- **Unsafe blocks:** Allow all guarantees to be bypassed. Required for low-level
  code but dangerous.
- **Complexity:** Ownership system has steep learning curve.
- **Integer overflow:** Silent wraparound in release mode (opt-in checking).

**Lessons for Topineur:**

✓ Adopt linear types for ownership
✓ Prevent aliasing of mutable data
✓ Do not allow unsafe escape hatches
✗ Use GC instead of manual memory (simpler, but less predictable)
✓ Check integer overflow (or use arbitrary precision)

Standard ML / OCaml
~~~~~~~~~~~~~~~~~~~

**Security strengths:**

- **Strong static typing:** Prevents type errors at compile time.
- **Immutability by default:** Reduces bugs.
- **Pattern matching exhaustiveness:** Prevents missing cases.
- **Module system:** Encapsulation and abstraction.
- **Memory safety:** Garbage collection.

**Security weaknesses:**

- **Mutable references:** ``ref`` cells allow mutation, breaking referential
  transparency.
- **Weak concurrency story:** Race conditions possible with mutable state.
- **No effect system:** Side effects are hidden.
- **Structural equality:** Can expose implementation details.

**Lessons for Topineur:**

✓ Use immutability by default
✓ Strong type system with inference
✓ Exhaustiveness checking for pattern matching
✓ Add effect system to make side effects explicit
✓ Control mutation through linear types

Scala
~~~~~

**Security strengths:**

- **Hybrid FP/OOP:** Flexible programming model.
- **Strong type system:** Generics, variance, path-dependent types.
- **Immutable collections:** Available in standard library.
- **Actor model:** Safe concurrency through message passing.

**Security weaknesses:**

- **Null references:** Inherited from JVM, major source of bugs.
- **Mutation allowed:** Mutable collections and ``var`` keywords.
- **Weak effect tracking:** Side effects are not tracked.
- **Complex type system:** Can be bypassed with ``asInstanceOf``.
- **JVM security issues:** Inherits vulnerabilities from platform.

**Lessons for Topineur:**

✓ Combine FP and OOP ergonomically
✓ Provide actor model for concurrency
✗ Do not allow null references
✓ Track effects explicitly
✗ Avoid overly complex type system features that can be abused

Comparative Security Table
---------------------------

+-------------------+----------+------+------------+-------+----------+
| Feature           | Haskell  | Rust | ML/OCaml   | Scala | Topineur |
+===================+==========+======+============+=======+==========+
| Memory safety     | Yes (GC) | Yes  | Yes (GC)   | Yes   | Yes (GC) |
+-------------------+----------+------+------------+-------+----------+
| Type safety       | Strong   | Strong| Strong    | Strong| Strong   |
+-------------------+----------+------+------------+-------+----------+
| No null refs      | Yes      | Yes  | N/A        | No    | Yes      |
+-------------------+----------+------+------------+-------+----------+
| Effect tracking   | Yes      | No   | No         | No    | Yes      |
+-------------------+----------+------+------------+-------+----------+
| Ownership/linear  | No       | Yes  | No         | No    | Yes      |
+-------------------+----------+------+------------+-------+----------+
| Immutable default | Yes      | No   | Yes        | No    | Yes      |
+-------------------+----------+------+------------+-------+----------+
| Safe concurrency  | Limited  | Yes  | Limited    | Actor | Actor    |
+-------------------+----------+------+------------+-------+----------+
| Unsafe escapes    | Yes      | Yes  | Limited    | Yes   | No       |
+-------------------+----------+------+------------+-------+----------+
| Pattern exhaust.  | Yes      | Yes  | Yes        | Yes   | Yes      |
+-------------------+----------+------+------------+-------+----------+

Topineur Security Features
---------------------------

1. Type Safety
~~~~~~~~~~~~~~

**Threat addressed:** Type confusion, invalid operations

**Implementation:**

- Strong static type system with Hindley-Milner inference
- No implicit conversions (explicit only)
- No downcasting or type casts
- Algebraic data types with exhaustive pattern matching

**Example:**

.. code-block:: topineur

   // This is a compile error:
   let x: Int = 42
   let s: String = x  // ERROR: type mismatch

   // Explicit conversion required:
   let s: String = show(x)  // OK

**Guarantees:**

- Programs that type-check cannot have type errors at runtime
- No segmentation faults due to type confusion
- Operations are only performed on valid types

2. Effect System
~~~~~~~~~~~~~~~~

**Threat addressed:** Hidden side effects, information leakage, unauthorized
operations

**Implementation:**

- Functions declare their effects using effect rows: ``!{IO, Network}``
- Pure functions have no effect annotation (default)
- Effect checking prevents calling effectful code from pure contexts
- Effects are tracked through the entire call chain

**Example:**

.. code-block:: topineur

   // Pure function — can be called from anywhere
   def factorial(n: Int): Int =
     if n <= 1 then 1 else n * factorial(n - 1)

   // Effectful function — must declare effects
   def readConfig(): !{IO} Config =
     let contents = readFile("config.json")
     parseConfig(contents)

   // Compile error if effects not declared:
   def broken(): Int =
     let cfg = readConfig()  // ERROR: IO effect not declared
     cfg.timeout

   // Correct version:
   def correct(): !{IO} Int =
     let cfg = readConfig()  // OK
     cfg.timeout

**Guarantees:**

- Side effects are visible in function signatures
- Impossible to perform IO from pure functions
- Security-sensitive operations (file access, network) are auditable
- Effect polymorphism allows abstracting over effects

**Security benefit:** An attacker cannot hide malicious side effects (network
exfiltration, file writes) inside seemingly pure functions.

3. Linear Types and Ownership
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Threat addressed:** Use-after-free, double-free, data races, aliasing bugs

**Implementation:**

- Linear types ``!lin T`` must be consumed exactly once
- Borrowing and ownership transfer tracked at compile time
- Mutable data requires linear ownership
- No aliasing of mutable data

**Example:**

.. code-block:: topineur

   object type File {
     handle: Int

     def close(): !{IO} Unit =
       closeFileHandle(handle)
   }

   def example(): !{IO} Unit =
     let file: !lin File = openFile("data.txt")  // file is linear
     file.close()  // file is consumed
     file.close()  // ERROR: file already used

**Example: Safe mutation**

.. code-block:: topineur

   object type MutableCounter {
     value: Int

     def inc(): !{State} MutableCounter =
       MutableCounter { value = value + 1 }
   }

   def example(): !{State} Unit =
     let counter: !lin MutableCounter = MutableCounter { value = 0 }
     let counter = counter.inc()  // old counter consumed, new one returned
     let counter = counter.inc()  // OK, no aliasing

**Guarantees:**

- No use-after-free: consumed values cannot be used again
- No double-free: resources cleaned up exactly once
- No data races: mutable data cannot be aliased
- Explicit ownership transfer

**Security benefit:** Prevents entire classes of memory corruption bugs that
are the root cause of many security vulnerabilities.

4. Immutability by Default
~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Threat addressed:** State-related bugs, race conditions, unexpected mutations

**Implementation:**

- All values are immutable by default
- Mutation only through explicit linear types or actors
- Shared data is always immutable

**Example:**

.. code-block:: topineur

   object type Point {
     x: Float
     y: Float

     def translate(dx: Float, dy: Float): Point =
       Point { x = x + dx, y = y + dy }  // Returns new point
   }

   let p1 = Point { x = 0.0, y = 0.0 }
   let p2 = p1.translate(1.0, 2.0)
   // p1 is unchanged, p2 is a new value

**Guarantees:**

- Shared data cannot be mutated
- No action-at-a-distance bugs
- Safe to share data across threads

**Security benefit:** Reduces attack surface by preventing unexpected state
changes. Immutable data structures are inherently thread-safe.

5. No Null References
~~~~~~~~~~~~~~~~~~~~~~

**Threat addressed:** Null pointer exceptions, segmentation faults

**Implementation:**

- No ``null`` value exists in Topineur
- Optional values use ``Option[T]`` type:

  - ``Some(value)``
  - ``None``

- Pattern matching forces handling both cases

**Example:**

.. code-block:: topineur

   def divide(a: Int, b: Int): Option[Int] =
     if b == 0 then None
     else Some(a / b)

   def example(): !{IO} Unit =
     match divide(10, 2) {
       case Some(result) => println("Result: " ++ show(result))
       case None => println("Division by zero")
     }

**Guarantees:**

- No null pointer exceptions
- All cases must be handled explicitly
- Type system ensures safety

**Security benefit:** Null pointer errors are a major source of crashes and
security vulnerabilities (CVE databases show thousands of null-related bugs).
Eliminating null eliminates this entire class of vulnerabilities.

6. Safe Concurrency with Actors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Threat addressed:** Data races, deadlocks, shared mutable state bugs

**Implementation:**

- Actor model for concurrent programming
- Actors communicate via immutable messages
- Each actor has private mutable state (no sharing)
- Message passing is asynchronous

**Example:**

.. code-block:: topineur

   actor Counter {
     count: Int  // private mutable state

     receive {
       case Increment => count = count + 1
       case GetCount(replyTo) => replyTo.send(count)
     }
   }

**Guarantees:**

- No data races (state is isolated)
- No shared mutable state
- Messages are immutable
- Type-safe message passing

**Security benefit:** Prevents concurrency bugs that can lead to security
vulnerabilities (TOCTOU, race conditions, corrupted state).

7. Exhaustive Pattern Matching
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Threat addressed:** Missing error cases, incomplete handling

**Implementation:**

- Pattern matching must cover all cases
- Compiler warns about non-exhaustive matches
- Forces explicit error handling

**Example:**

.. code-block:: topineur

   data Result[T, E] = Ok(T) | Err(E)

   def handle(result: Result[Int, String]): !{IO} Unit =
     match result {
       case Ok(value) => println("Success: " ++ show(value))
       case Err(msg) => println("Error: " ++ msg)
     }
     // If we forget a case, compiler error

**Guarantees:**

- All cases are handled
- No forgotten error paths
- Explicit error handling

**Security benefit:** Security bugs often arise from unhandled error cases.
Forcing exhaustive matching prevents these bugs.

Security Best Practices for Topineur
-------------------------------------

When writing Topineur code, follow these security guidelines:

1. **Minimize effectful code**

   - Keep effects at the boundaries (IO, network)
   - Core logic should be pure

2. **Use linear types for resources**

   - Files, sockets, database connections should use ``!lin`` types
   - Ensures proper cleanup

3. **Validate inputs at boundaries**

   - Use effect system to mark validation points
   - Parse untrusted data into safe types

4. **Prefer immutable data structures**

   - Use mutation only when necessary
   - Use actors for concurrent mutable state

5. **Use Option instead of error values**

   - Explicitly represent absence of value
   - Use Result[T, E] for errors

6. **Avoid partial functions**

   - Functions should handle all inputs or use Option/Result
   - Example: ``head`` on empty list should return ``Option[T]``

7. **Use traits for capability-based security**

   - Pass capabilities as trait objects
   - Limit what code can do through trait bounds

Known Limitations and Future Work
----------------------------------

**Current limitations:**

1. **No formal verification**

   - Type system provides strong guarantees but not formal proofs
   - Future: integrate with theorem provers

2. **No information flow control**

   - Effect system tracks side effects but not information flow
   - Future: add security levels (high/low) to types

3. **Limited effect granularity**

   - Effects are coarse-grained (IO, Network)
   - Future: finer-grained effects (ReadFile, WriteFile)

4. **No constant-time guarantees**

   - No protection against timing side channels
   - Future: add constant-time annotations

5. **GC pause times**

   - Garbage collection introduces non-determinism
   - Not suitable for hard real-time systems

6. **No hardware memory protection**

   - Relies on language semantics, not hardware MMU
   - Future: integrate with hardware capabilities

Security Audit Recommendations
-------------------------------

For production use, we recommend:

1. **Static analysis**

   - Run effect analysis to find hidden side effects
   - Use linearity checker to verify ownership

2. **Fuzzing**

   - Fuzz parsers and external interfaces
   - Test effect boundary violations

3. **Code review**

   - Review all effectful code paths
   - Ensure proper error handling

4. **Dependency audit**

   - Vet all external libraries
   - Prefer libraries with minimal effects

5. **Sandboxing**

   - Run untrusted code with restricted effect capabilities
   - Use effect system to limit what code can do

Conclusion
----------

Topineur provides strong language-based security through:

- **Memory safety** (GC)
- **Type safety** (strong static types)
- **Effect safety** (explicit side effects)
- **Ownership safety** (linear types)
- **Concurrency safety** (actors + immutability)

These features combine to create a language where large classes of security
vulnerabilities are prevented by construction.

**Compared to inspiration languages:**

- Stronger than Scala (no null, effects tracked)
- Comparable to Rust (ownership) but with GC
- Comparable to Haskell (effects) but with strict evaluation
- Stronger than ML (linear types, effects)

**Trade-offs:**

- More complex than simple languages (learning curve)
- GC overhead (vs. Rust manual memory)
- Less flexible than languages with escape hatches (vs. unsafe)

**Recommendation:** Topineur is suitable for security-critical applications
where correctness and safety are paramount, such as:

- Financial systems
- Medical devices
- Cryptographic protocols
- Access control systems
- Distributed consensus

References
----------

- [Rust] The Rust Programming Language — https://doc.rust-lang.org/book/
- [Haskell] Real World Haskell — http://book.realworldhaskell.org/
- [Linear Types] Linear Types Can Change the World (Wadler 1990)
- [Effect Systems] Algebraic Effects and Handlers (Plotkin & Pretnar 2009)
- [Capabilities] Capability Myths Demolished (Mark S. Miller et al. 2003)
- [CVE] Common Vulnerabilities and Exposures — https://cve.mitre.org/
