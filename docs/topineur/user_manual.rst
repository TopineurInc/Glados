Topineur User Manual
====================

Welcome to Topineur, a language that harmonizes functional purity with
object-oriented modeling through immutable objects, explicit effects, and safe
ownership.

Table of Contents
-----------------

1. Getting Started
2. Basic Syntax
3. Types and Values
4. Functions and Methods
5. Object Types
6. Traits and Implementations
7. Effect System
8. Linear Types and Ownership
9. Pattern Matching
10. Concurrency with Actors
11. Standard Library
12. Error Handling
13. Advanced Topics
14. Common Patterns
15. Performance Tips

1. Getting Started
------------------

Installation
~~~~~~~~~~~~

Build from source:

.. code-block:: bash

   git clone https://github.com/your-org/glados.git
   cd glados
   make
   # The glados executable supports .top files

Running Your First Program
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Create ``hello.top``:

.. code-block:: topineur

   def main(): !{IO} Unit =
     println("Hello, Topineur!")

Run it:

.. code-block:: bash

   ./glados hello.top
   # Output: Hello, Topineur!

Compilation Modes
~~~~~~~~~~~~~~~~~

.. code-block:: bash

   # Run directly
   ./glados program.top

   # Compile to bytecode
   ./glados --compile program.top -o program.tbc

   # Disassemble bytecode
   ./glados --disasm program.tbc

   # Show type annotations
   ./glados --types program.top

2. Basic Syntax
---------------

Comments
~~~~~~~~

.. code-block:: topineur

   // Single-line comment

   /*
     Multi-line comment
     Can be nested /* like this */
   */

Variables
~~~~~~~~~

.. code-block:: topineur

   // Immutable binding
   let x = 42
   let y: Int = 100  // With type annotation

   // Type is inferred from expression
   let name = "Alice"

   // Multiple bindings
   let a = 1
   let b = 2
   let c = a + b

Blocks and Semicolons
~~~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   // Block expression — last expression is the value
   let result = {
     let x = 10
     let y = 20
     x + y  // No semicolon — this is the block's value
   }
   // result = 30

   // Statements need semicolons
   let result = {
     println("Computing...")
     42  // Return value
   }

3. Types and Values
-------------------

Primitive Types
~~~~~~~~~~~~~~~

.. code-block:: topineur

   // Integers (arbitrary precision)
   let i: Int = 42
   let negative: Int = -100

   // Floating point (64-bit)
   let f: Float = 3.14
   let scientific: Float = 1.5e-10

   // Booleans
   let t: Bool = #t
   let f: Bool = #f

   // Strings
   let s: String = "Hello, world!"
   let escaped: String = "Line 1\nLine 2\tTabbed"

   // Unit (void)
   let u: Unit = ()

Compound Types
~~~~~~~~~~~~~~

.. code-block:: topineur

   // Lists (homogeneous)
   let numbers: [Int] = [1, 2, 3, 4, 5]
   let empty: [String] = []

   // Tuples (heterogeneous)
   let pair: (Int, String) = (42, "answer")
   let triple: (Int, Float, Bool) = (1, 2.5, #t)

   // Accessing tuple elements
   let (x, y) = pair  // Pattern matching
   let first = pair.0  // Direct access

Option Type
~~~~~~~~~~~

.. code-block:: topineur

   // Represents optional values
   data Option[T] = Some(T) | None

   def find(list: [Int], target: Int): Option[Int] =
     // ... returns Some(index) or None

   // Usage
   match find([1, 2, 3], 2) {
     case Some(idx) => println("Found at index " ++ show(idx))
     case None => println("Not found")
   }

Result Type
~~~~~~~~~~~

.. code-block:: topineur

   // Represents success or failure
   data Result[T, E] = Ok(T) | Err(E)

   def parseNumber(s: String): Result[Int, String] =
     // ... returns Ok(number) or Err(error_message)

   // Usage
   match parseNumber("42") {
     case Ok(n) => println("Parsed: " ++ show(n))
     case Err(msg) => println("Error: " ++ msg)
   }

4. Functions and Methods
------------------------

Function Definitions
~~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   // Simple function
   def add(a: Int, b: Int): Int =
     a + b

   // Multi-line body
   def factorial(n: Int): Int =
     if n <= 1 then 1
     else n * factorial(n - 1)

   // With block
   def complex(x: Int): Int = {
     let temp = x * 2
     let result = temp + 10
     result
   }

Anonymous Functions (Lambdas)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   // Long form
   let double = fn(x: Int) => x * 2

   // Short form
   let triple = |x: Int| => x * 3

   // Higher-order functions
   def applyTwice(f: (Int) -> Int, x: Int): Int =
     f(f(x))

   let result = applyTwice(double, 5)  // result = 20

Generic Functions
~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   // Polymorphic function
   def identity[T](x: T): T = x

   // Multiple type parameters
   def pair[A, B](a: A, b: B): (A, B) = (a, b)

   // With trait bounds
   def maximum[T](a: T, b: T): T where T: Ord =
     if a > b then a else b

5. Object Types
---------------

Defining Objects
~~~~~~~~~~~~~~~~

.. code-block:: topineur

   object type Point {
     x: Float
     y: Float

     def translate(dx: Float, dy: Float): Point =
       Point { x = x + dx, y = y + dy }

     def distance(): Float =
       sqrt(x * x + y * y)

     def repr(): String =
       "Point(" ++ show(x) ++ ", " ++ show(y) ++ ")"
   }

Creating Objects
~~~~~~~~~~~~~~~~

.. code-block:: topineur

   // Object literal
   let origin = Point { x = 0.0, y = 0.0 }

   // Calling methods
   let p = origin.translate(3.0, 4.0)
   let d = p.distance()  // d = 5.0

   // Method chaining
   let final = origin
     .translate(1.0, 0.0)
     .translate(0.0, 1.0)

Accessing Fields
~~~~~~~~~~~~~~~~

.. code-block:: topineur

   let p = Point { x = 10.0, y = 20.0 }
   let x_coord = p.x  // Direct field access

   // Update via reconstruction
   let p2 = Point { x = 15.0, y = p.y }  // Update x, keep y

Objects with Generics
~~~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   object type Box[T] {
     value: T

     def get(): T = value

     def map[U](f: (T) -> U): Box[U] =
       Box { value = f(value) }
   }

   let intBox = Box { value = 42 }
   let strBox = intBox.map(show)  // Box { value = "42" }

6. Traits and Implementations
------------------------------

Defining Traits
~~~~~~~~~~~~~~~

.. code-block:: topineur

   trait Show {
     def show(): String
   }

   trait Eq {
     def equals(other: Self): Bool
   }

   trait Ord extends Eq {
     def compare(other: Self): Ordering
   }

Implementing Traits
~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   impl Show for Point {
     def show(): String =
       "(" ++ show(x) ++ ", " ++ show(y) ++ ")"
   }

   impl Eq for Point {
     def equals(other: Point): Bool =
       x == other.x && y == other.y
   }

Using Traits in Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   // Trait bound
   def printValue[T](value: T): !{IO} Unit where T: Show =
     println(value.show())

   // Multiple bounds
   def sortAndPrint[T](list: [T]): !{IO} Unit where T: Ord, T: Show =
     let sorted = sort(list)
     forEach(sorted, printValue)

Default Trait Implementations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   trait Drawable {
     def draw(ctx: Canvas): !{IO} Unit

     // Default implementation
     def drawTwice(ctx: Canvas): !{IO} Unit = {
       draw(ctx)
       draw(ctx)
     }
   }

7. Effect System
----------------

Pure Functions
~~~~~~~~~~~~~~

.. code-block:: topineur

   // No effect annotation — pure function
   def add(a: Int, b: Int): Int = a + b

   // Can be called from anywhere
   let result = add(2, 3)

Effectful Functions
~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   // IO effect
   def readFile(path: String): !{IO} String =
     // ... implementation

   // Multiple effects
   def fetchData(url: String): !{IO, Network} String =
     // ... implementation

   // State effect
   def increment(counter: MutableCounter): !{State} Unit =
     // ... implementation

Effect Propagation
~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   // Effects propagate through call chain
   def helper(): !{IO} String =
     readFile("config.txt")  // OK, we declared IO

   def caller(): !{IO} Unit =
     let content = helper()  // OK, we have IO
     println(content)

   // Compile error:
   def broken(): String =
     helper()  // ERROR: IO effect not in signature

Effect Polymorphism
~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   // Generic over effects
   def withLogging[E](action: () !{E} -> Int): !{E, IO} Int =
     println("Starting action")
     let result = action()
     println("Finished action")
     result

Common Effects
~~~~~~~~~~~~~~

- ``!{IO}`` — Input/output operations
- ``!{State}`` — Mutable state operations
- ``!{Network}`` — Network operations
- ``!{Exception}`` — Can throw exceptions
- ``!{Async}`` — Asynchronous operations

8. Linear Types and Ownership
------------------------------

Linear Values
~~~~~~~~~~~~~

.. code-block:: topineur

   // Linear type annotation
   let file: !lin File = openFile("data.txt")

   // Use exactly once
   file.close()  // OK, file consumed

   // ERROR: file already used
   // file.close()

Ownership Transfer
~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   def processFile(file: !lin File): !{IO} String =
     let contents = file.read()
     file.close()  // file consumed here
     contents

   let f = openFile("data.txt")
   let data = processFile(f)  // f moved into function
   // f cannot be used here anymore

Safe Mutation
~~~~~~~~~~~~~

.. code-block:: topineur

   object type MutableCounter {
     value: Int

     def inc(): !{State} MutableCounter =
       MutableCounter { value = value + 1 }
   }

   def example(): !{State} Unit =
     let counter: !lin MutableCounter = MutableCounter { value = 0 }
     let counter = counter.inc()  // Re-bind to new value
     let counter = counter.inc()  // OK, no aliasing
     println("Final count: " ++ show(counter.value))

Linear Resources
~~~~~~~~~~~~~~~~

Common use cases for linear types:

- File handles
- Socket connections
- Database connections
- Lock guards
- Memory regions

9. Pattern Matching
-------------------

Basic Matching
~~~~~~~~~~~~~~

.. code-block:: topineur

   match value {
     case 0 => "zero"
     case 1 => "one"
     case n => "other: " ++ show(n)  // Bind to variable
   }

Matching ADTs
~~~~~~~~~~~~~

.. code-block:: topineur

   data Shape
     = Circle(Float)
     | Rectangle(Float, Float)
     | Triangle(Float, Float, Float)

   def area(shape: Shape): Float =
     match shape {
       case Circle(r) => 3.14159 * r * r
       case Rectangle(w, h) => w * h
       case Triangle(a, b, c) =>
         let s = (a + b + c) / 2.0
         sqrt(s * (s - a) * (s - b) * (s - c))
     }

Nested Patterns
~~~~~~~~~~~~~~~

.. code-block:: topineur

   match result {
     case Ok(Some(value)) => println("Found: " ++ show(value))
     case Ok(None) => println("Not found")
     case Err(msg) => println("Error: " ++ msg)
   }

Guards
~~~~~~

.. code-block:: topineur

   match age {
     case n if n < 0 => "Invalid age"
     case n if n < 13 => "Child"
     case n if n < 20 => "Teenager"
     case _ => "Adult"
   }

Wildcard Pattern
~~~~~~~~~~~~~~~~

.. code-block:: topineur

   match status {
     case Success => handleSuccess()
     case _ => handleOtherCases()  // Catch-all
   }

10. Concurrency with Actors
----------------------------

Defining Actors
~~~~~~~~~~~~~~~

.. code-block:: topineur

   actor Counter {
     count: Int  // Private mutable state

     receive {
       case Increment => count = count + 1
       case Decrement => count = count - 1
       case GetCount(replyTo) =>
         replyTo.send(count)
     }
   }

Creating and Using Actors
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   def main(): !{IO, Async} Unit =
     let counter = spawn(Counter { count = 0 })

     // Send messages
     counter.send(Increment)
     counter.send(Increment)

     // Request-reply pattern
     let currentCount = counter.ask(GetCount)
     println("Count: " ++ show(currentCount))

Actor Messages
~~~~~~~~~~~~~~

.. code-block:: topineur

   // Messages are immutable data
   data CounterMsg
     = Increment
     | Decrement
     | GetCount(ActorRef[Int])
     | Reset

   actor Counter {
     count: Int

     receive {
       case Increment => count = count + 1
       case Decrement => count = count - 1
       case GetCount(replyTo) => replyTo.send(count)
       case Reset => count = 0
     }
   }

Actor Communication Patterns
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   // Fire-and-forget
   actor.send(message)

   // Request-reply
   let result = actor.ask(RequestMessage)

   // Forward to another actor
   actor received {
     case ProcessData(data, replyTo) =>
       otherActor.send(Transform(data, replyTo))
   }

11. Standard Library
--------------------

Core Functions
~~~~~~~~~~~~~~

.. code-block:: topineur

   // Arithmetic
   add, sub, mul, div, mod: (Int, Int) -> Int
   sqrt, pow, sin, cos: (Float) -> Float

   // Comparison
   eq, lt, gt, lte, gte: [T](T, T) -> Bool where T: Ord

   // Logic
   and, or, not: (Bool, Bool) -> Bool

   // String operations
   concat: (String, String) -> String  // Also: ++
   length: (String) -> Int
   substring: (String, Int, Int) -> String
   split: (String, String) -> [String]

List Operations
~~~~~~~~~~~~~~~

.. code-block:: topineur

   // Construction
   let list = [1, 2, 3]
   let prepended = 0 :: list  // [0, 1, 2, 3]

   // Access
   head: [T] -> Option[T]
   tail: [T] -> Option[[T]]
   get: ([T], Int) -> Option[T]

   // Transformation
   map: ([T], (T) -> U) -> [U]
   filter: ([T], (T) -> Bool) -> [T]
   fold: ([T], U, (U, T) -> U) -> U
   reverse: [T] -> [T]
   sort: [T] -> [T] where T: Ord

   // Examples
   let doubled = map([1, 2, 3], |x| => x * 2)  // [2, 4, 6]
   let evens = filter([1, 2, 3, 4], |x| => x % 2 == 0)  // [2, 4]
   let sum = fold([1, 2, 3], 0, add)  // 6

IO Operations
~~~~~~~~~~~~~

.. code-block:: topineur

   // Console
   println: (String) -> !{IO} Unit
   print: (String) -> !{IO} Unit
   readLine: () -> !{IO} String

   // Files
   readFile: (String) -> !{IO} String
   writeFile: (String, String) -> !{IO} Unit
   appendFile: (String, String) -> !{IO} Unit
   fileExists: (String) -> !{IO} Bool

   // Example
   def example(): !{IO} Unit = {
     println("Enter your name:")
     let name = readLine()
     println("Hello, " ++ name ++ "!")
   }

12. Error Handling
------------------

Option Type
~~~~~~~~~~~

.. code-block:: topineur

   def safeDivide(a: Int, b: Int): Option[Int] =
     if b == 0 then None
     else Some(a / b)

   // Using with pattern matching
   match safeDivide(10, 2) {
     case Some(result) => println(show(result))
     case None => println("Cannot divide by zero")
   }

   // Using with combinators
   let result = safeDivide(10, 2)
     .map(|x| => x * 2)
     .getOrElse(0)

Result Type
~~~~~~~~~~~

.. code-block:: topineur

   def parseAge(input: String): Result[Int, String] =
     match parseInt(input) {
       case Some(age) if age >= 0 && age <= 150 =>
         Ok(age)
       case Some(_) =>
         Err("Age out of valid range")
       case None =>
         Err("Invalid number format")
     }

   // Using Result
   match parseAge("25") {
     case Ok(age) => println("Valid age: " ++ show(age))
     case Err(msg) => println("Error: " ++ msg)
   }

Early Returns with Result
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   def processUser(input: String): Result[User, String] =
     let age = parseAge(input)?  // Propagate error
     let name = parseName(input)?
     Ok(User { age = age, name = name })

   // The ? operator desugars to:
   // match expr {
   //   case Ok(value) => value
   //   case Err(e) => return Err(e)
   // }

13. Advanced Topics
-------------------

Type Inference
~~~~~~~~~~~~~~

.. code-block:: topineur

   // Type inferred from usage
   let double = |x| => x * 2  // Inferred: (Int) -> Int
   println(show(double(21)))

   // Explicit annotation when needed
   let identity = |x: T| => x  // Generic

Lazy Evaluation
~~~~~~~~~~~~~~~

.. code-block:: topineur

   // Lazy value
   let expensive = lazy {
     println("Computing...")
     factorial(1000)
   }

   // Computed on first access
   let result = force(expensive)  // Prints "Computing..."
   let cached = force(expensive)   // No print, cached

   // Lazy type
   let lazyList: Lazy[[Int]] = lazy [1, 2, 3]

Tail Call Optimization
~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   // Tail-recursive function (optimized)
   def sumTail(list: [Int], acc: Int): Int =
     match list {
       case [] => acc
       case x :: xs => sumTail(xs, acc + x)  // Tail call
     }

   // Non-tail-recursive (not optimized)
   def sumNonTail(list: [Int]): Int =
     match list {
       case [] => 0
       case x :: xs => x + sumNonTail(xs)  // Not tail position
     }

14. Common Patterns
-------------------

Builder Pattern
~~~~~~~~~~~~~~~

.. code-block:: topineur

   object type Request {
     url: String
     method: String
     headers: [(String, String)]
     body: Option[String]

     def withHeader(key: String, value: String): Request =
       Request {
         url = url,
         method = method,
         headers = (key, value) :: headers,
         body = body
       }

     def withBody(content: String): Request =
       Request {
         url = url,
         method = method,
         headers = headers,
         body = Some(content)
       }
   }

   // Usage
   let req = Request {
       url = "https://api.example.com",
       method = "POST",
       headers = [],
       body = None
     }
     .withHeader("Content-Type", "application/json")
     .withBody("{\"name\": \"Alice\"}")

Visitor Pattern
~~~~~~~~~~~~~~~

.. code-block:: topineur

   trait Visitor[R] {
     def visitCircle(c: Circle): R
     def visitRectangle(r: Rectangle): R
   }

   trait Shape {
     def accept[R](visitor: Visitor[R]): R
   }

   impl Shape for Circle {
     def accept[R](visitor: Visitor[R]): R =
       visitor.visitCircle(this)
   }

Strategy Pattern
~~~~~~~~~~~~~~~~

.. code-block:: topineur

   trait SortStrategy {
     def sort(list: [Int]): [Int]
   }

   object type Sorter {
     strategy: SortStrategy

     def sort(list: [Int]): [Int] =
       strategy.sort(list)
   }

   impl SortStrategy for QuickSort {
     def sort(list: [Int]): [Int] = // ...
   }

   let sorter = Sorter { strategy = QuickSort {} }

15. Performance Tips
--------------------

1. **Use tail recursion**

   .. code-block:: topineur

      // Good: tail-recursive
      def sumTail(list: [Int], acc: Int): Int =
        match list {
          case [] => acc
          case x :: xs => sumTail(xs, acc + x)
        }

2. **Avoid repeated list concatenation**

   .. code-block:: topineur

      // Bad: O(n²)
      let result = fold(list, [], |acc, x| => acc ++ [x])

      // Good: O(n)
      let result = reverse(fold(list, [], |acc, x| => x :: acc))

3. **Use lazy evaluation for infinite structures**

   .. code-block:: topineur

      let infiniteList = lazy generateInfinite(0)
      let first10 = take(10, force(infiniteList))

4. **Prefer pattern matching over repeated field access**

   .. code-block:: topineur

      // Less efficient
      if point.x > 0 && point.y > 0 then ...

      // More efficient
      match point {
        case Point { x, y } if x > 0 && y > 0 => ...
      }

5. **Use actors for concurrent mutable state**

   .. code-block:: topineur

      // Actors are efficient for message-passing concurrency
      let worker = spawn(Worker { state = initialState })

Conclusion
----------

This manual covers the core features of Topineur. For more details, see:

- ``grammar.rst`` — formal grammar
- ``language_reference.rst`` — detailed reference
- ``security_review.rst`` — security features
- ``examples/topineur/`` — example programs

Happy coding in Topineur!
