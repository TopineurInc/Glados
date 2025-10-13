Use Cases
=========

This section collects practical scenarios that exercise the paradigm.  Each
excerpt mirrors the structure of Python's tutorial-style documentation: brief
problem statements, followed by annotated code, and concluding remarks on
effects and ownership.  Companion source files live under
``examples/topineur`` with the ``.top`` extension.

Warm-up Examples
----------------

If you just want to poke the syntax, start with the tiny programs in
``hello_world.top``, ``fibonacci_recursive.top``, and ``factorial_recursive.top``.
They purposely keep the language on training wheels.

.. code-block:: topineur

   // hello_world.top
   def main(): !{IO} Unit =
     println("Hello, Topineur!")

   // fibonacci_recursive.top
   def fib(n: Int): Int =
     if n <= 1 then n
     else fib(n - 1) + fib(n - 2)

Discussion: these files are intentionally naive; they provide copy-paste fodder
for workshops or quick regression tests without touching the richer features.

Immutable Domain Modelling
--------------------------

Problem: represent a geometric model that must be shared freely across threads
without accidental mutation.

.. code-block:: topineur

   object type Point {
     x: Float
     y: Float

     def translate(dx: Float, dy: Float): Point =
       Point { x = x + dx, y = y + dy }
   }

   object type Polygon {
     vertices: List[Point]

     def centroid(): Point =
       let (sumX, sumY) = fold(vertices, (0.0, 0.0),
         fun ((accX, accY), point) ->
           (accX + point.x, accY + point.y))

       let n = float(length(vertices))
       Point { x = sumX / n, y = sumY / n }
   }

Discussion: all methods are pure.  Consumers can memoise results or reuse values
without worrying about concurrent modification.

Composable Services with Effects
--------------------------------

Problem: implement a user service that combines logging and persistence while
keeping side-effects explicit.

.. code-block:: topineur

   trait Logger {
     def log(level: LogLevel, message: String): !{IO} Unit
   }

   trait Repository[T] {
     def save(entity: T): !{IO, Database} Unit
   }

   object type UserService {
     logger: Logger
     repo: Repository[User]

     def createUser(name: String): !{IO, Database} User =
       logger.log(Info, "creating " ++ name)
       let user = User { name = name }
       repo.save(user)
       user
   }

Discussion: the method signature exposes the effects needed (`IO` and
`Database`).  Clients that only accept `!{IO}` must wrap the repository call in a
capability that downgrades or mocks the `Database` effect.

Safe Mutation with Linear Capabilities
--------------------------------------

Problem: manage a mutable counter that feeds a metrics subsystem while avoiding
aliasing bugs.

.. code-block:: topineur

   object type Counter {
     value: Int

     def increment(): !{State} Counter =
       Counter { value = value + 1 }
   }

   def bump(counter: !lin Counter, times: Int): !{State} Counter =
     if times == 0 then counter
     else
       let counter = counter.increment()
       bump(counter, times - 1)

Discussion: the linear annotation forces the caller to thread ownership through
the control flow.  The runtime translates consecutive increments into an
in-place mutation when possible.

UI Components as Pure Values
----------------------------

Problem: build a reusable button component that reacts to events by returning a
new component value.

.. code-block:: topineur

   object type Button {
     label: String
     onClick: Event -> !{IO} Button

     def render(ctx: Canvas): !{IO} Unit =
       ctx.drawLabel(label)
   }

   let button =
     Button {
       label = "OK",
       onClick = fun event ->
         Button {
           label = "Clicked",
           onClick = self.onClick
         }
     }

Discussion: `render` declares `IO` so that a UI toolkit can schedule drawing.
Event handling returns a new button, making state transitions explicit and
testable.

Actor-Based Concurrency
-----------------------

Problem: coordinate financial transactions across accounts without shared
locks.

.. code-block:: topineur

   actor Ledger {
     balances: Map[AccountId, Int]

     receive {
       case Transfer(from, to, amount, replyTo) ->
         if balances[from] >= amount then
           balances = balances
             .update(from, balances[from] - amount)
             .update(to, balances[to] + amount)
           replyTo.send(Success)
         else
           replyTo.send(InsufficientFunds)
     }
   }

Discussion: the actor owns its mutable state.  The effect row is
``!{IO, Actor[Ledger]}``, making it clear that message sending is the only
allowed side-effect.

Testing Strategies
------------------

* Provide pure reference implementations for traits and swap them in via effect
  narrowing.
* Replace linear capabilities with deterministic stubs during tests by using
  capability combinators that expose a proof of exclusive ownership.
* Trace effect rows during integration tests to ensure no unexpected operations
  were introduced.

Effect Adaptation
-----------------

Problem: expose a pure component through an interface that expects IO so it can
be reused without widening the caller's effect budget.

.. code-block:: topineur

   trait TimeSource {
     def now(): !{IO} Instant
   }

   object type FixedClock {
     instant: Instant

     def now(): Instant = instant
   }

   object type TimeSourceAdapter {
     clock: FixedClock

     def now(): !{IO} Instant =
       effect mask {IO} ->
         clock.now()
   }

Discussion: the adapter uses an effect mask to prove to the compiler that the
pure `FixedClock` satisfies `TimeSource` without introducing extra side-effects.

Atomic Transfers with Linear Capabilities
-----------------------------------------

Problem: move value between two accounts in a single transaction, ensuring that
both updates succeed or fail together.

.. code-block:: topineur

   def transfer(source: !lin Account,
                target: !lin Account,
                amount: Int): !{State} (!lin Account, !lin Account) =
     atomic (source, target) as (src, dst) ->
       let src = src.debit(amount)
       let dst = dst.credit(amount)
       (src, dst)

Discussion: the `atomic` block borrows both capabilities, preventing aliases
while guaranteeing rollback if an exception is raised.

Selective Laziness
------------------

Problem: stage a data-processing pipeline so that expensive computation is
deferred until consumers actually need the values.

.. code-block:: topineur

   object type Pipeline {
     source: DataSource

     def run(): !{IO} Lazy[List[Int]] =
       let raw = source.fetch()
       lazy map(raw, fun x -> x * x)
   }

Discussion: returning a `Lazy` list allows callers to partially evaluate results
(`take`, `drop`) while keeping source effects explicit.

Interop with Foreign Runtimes
-----------------------------

Problem: wrap a JavaScript API while preserving effect discipline.

.. code-block:: topineur

   foreign import js "Date.now" DateNow: !{IO, JS} Int

   object type NativeJsClock {
     def now(): !{IO, JS} Int =
       DateNow()
   }

Discussion: the foreign import carries both `IO` and `JS` effects, making it
obvious to reviewers and tooling that the call crosses the runtime boundary.

Conclusion
----------

Topineur's blend of immutable objects, explicit effects, and linear capabilities
covers a broad spectrum of architectural needs—from deterministic domain
modelling to actor-based concurrency, transactional updates, and foreign
interop.  The additional examples illustrate how teams can compose these
features pragmatically while keeping reasoning transparent.  Explore the
``examples/topineur`` directory to experiment further or adapt the patterns to
your own projects.
