Runtime and Effects
===================

Execution Model
---------------

* Immutable objects are freely sharable between threads without synchronisation.
* Linear capabilities enable the runtime to mutate in place when safe.  When a
  capability is consumed, the old reference becomes invalid.
* Memory management combines a tracing garbage collector for regular values with
  reference-counted fast paths for linear resources.

Effect Tracking
---------------

The effect system is integral to tooling and safety:

* Every function carries an effect row.  The compiler rejects attempts to call a
  function whose effects are not tolerated by the caller.
* Effect analysis allows IDEs and static tools to produce call graphs annotated
  with side-effects.
* Testing harnesses can stub effectful methods by providing alternative
  implementations with narrower rows.

Concurrency Model
-----------------

Actors encapsulate mutable state and process messages sequentially.  Messages
must be immutable values so that the runtime can ship them safely across threads
or processes.

.. code-block:: topineur

   actor BankAccount {
     balance: Int

     receive {
       case Deposit(amount, replyTo) ->
         balance = balance + amount
         replyTo.send(OK)

       case Withdraw(amount, replyTo) ->
         if balance >= amount then
           balance = balance - amount
           replyTo.send(OK)
         else
           replyTo.send(InsufficientFunds)
     }
   }

For cross-actor coordination, Topineur favours message passing and effect-aware
transactions over shared locks.  The runtime can surface potential deadlocks by
tracking await graphs within actors.

Atomic Sections
---------------

When linear resources need transactional updates, use the ``atomic`` primitive.
It executes a block with exclusive access to the resource and rolls back if an
exceptional effect is raised.

.. code-block:: topineur

   atomic accountCapability as account ->
     account.transfer(to = target, amount = 50)

The typechecker enforces that no capability escapes the atomic scope without
being re-wrapped, preventing double-spend mistakes.
