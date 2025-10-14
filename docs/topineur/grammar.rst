Topineur Formal Grammar
=======================

This document provides the complete formal grammar for the Topineur programming
language using Extended Backus-Naur Form (EBNF) notation.

Notation Conventions
--------------------

- ``|`` separates alternatives
- ``[ ]`` denotes optional elements (zero or one occurrence)
- ``{ }`` denotes repetition (zero or more occurrences)
- ``( )`` groups elements
- ``" "`` denotes terminal symbols (keywords and operators)
- ``(* *)`` denotes comments in the grammar
- Lowercase names are non-terminals
- UPPERCASE names are token classes

Lexical Structure
-----------------

Tokens
~~~~~~

.. code-block:: ebnf

   (* Comments *)
   COMMENT = "//" { any_char - newline } newline
           | "/*" { any_char - "*/" } "*/"

   (* Whitespace *)
   WHITESPACE = " " | "\t" | "\n" | "\r"

   (* Identifiers *)
   IDENT = letter { letter | digit | "_" }
   TYPE_IDENT = upper_letter { letter | digit | "_" }

   letter = "a".."z" | "A".."Z"
   upper_letter = "A".."Z"
   digit = "0".."9"

   (* Literals *)
   INT_LIT = [ "-" ] digit { digit }
   FLOAT_LIT = [ "-" ] digit { digit } "." digit { digit }
             [ ("e" | "E") [ "+" | "-" ] digit { digit } ]
   BOOL_LIT = "#t" | "#f"
   STRING_LIT = '"' { string_char } '"'
   string_char = any_char - '"' | "\\" escape_char
   escape_char = "n" | "t" | "r" | '"' | "\\"

   (* Keywords *)
   KEYWORD = "object" | "type" | "trait" | "impl" | "for"
           | "def" | "let" | "mut" | "if" | "then" | "else"
           | "match" | "case" | "lazy" | "actor" | "receive"

   (* Operators *)
   OPERATOR = "+" | "-" | "*" | "/" | "%" | "=="
            | "!=" | "<" | ">" | "<=" | ">="
            | "&&" | "||" | "!" | "++"
            | "=" | ":" | "->" | "=>"

   (* Delimiters *)
   DELIMITER = "(" | ")" | "{" | "}" | "[" | "]"
             | "," | ";" | "." | "|"

Module Structure
----------------

A Topineur program is a module containing zero or more top-level declarations.

.. code-block:: ebnf

   module = { declaration }

   declaration = object_def
               | trait_def
               | trait_impl
               | function_def
               | const_def

Type System
-----------

Type Expressions
~~~~~~~~~~~~~~~~

.. code-block:: ebnf

   type = simple_type
        | function_type
        | linear_type
        | lazy_type

   simple_type = "Int"
               | "Float"
               | "Bool"
               | "String"
               | "Unit"
               | TYPE_IDENT              (* Object type or type variable *)
               | "[" type "]"            (* List type *)
               | "(" type { "," type } ")"  (* Tuple type *)

   function_type = "(" [ type_list ] ")" [ effect_row ] "->" type

   type_list = type { "," type }

   linear_type = "!lin" type

   lazy_type = "Lazy" "[" type "]"

Effect Rows
~~~~~~~~~~~

.. code-block:: ebnf

   effect_row = "!{" [ effect_list ] "}"

   effect_list = effect { "," effect }

   effect = "IO"
          | "State"
          | "Network"
          | "Exception"
          | TYPE_IDENT           (* Custom effect *)

Object Definitions
------------------

.. code-block:: ebnf

   object_def = "object" "type" TYPE_IDENT [ type_params ] "{"
                  { field_def | method_def }
                "}"

   type_params = "[" TYPE_IDENT { "," TYPE_IDENT } "]"

   field_def = IDENT ":" type

   method_def = "def" IDENT "(" [ param_list ] ")" ":" [ effect_row ] type "="
                expr

   param_list = param { "," param }

   param = IDENT ":" type

Trait Definitions
-----------------

.. code-block:: ebnf

   trait_def = "trait" TYPE_IDENT [ type_params ] [ trait_bounds ] "{"
                 { method_sig }
               "}"

   trait_bounds = "where" trait_bound { "," trait_bound }

   trait_bound = TYPE_IDENT ":" TYPE_IDENT  (* T: Drawable *)

   method_sig = "def" IDENT "(" [ param_list ] ")" ":" [ effect_row ] type

Trait Implementations
---------------------

.. code-block:: ebnf

   trait_impl = "impl" TYPE_IDENT "for" type [ where_clause ] "{"
                  { method_def }
                "}"

   where_clause = "where" trait_bound { "," trait_bound }

Function Definitions
--------------------

.. code-block:: ebnf

   function_def = "def" IDENT [ type_params ] "(" [ param_list ] ")"
                  ":" [ effect_row ] type [ where_clause ] "="
                  expr

   const_def = "let" IDENT ":" type "=" expr

Expressions
-----------

Precedence Levels
~~~~~~~~~~~~~~~~~

From highest to lowest precedence:

1. Primary expressions (literals, identifiers, parentheses)
2. Postfix operators (method calls, field access, function calls)
3. Prefix operators (``!``, ``-``)
4. Multiplicative operators (``*``, ``/``, ``%``)
5. Additive operators (``+``, ``-``, ``++``)
6. Comparison operators (``<``, ``>``, ``<=``, ``>=``)
7. Equality operators (``==``, ``!=``)
8. Logical AND (``&&``)
9. Logical OR (``||``)
10. Type ascription (``::``)
11. Assignment / Let binding

Expression Grammar
~~~~~~~~~~~~~~~~~~

.. code-block:: ebnf

   expr = logical_or_expr

   logical_or_expr = logical_and_expr { "||" logical_and_expr }

   logical_and_expr = equality_expr { "&&" equality_expr }

   equality_expr = comparison_expr { equality_op comparison_expr }

   equality_op = "==" | "!="

   comparison_expr = additive_expr { comparison_op additive_expr }

   comparison_op = "<" | ">" | "<=" | ">="

   additive_expr = multiplicative_expr { additive_op multiplicative_expr }

   additive_op = "+" | "-" | "++"

   multiplicative_expr = prefix_expr { multiplicative_op prefix_expr }

   multiplicative_op = "*" | "/" | "%"

   prefix_expr = [ prefix_op ] postfix_expr

   prefix_op = "!" | "-"

   postfix_expr = primary_expr { postfix_suffix }

   postfix_suffix = "." IDENT                    (* Field access *)
                  | "." IDENT "(" [ expr_list ] ")"  (* Method call *)
                  | "(" [ expr_list ] ")"             (* Function call *)

   expr_list = expr { "," expr }

Primary Expressions
~~~~~~~~~~~~~~~~~~~

.. code-block:: ebnf

   primary_expr = literal
                | IDENT
                | lambda_expr
                | if_expr
                | match_expr
                | let_expr
                | block_expr
                | object_literal
                | list_literal
                | tuple_literal
                | "(" expr ")"

Literals
~~~~~~~~

.. code-block:: ebnf

   literal = INT_LIT
           | FLOAT_LIT
           | BOOL_LIT
           | STRING_LIT
           | unit_literal

   unit_literal = "()"

   object_literal = TYPE_IDENT "{" [ field_init_list ] "}"

   field_init_list = field_init { "," field_init }

   field_init = IDENT "=" expr

   list_literal = "[" [ expr_list ] "]"

   tuple_literal = "(" expr "," expr_list ")"

Lambda Expressions
~~~~~~~~~~~~~~~~~~

.. code-block:: ebnf

   lambda_expr = "fn" "(" [ param_list ] ")" [ ":" [ effect_row ] type ] "=>" expr
               | "|" [ param_list ] "|" "=>" expr    (* Shorthand *)

Control Flow
~~~~~~~~~~~~

.. code-block:: ebnf

   if_expr = "if" expr "then" expr "else" expr

   match_expr = "match" expr "{" { match_case } "}"

   match_case = "case" pattern "=>" expr

   pattern = literal_pattern
           | var_pattern
           | constructor_pattern
           | wildcard_pattern

   literal_pattern = literal

   var_pattern = IDENT

   constructor_pattern = TYPE_IDENT [ "(" [ pattern_list ] ")" ]

   pattern_list = pattern { "," pattern }

   wildcard_pattern = "_"

Let Bindings
~~~~~~~~~~~~

.. code-block:: ebnf

   let_expr = "let" [ "mut" ] [ "!lin" ] IDENT [ ":" type ] "=" expr "in" expr
            | "let" [ "mut" ] [ "!lin" ] IDENT [ ":" type ] "=" expr ";" expr

   (* Note: second form is syntactic sugar for the first *)

Block Expressions
~~~~~~~~~~~~~~~~~

.. code-block:: ebnf

   block_expr = "{" { statement ";" } [ expr ] "}"

   statement = let_expr
             | expr

Comments
--------

.. code-block:: ebnf

   line_comment = "//" { any_char - newline } newline

   block_comment = "/*" { any_char | block_comment } "*/"

   (* Block comments can be nested *)

Operator Precedence Table
--------------------------

+------------+------------------------+---------------+
| Precedence | Operators              | Associativity |
+============+========================+===============+
| 1 (lowest) | ``||``                 | Left          |
+------------+------------------------+---------------+
| 2          | ``&&``                 | Left          |
+------------+------------------------+---------------+
| 3          | ``==``, ``!=``         | Left          |
+------------+------------------------+---------------+
| 4          | ``<``, ``>``, ``<=``,  | Left          |
|            | ``>=``                 |               |
+------------+------------------------+---------------+
| 5          | ``+``, ``-``, ``++``   | Left          |
+------------+------------------------+---------------+
| 6          | ``*``, ``/``, ``%``    | Left          |
+------------+------------------------+---------------+
| 7          | ``!``, ``-`` (prefix)  | Right         |
+------------+------------------------+---------------+
| 8          | ``.`` (field/method),  | Left          |
|            | ``()`` (call)          |               |
+------------+------------------------+---------------+
| 9 (highest)| Primary expressions    | N/A           |
+------------+------------------------+---------------+

Syntactic Sugar
---------------

Topineur provides several syntactic sugar forms that desugar to simpler
constructs:

Named Functions
~~~~~~~~~~~~~~~

.. code-block:: topineur

   def add(a: Int, b: Int): Int = a + b

Desugars to:

.. code-block:: topineur

   let add: (Int, Int) -> Int = fn(a: Int, b: Int) => a + b

Method Definitions
~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   object type Point {
     x: Float
     y: Float

     def translate(dx: Float, dy: Float): Point =
       Point { x = x + dx, y = y + dy }
   }

Desugars to object with vtable containing closures over ``this``.

String Interpolation
~~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   "Hello, {name}!"

Desugars to:

.. code-block:: topineur

   "Hello, " ++ show(name) ++ "!"

Chained Method Calls
~~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   p.translate(1.0, 2.0).distance()

Desugars to:

.. code-block:: topineur

   let tmp = p.translate(1.0, 2.0) in tmp.distance()

List Comprehensions (Optional)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If implemented:

.. code-block:: topineur

   [x * 2 | x <- xs, x > 0]

Desugars to:

.. code-block:: topineur

   map(fn(x) => x * 2, filter(fn(x) => x > 0, xs))

Grammar Examples
----------------

These examples demonstrate the grammar in action:

Example 1: Simple Function
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   def factorial(n: Int): Int =
     if n <= 1 then 1
     else n * factorial(n - 1)

Parse tree:

.. code-block:: text

   function_def
   ├─ def
   ├─ IDENT: factorial
   ├─ ( param_list )
   │  └─ param: n : Int
   ├─ : type: Int
   ├─ =
   └─ expr: if_expr
      ├─ if
      ├─ expr: comparison_expr (n <= 1)
      ├─ then
      ├─ expr: literal (1)
      ├─ else
      └─ expr: multiplicative_expr (n * factorial(n - 1))

Example 2: Object Definition
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   object type Counter {
     value: Int

     def inc(): Counter =
       Counter { value = value + 1 }

     def get(): Int = value
   }

Parse tree:

.. code-block:: text

   object_def
   ├─ object type
   ├─ TYPE_IDENT: Counter
   ├─ {
   ├─ field_def: value : Int
   ├─ method_def: inc
   │  ├─ def inc
   │  ├─ ( )
   │  ├─ : type: Counter
   │  ├─ =
   │  └─ expr: object_literal (Counter { ... })
   ├─ method_def: get
   │  ├─ def get
   │  ├─ ( )
   │  ├─ : type: Int
   │  ├─ =
   │  └─ expr: IDENT (value)
   └─ }

Example 3: Trait and Implementation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: topineur

   trait Show {
     def show(): String
   }

   impl Show for Counter {
     def show(): String =
       "Counter(" ++ show(value) ++ ")"
   }

Parse tree:

.. code-block:: text

   trait_def
   ├─ trait
   ├─ TYPE_IDENT: Show
   ├─ {
   ├─ method_sig: show
   │  ├─ def show
   │  ├─ ( )
   │  └─ : type: String
   └─ }

   trait_impl
   ├─ impl
   ├─ TYPE_IDENT: Show
   ├─ for
   ├─ type: Counter
   ├─ {
   ├─ method_def: show
   │  ├─ def show
   │  ├─ ( )
   │  ├─ : type: String
   │  ├─ =
   │  └─ expr: additive_expr (string concat)
   └─ }

Ambiguities and Resolutions
----------------------------

1. **Function call vs. type annotation**

   Ambiguity: ``f(x)`` could be a call or a function type.

   Resolution: Context-dependent. In type position, it's a type; in expression
   position, it's a call.

2. **Tuple vs. parenthesized expression**

   Ambiguity: ``(x)`` could be a 1-tuple or grouped expression.

   Resolution: Require at least one comma for tuples: ``(x,)`` is a tuple,
   ``(x)`` is a grouped expression.

3. **Method call vs. field access**

   Ambiguity: ``obj.field`` vs. ``obj.method()``.

   Resolution: No ambiguity; field access has no parentheses, method calls do.

4. **Negative numbers vs. subtraction**

   Ambiguity: ``1-2`` could be parsed as ``1 - 2`` or ``1(-2)``.

   Resolution: Whitespace-insensitive: always treat as subtraction. Negative
   literals must have no preceding expression: ``let x = -2`` is a negative
   literal.

Future Extensions
-----------------

The grammar is designed to be extensible. Planned extensions include:

- User-defined infix operators with custom precedence
- Pattern guards in match expressions
- Type classes with associated types
- Module system with imports/exports
- Macros and metaprogramming constructs

Notes
-----

- This grammar is LL(k) parseable with appropriate lookahead
- Left recursion in operators is handled via precedence climbing
- The grammar prioritizes readability and familiar syntax
- Effect annotations are optional; omitting them implies a pure function
- Linear types are opt-in via explicit ``!lin`` annotation
