module Test.CompilerSpec (tests) where

import Test.HUnit

import AST
import Compiler
import qualified Data.Vector as Vector

tests :: Test
tests = TestList
  [ TestLabel "Compile Integer" testCompileInteger
  , TestLabel "Compile Boolean" testCompileBoolean
  , TestLabel "Compile String" testCompileString
  , TestLabel "Compile Addition" testCompileAddition
  , TestLabel "Compile If" testCompileIf
  , TestLabel "Compile Lambda" testCompileLambda
  , TestLabel "Compile Define" testCompileDefine
  , TestLabel "Compile Factorial" testCompileFactorial
  , TestLabel "Compile Fibonacci" testCompileFibonacci
  , TestLabel "Compile Let" testCompileLet
  , TestLabel "Compile Letrec" testCompileLetrec
  , TestLabel "Compile Begin" testCompileBegin
  , TestLabel "Compile Nested" testCompileNested
  , TestLabel "Compile Multiple Defines" testCompileMultipleDefines
  , TestLabel "Compile Closure" testCompileClosure
  , TestLabel "Compile Higher Order" testCompileHigherOrder
  , TestLabel "Compile Recursive Sum" testCompileRecursiveSum
  , TestLabel "Compile Complex Program" testCompileComplexProgram
  , TestLabel "Compile Parse Error" testCompileParseError
  , TestLabel "Compile Invalid Syntax" testCompileInvalidSyntax
  ]

testCompileInteger :: Test
testCompileInteger = "compile integer" ~: case compile defaultConfig "42" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile integer"

testCompileBoolean :: Test
testCompileBoolean = "compile boolean" ~: case compile defaultConfig "#t" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile boolean"

testCompileString :: Test
testCompileString = "compile string" ~: case compile defaultConfig "\"hello\"" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile string"

testCompileAddition :: Test
testCompileAddition = "compile addition" ~: case compile defaultConfig "(+ 1 2)" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile addition"

testCompileIf :: Test
testCompileIf = "compile if" ~: case compile defaultConfig "(if #t 1 2)" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile if"

testCompileLambda :: Test
testCompileLambda = "compile lambda" ~: case compile defaultConfig "(lambda (x) x)" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile lambda"

testCompileDefine :: Test
testCompileDefine = "compile define" ~: case compile defaultConfig "(define x 42)" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile define"

testCompileFactorial :: Test
testCompileFactorial = "compile factorial" ~: case compile defaultConfig "(define (fact n) (if (eq? n 0) 1 (* n (fact (- n 1)))))" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile factorial"

testCompileFibonacci :: Test
testCompileFibonacci = "compile fibonacci" ~: case compile defaultConfig "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile fibonacci"

testCompileLet :: Test
testCompileLet = "compile let" ~: case compile defaultConfig "(let ((x 1) (y 2)) (+ x y))" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile let"

testCompileLetrec :: Test
testCompileLetrec = "compile letrec" ~: case compile defaultConfig "(letrec ((f (lambda (x) x))) (f 42))" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile letrec"

testCompileBegin :: Test
testCompileBegin = "compile begin" ~: case compile defaultConfig "(begin 1 2 3)" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile begin"

testCompileNested :: Test
testCompileNested = "compile nested" ~: case compile defaultConfig "(+ (* 2 3) (- 10 4))" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile nested"

testCompileMultipleDefines :: Test
testCompileMultipleDefines = "compile multiple defines" ~: case compile defaultConfig "(define x 1) (define y 2) (+ x y)" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile multiple defines"

testCompileClosure :: Test
testCompileClosure = "compile closure" ~: case compile defaultConfig "(define (make-adder n) (lambda (x) (+ x n)))" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile closure"

testCompileHigherOrder :: Test
testCompileHigherOrder = "compile higher order" ~: case compile defaultConfig "(define (twice f x) (f (f x)))" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile higher order"

testCompileRecursiveSum :: Test
testCompileRecursiveSum = "compile recursive sum" ~: case compile defaultConfig "(define (sum n) (if (eq? n 0) 0 (+ n (sum (- n 1)))))" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile recursive sum"

testCompileComplexProgram :: Test
testCompileComplexProgram = "compile complex program" ~: case compile defaultConfig "(define (gcd a b) (if (eq? b 0) a (gcd b (mod a b)))) (gcd 48 18)" of
  Right code -> Vector.length (coInstrs code) > 0
  Left _ -> False
  ~? "Should compile complex program"

testCompileParseError :: Test
testCompileParseError = "compile parse error" ~: case compile defaultConfig "(+ 1 2" of
  Left _ -> True
  Right _ -> False
  ~? "Should fail on parse error"

testCompileInvalidSyntax :: Test
testCompileInvalidSyntax = "compile invalid syntax" ~: case compile defaultConfig "(let (x 1) x)" of
  Left _ -> True
  Right _ -> False
  ~? "Should fail on invalid syntax"
