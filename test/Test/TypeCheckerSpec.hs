{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- TypeCheckerSpec - Tests for type inference and checking
-}

module Test.TypeCheckerSpec (spec) where

import Test.Hspec
import AST
import TypeChecker
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Type Inference - Literals" $ do
    it "infers Int for integer literals" $
      inferType (EInt 42) `shouldBe` Right TInt
    
    it "infers Float for float literals" $
      inferType (EFloat 3.14) `shouldBe` Right TFloat
    
    it "infers Bool for boolean literals" $
      inferType (EBool True) `shouldBe` Right TBool
    
    it "infers String for string literals" $
      inferType (EString "hello") `shouldBe` Right TString
    
    it "infers Unit for unit value" $
      inferType EUnit `shouldBe` Right TUnit

  describe "Type Inference - Variables" $ do
    it "looks up variable types in environment" $
      typeCheck (Map.singleton "x" TInt) (EVar "x") `shouldBe` Right TInt
    
    it "fails for unbound variables" $
      inferType (EVar "unbound") `shouldSatisfy` isLeft

  describe "Type Inference - If Expressions" $ do
    it "infers type of if with matching branches" $
      inferType (EIf (EBool True) (EInt 1) (EInt 2)) `shouldBe` Right TInt
    
    it "fails if condition is not Bool" $
      inferType (EIf (EInt 42) (EInt 1) (EInt 2)) `shouldSatisfy` isLeft
    
    it "fails if branches have different types" $
      inferType (EIf (EBool True) (EInt 1) (EString "two")) `shouldSatisfy` isLeft

  describe "Type Inference - Lambda" $ do
    it "infers function type for lambda" $
      case inferType (ELambda [("x", Nothing)] Nothing (EVar "x") []) of
        Right (TFun [TVar _] (TVar _)) -> True
        _ -> False
      `shouldBe` True
    
    it "uses type annotations for parameters" $
      inferType (ELambda [("x", Just TInt)] Nothing (EVar "x") []) 
        `shouldBe` Right (TFun [TInt] TInt)
    
    it "checks return type annotation" $
      inferType (ELambda [("x", Just TInt)] (Just TInt) (EVar "x") [])
        `shouldBe` Right (TFun [TInt] TInt)
    
    it "fails if return type doesn't match annotation" $
      inferType (ELambda [("x", Just TInt)] (Just TBool) (EVar "x") [])
        `shouldSatisfy` isLeft

  describe "Type Inference - Application" $ do
    it "infers result type of function application" $
      let func = ELambda [("x", Just TInt)] (Just TInt) (EVar "x") []
          app = EApp func [EInt 42]
      in inferType app `shouldBe` Right TInt
    
    it "fails on arity mismatch" $
      let func = ELambda [("x", Just TInt), ("y", Just TInt)] Nothing 
                   (EBinOp Add (EVar "x") (EVar "y")) []
          app = EApp func [EInt 42]
      in inferType app `shouldSatisfy` isLeft
    
    it "fails on type mismatch in arguments" $
      let func = ELambda [("x", Just TInt)] Nothing (EVar "x") []
          app = EApp func [EString "wrong"]
      in inferType app `shouldSatisfy` isLeft

  describe "Type Inference - Binary Operators" $ do
    it "infers Int for arithmetic operations" $ do
      inferType (EBinOp Add (EInt 1) (EInt 2)) `shouldBe` Right TInt
      inferType (EBinOp Sub (EInt 5) (EInt 3)) `shouldBe` Right TInt
      inferType (EBinOp Mul (EInt 2) (EInt 3)) `shouldBe` Right TInt
      inferType (EBinOp Div (EInt 6) (EInt 2)) `shouldBe` Right TInt
    
    it "infers Bool for comparison operations" $ do
      inferType (EBinOp Lt (EInt 1) (EInt 2)) `shouldBe` Right TBool
      inferType (EBinOp Gt (EInt 2) (EInt 1)) `shouldBe` Right TBool
      inferType (EBinOp Lte (EInt 1) (EInt 2)) `shouldBe` Right TBool
      inferType (EBinOp Gte (EInt 2) (EInt 1)) `shouldBe` Right TBool
    
    it "infers Bool for equality operations" $ do
      inferType (EBinOp Eq (EInt 1) (EInt 1)) `shouldBe` Right TBool
      inferType (EBinOp Neq (EInt 1) (EInt 2)) `shouldBe` Right TBool
    
    it "fails for type mismatch in binary operations" $
      inferType (EBinOp Add (EInt 1) (EString "x")) `shouldSatisfy` isLeft

  describe "Type Inference - Unary Operators" $ do
    it "infers Bool for not operation" $
      inferType (EUnOp Not (EBool True)) `shouldBe` Right TBool
    
    it "infers Int for negation" $
      inferType (EUnOp Neg (EInt 42)) `shouldBe` Right TInt
    
    it "fails for type mismatch in unary operations" $
      inferType (EUnOp Not (EInt 42)) `shouldSatisfy` isLeft

  describe "Type Inference - Tuples" $ do
    it "infers tuple type" $
      inferType (ETuple [EInt 1, EString "hello", EBool True])
        `shouldBe` Right (TTuple [TInt, TString, TBool])
    
    it "handles empty tuples" $
      inferType (ETuple []) `shouldBe` Right (TTuple [])

  describe "Type Inference - Lists" $ do
    it "infers homogeneous list type" $
      inferType (EListLiteral [EInt 1, EInt 2, EInt 3] Nothing)
        `shouldBe` Right (TList TInt)
    
    it "uses type annotation for empty list" $
      inferType (EListLiteral [] (Just TString))
        `shouldBe` Right (TList TString)
    
    it "fails for heterogeneous lists" $
      inferType (EListLiteral [EInt 1, EString "x"] Nothing)
        `shouldSatisfy` isLeft

  describe "Type Inference - Loops" $ do
    it "infers Unit for while loops" $
      inferType (EWhile (EBool True) (EInt 42)) `shouldBe` Right TUnit
    
    it "fails if while condition is not Bool" $
      inferType (EWhile (EInt 1) (EInt 42)) `shouldSatisfy` isLeft
    
    it "infers Unit for for loops" $
      inferType (EFor "i" (EInt 0) (EInt 10) (EInt 42)) 
        `shouldBe` Right TUnit
    
    it "fails if for bounds are not Int" $
      inferType (EFor "i" (EString "0") (EInt 10) (EInt 42))
        `shouldSatisfy` isLeft

  describe "Type Inference - Range" $ do
    it "infers List Int for range" $
      inferType (ERange (EInt 0) (EInt 10)) `shouldBe` Right (TList TInt)
    
    it "fails if range bounds are not Int" $
      inferType (ERange (EString "0") (EInt 10)) `shouldSatisfy` isLeft

  describe "Type Inference - Complex Expressions" $ do
    it "handles nested lambdas" $
      let outer = ELambda [("x", Just TInt)] Nothing
                    (ELambda [("y", Just TInt)] Nothing 
                      (EBinOp Add (EVar "x") (EVar "y")) []) []
      in inferType outer `shouldBe` Right (TFun [TInt] (TFun [TInt] TInt))
    
    it "handles higher-order functions" $
      let apply = ELambda [("f", Just (TFun [TInt] TInt)), ("x", Just TInt)] Nothing
                    (EApp (EVar "f") [EVar "x"]) []
      in inferType apply `shouldBe` Right (TFun [TFun [TInt] TInt, TInt] TInt)
    
    it "handles recursive patterns with letrec" $
      let factorial = EDefine "fac" 
            (ELambda [("n", Just TInt)] (Just TInt)
              (EIf (EBinOp Lte (EVar "n") (EInt 1))
                (EInt 1)
                (EBinOp Mul (EVar "n") 
                  (EApp (EVar "fac") [EBinOp Sub (EVar "n") (EInt 1)]))) []) []
      in inferType factorial `shouldBe` Right TUnit

-- Helper function
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
