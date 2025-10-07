module Test.AlphaRenameSpec (tests) where

import Test.HUnit

import AST
import AlphaRename

tests :: Test
tests = TestList
  [ TestLabel "Alpha Rename Basic" testAlphaRenameBasic
  , TestLabel "Alpha Rename Lambda" testAlphaRenameLambda
  , TestLabel "Alpha Rename Define" testAlphaRenameDefine
  , TestLabel "Alpha Rename Letrec" testAlphaRenameLetrec
  , TestLabel "Alpha Rename If" testAlphaRenameIf
  , TestLabel "Alpha Rename App" testAlphaRenameApp
  ]

testAlphaRenameBasic :: Test
testAlphaRenameBasic = TestList
  [ "rename integer unchanged" ~: alphaRename (EInt 42) ~?= Right (EInt 42)
  , "rename boolean unchanged" ~: alphaRename (EBool True) ~?= Right (EBool True)
  , "rename string unchanged" ~: alphaRename (EString "hello") ~?= Right (EString "hello")
  , "rename free var unchanged" ~: alphaRename (EVar "x") ~?= Right (EVar "x")
  , "rename quote unchanged" ~: alphaRename (EQuote (SAtom (AInteger 42) Nothing)) ~?= Right (EQuote (SAtom (AInteger 42) Nothing))
  ]

testAlphaRenameLambda :: Test
testAlphaRenameLambda = TestList
  [ "rename lambda param" ~: case alphaRename (ELambda ["x"] (EVar "x")) of
      Right (ELambda [newName] (EVar varName)) | newName == varName && newName /= "x" -> True
      _ -> False
      ~? "Should rename lambda param"
  , "rename lambda with multiple params" ~: case alphaRename (ELambda ["x", "y"] (EApp (EVar "+") [EVar "x", EVar "y"])) of
      Right (ELambda [x', y'] (EApp (EVar "+") [EVar xVar, EVar yVar])) | x' == xVar && y' == yVar && x' /= "x" && y' /= "y" -> True
      _ -> False
      ~? "Should rename multiple params"
  , "rename nested lambda" ~: case alphaRename (ELambda ["x"] (ELambda ["y"] (EApp (EVar "+") [EVar "x", EVar "y"]))) of
      Right (ELambda [x'] (ELambda [y'] (EApp (EVar "+") [EVar xVar, EVar yVar]))) | x' == xVar && y' == yVar -> True
      _ -> False
      ~? "Should rename nested lambda"
  , "rename lambda with free var" ~: case alphaRename (ELambda ["x"] (EApp (EVar "+") [EVar "x", EVar "z"])) of
      Right (ELambda [x'] (EApp (EVar "+") [EVar xVar, EVar "z"])) | x' == xVar && x' /= "x" -> True
      _ -> False
      ~? "Should preserve free var"
  , "rename lambda shadowing" ~: case alphaRename (ELambda ["x"] (ELambda ["x"] (EVar "x"))) of
      Right (ELambda [x1] (ELambda [x2] (EVar xVar))) | x2 == xVar && x1 /= x2 -> True
      _ -> False
      ~? "Should handle shadowing"
  ]

testAlphaRenameDefine :: Test
testAlphaRenameDefine = TestList
  [ "rename simple define" ~: case alphaRename (EDefine "x" (EInt 42)) of
      Right (EDefine newName (EInt 42)) | newName /= "x" -> True
      _ -> False
      ~? "Should rename define"
  , "rename define with lambda" ~: case alphaRename (EDefine "f" (ELambda ["x"] (EVar "x"))) of
      Right (EDefine f' (ELambda [x'] (EVar xVar))) | f' /= "f" && x' == xVar -> True
      _ -> False
      ~? "Should rename define with lambda"
  , "rename define with expression" ~: case alphaRename (EDefine "result" (EApp (EVar "+") [EInt 1, EInt 2])) of
      Right (EDefine newName (EApp (EVar "+") [EInt 1, EInt 2])) | newName /= "result" -> True
      _ -> False
      ~? "Should rename define with expr"
  , "rename multiple defines" ~: case alphaRename (EList [EDefine "x" (EInt 1), EDefine "y" (EInt 2)]) of
      Right (EList [EDefine x' (EInt 1), EDefine y' (EInt 2)]) | x' /= "x" && y' /= "y" && x' /= y' -> True
      _ -> False
      ~? "Should rename multiple defines"
  , "rename define referencing previous" ~: case alphaRename (EList [EDefine "x" (EInt 1), EDefine "y" (EVar "x")]) of
      Right (EList [EDefine x' (EInt 1), EDefine y' (EVar xRef)]) | x' == xRef && y' /= "y" -> True
      _ -> False
      ~? "Should rename with correct references"
  ]

testAlphaRenameLetrec :: Test
testAlphaRenameLetrec = TestList
  [ "rename letrec with recursive function" ~: case alphaRename (EList [EDefine "fact" (ELambda ["n"] (EApp (EVar "fact") [EVar "n"]))]) of
      Right (EList [EDefine fact' (ELambda [n'] (EApp (EVar factRef) [EVar nRef]))]) | fact' == factRef && n' == nRef -> True
      _ -> False
      ~? "Should handle recursive reference"
  , "rename letrec with mutual recursion" ~: case alphaRename (EList [EDefine "even?" (ELambda ["n"] (EApp (EVar "odd?") [EVar "n"])), EDefine "odd?" (ELambda ["n"] (EApp (EVar "even?") [EVar "n"]))]) of
      Right (EList [EDefine even' (ELambda [n1] (EApp (EVar oddRef) [EVar n1Ref])), EDefine odd' (ELambda [n2] (EApp (EVar evenRef) [EVar n2Ref]))]) | even' == evenRef && odd' == oddRef && n1 == n1Ref && n2 == n2Ref -> True
      _ -> False
      ~? "Should handle mutual recursion"
  , "rename letrec with body using defines" ~: case alphaRename (EList [EDefine "f" (ELambda [] (EInt 1)), EApp (EVar "f") []]) of
      Right (EList [EDefine f' (ELambda [] (EInt 1)), EApp (EVar fRef) []]) | f' == fRef -> True
      _ -> False
      ~? "Should use renamed defines in body"
  , "rename nested letrec" ~: case alphaRename (EList [EDefine "outer" (ELambda [] (EList [EDefine "inner" (ELambda [] (EInt 1)), EApp (EVar "inner") []]))]) of
      Right _ -> True
      _ -> False
      ~? "Should handle nested letrec"
  , "rename letrec with multiple recursive calls" ~: case alphaRename (EList [EDefine "f" (ELambda ["x"] (EApp (EVar "f") [EApp (EVar "f") [EVar "x"]]))]) of
      Right (EList [EDefine f' (ELambda [x'] (EApp (EVar fRef1) [EApp (EVar fRef2) [EVar xRef]]))]) | f' == fRef1 && f' == fRef2 && x' == xRef -> True
      _ -> False
      ~? "Should handle multiple recursive calls"
  ]

testAlphaRenameIf :: Test
testAlphaRenameIf = TestList
  [ "rename if expression" ~: case alphaRename (EIf (EBool True) (EInt 1) (EInt 2)) of
      Right (EIf (EBool True) (EInt 1) (EInt 2)) -> True
      _ -> False
      ~? "Should rename if"
  , "rename if with vars" ~: case alphaRename (ELambda ["x"] (EIf (EVar "x") (EVar "x") (EInt 0))) of
      Right (ELambda [x'] (EIf (EVar xRef1) (EVar xRef2) (EInt 0))) | x' == xRef1 && x' == xRef2 -> True
      _ -> False
      ~? "Should rename vars in if"
  , "rename nested if" ~: case alphaRename (EIf (EBool True) (EIf (EBool False) (EInt 1) (EInt 2)) (EInt 3)) of
      Right (EIf (EBool True) (EIf (EBool False) (EInt 1) (EInt 2)) (EInt 3)) -> True
      _ -> False
      ~? "Should rename nested if"
  , "rename if in lambda" ~: case alphaRename (ELambda ["x", "y"] (EIf (EVar "x") (EVar "y") (EInt 0))) of
      Right (ELambda [x', y'] (EIf (EVar xRef) (EVar yRef) (EInt 0))) | x' == xRef && y' == yRef -> True
      _ -> False
      ~? "Should rename if in lambda"
  , "rename if with complex branches" ~: case alphaRename (ELambda ["x"] (EIf (EVar "x") (EApp (EVar "+") [EVar "x", EInt 1]) (EApp (EVar "-") [EVar "x", EInt 1]))) of
      Right _ -> True
      _ -> False
      ~? "Should rename complex if"
  ]

testAlphaRenameApp :: Test
testAlphaRenameApp = TestList
  [ "rename application" ~: case alphaRename (EApp (EVar "f") [EInt 1, EInt 2]) of
      Right (EApp (EVar "f") [EInt 1, EInt 2]) -> True
      _ -> False
      ~? "Should rename application"
  , "rename application with bound vars" ~: case alphaRename (ELambda ["f", "x"] (EApp (EVar "f") [EVar "x"])) of
      Right (ELambda [f', x'] (EApp (EVar fRef) [EVar xRef])) | f' == fRef && x' == xRef -> True
      _ -> False
      ~? "Should rename bound vars in app"
  , "rename nested application" ~: case alphaRename (EApp (EVar "f") [EApp (EVar "g") [EInt 1]]) of
      Right (EApp (EVar "f") [EApp (EVar "g") [EInt 1]]) -> True
      _ -> False
      ~? "Should rename nested app"
  , "rename lambda application" ~: case alphaRename (EApp (ELambda ["x"] (EVar "x")) [EInt 42]) of
      Right (EApp (ELambda [x'] (EVar xRef)) [EInt 42]) | x' == xRef -> True
      _ -> False
      ~? "Should rename lambda in app"
  , "rename application with many args" ~: case alphaRename (ELambda ["a", "b", "c"] (EApp (EVar "f") [EVar "a", EVar "b", EVar "c"])) of
      Right (ELambda [a', b', c'] (EApp (EVar "f") [EVar aRef, EVar bRef, EVar cRef])) | a' == aRef && b' == bRef && c' == cRef -> True
      _ -> False
      ~? "Should rename many args"
  ]
