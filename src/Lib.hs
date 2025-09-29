module Lib
    ( someFunc
    ) where

import AST

someFunc :: IO ()
someFunc = do
  let pos = SourcePos 1 1
      atom1 = AInteger 42
      atom2 = ASymbol "factorial"
      sexpr1 = SAtom atom1 (Just pos)
      sexpr2 = SAtom atom2 (Just pos)
      list = SList [sexpr2, sexpr1] (Just pos)
  print list
