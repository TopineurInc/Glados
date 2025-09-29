module Main (main) where

data Expr
  = Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  | Var String
  | Let String Expr Expr
  deriving (Show)

type Env = [(String, Int)]

eval :: Env -> Expr -> Int
eval _   (Lit n)       = n
eval env (Add e1 e2)   = eval env e1 + eval env e2
eval env (Mul e1 e2)   = eval env e1 * eval env e2
eval env (Var x)       =
  case lookup x env of
    Just v  -> v
    Nothing -> error ("Variable " ++ x ++ " not found")
eval env (Let x e1 e2) =
  let v = eval env e1
   in eval ((x, v):env) e2

main :: IO ()
main = do
  let ast = Let "x" (Lit 5) (Add (Var "x") (Mul (Lit 2) (Var "x")))
  print (eval [] ast)