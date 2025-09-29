-- Valeurs manipulées à l'exécution (pas seulement des Ints)
import Debug.Trace (trace)

data Value
  = VInt Int
  | VFun (Value -> IO Value)

instance Show Value where
  show (VInt n) = show n
  show (VFun _) = "<function>"

data Expr
  = Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  | Var String
  | Let String Expr Expr
  | Lam String Expr
  | App Expr Expr
  | Print Expr
  deriving (Show)

pretty :: Expr -> String
pretty (Lit n)       = show n
pretty (Var x)       = x
pretty (Add e1 e2)   = "(" ++ pretty e1 ++ " + " ++ pretty e2 ++ ")"
pretty (Mul e1 e2)   = "(" ++ pretty e1 ++ " * " ++ pretty e2 ++ ")"
pretty (Let x e1 e2) = "let " ++ x ++ " = " ++ pretty e1 ++ " in " ++ pretty e2
pretty (Lam x body)  = "\\" ++ x ++ " -> " ++ pretty body
pretty (App f a)     = pretty f ++ " " ++ pretty a
pretty (Print e)     = "print(" ++ pretty e ++ ")"

type Env = [(String, Value)]

eval :: Env -> Expr -> IO Value
eval _   (Lit n)       = pure (VInt n)

eval env (Add e1 e2)   = do
  VInt v1 <- eval env e1
  VInt v2 <- eval env e2
  pure (VInt (v1 + v2))

eval env (Mul e1 e2)   = do
  VInt v1 <- eval env e1
  VInt v2 <- eval env e2
  pure (VInt (v1 * v2))

eval env (Var x) =
  case lookup x env of
    Just v  -> pure v
    Nothing -> error ("Variable " ++ x ++ " not found")

eval env (Let x e1 e2) = do
  v <- eval env e1
  eval ((x,v):env) e2

eval env (Lam x body)  =
  pure (VFun (\v -> eval ((x,v):env) body))

eval env (App e1 e2) = do
  VFun f <- eval env e1
  arg    <- eval env e2
  f arg

eval env (Print e) = do
  v <- eval env e
  putStrLn ("[PRINT] " ++ show v)
  pure v

main :: IO ()
main = do
  let ast =
        Let "f" (Lam "x" (Add (Var "x") (Lit 1)))
          (Print (App (Var "f") (Lit 10)))
  putStrLn "AST:"
  print ast
  putStrLn "Result:"
  _ <- eval [] ast
  pure ()