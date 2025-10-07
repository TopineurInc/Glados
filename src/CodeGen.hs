{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- CodeGen
-}

module CodeGen
  ( generateCode
  , generateCodeWithDefs
  , CodeGenState(..)
  , emptyCodeGenState
  ) where

import AST
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Vector as Vector

data CodeGenState = CodeGenState
  { cgsCounter :: Int
  , cgsConstants :: [Constant]
  , cgsInstructions :: [Instr]
  , cgsLabels :: Map.Map Label Int
  , cgsCodeObjects :: Map.Map Name CodeObject
  , cgsLocalMap :: Map.Map Name Int
  , cgsMaxLocals :: Int
  } deriving (Show)

emptyCodeGenState :: CodeGenState
emptyCodeGenState = CodeGenState 0 [] [] Map.empty Map.empty Map.empty 0

type CodeGenM = State CodeGenState

runCodeGen :: CodeGenM a -> CodeGenState -> (a, CodeGenState)
runCodeGen = runState

generateCode :: Name -> Expr -> Either CompileError CodeObject
generateCode name expr =
  fst (generateCodeWithDefs name expr)

generateCodeWithDefs :: Name -> Expr -> (Either CompileError CodeObject, Map.Map Name CodeObject)
generateCodeWithDefs name expr =
  let buildAction = compileExpr expr >> emit IReturn
      (_, finalState) = runCodeGen buildAction emptyCodeGenState
      mainCode = buildMainCode name expr finalState
  in (Right mainCode, cgsCodeObjects finalState)

buildMainCode :: Name -> Expr -> CodeGenState -> CodeObject
buildMainCode name expr state =
  CodeObject
    { coName = name
    , coArity = getArity expr
    , coMaxLocals = cgsMaxLocals state
    , coConsts = Vector.fromList (reverse $ cgsConstants state)
    , coInstrs = Vector.fromList (reverse $ cgsInstructions state)
    , coLabelMap = cgsLabels state
    }

getArity :: Expr -> Int
getArity (ELambda params _) = length params
getArity _ = 0

emit :: Instr -> CodeGenM ()
emit instr = modify $ \s -> s { cgsInstructions = instr : cgsInstructions s }

addConst :: Constant -> CodeGenM Int
addConst c =
  get >>= \s ->
    let consts = cgsConstants s
        idx = length consts
    in put s { cgsConstants = c : consts } >> pure idx

emitConst :: Constant -> CodeGenM ()
emitConst = (>>= emit . IConst) . addConst

recordPosition :: CodeGenM Int
recordPosition = gets (length . cgsInstructions)

allocLocal :: Name -> CodeGenM Int
allocLocal name =
  get >>= \s ->
    let localMap = cgsLocalMap s
        idx = Map.size localMap
        maxLocals = max (idx + 1) (cgsMaxLocals s)
    in put s
        { cgsLocalMap = Map.insert name idx localMap
        , cgsMaxLocals = maxLocals
        } >> pure idx

getLocal :: Name -> CodeGenM (Maybe Int)
getLocal name = Map.lookup name <$> gets cgsLocalMap

compileExpr :: Expr -> CodeGenM ()
compileExpr expr =
  case expr of
    EInt _ -> compileLiteral expr
    EBool _ -> compileLiteral expr
    EString _ -> compileLiteral expr
    EVar name -> compileVar name
    EDefine name value -> compileDefinition name value
    EList exprs -> compileList exprs
    _ -> compileComplex expr

compileComplex :: Expr -> CodeGenM ()
compileComplex expr =
  case expr of
    EIf cond thenE elseE -> compileIf cond thenE elseE
    ELambda params body -> compileLambdaLiteral params body
    EApp func args -> compileApplication func args
    EQuote _ -> pure ()
    _ -> pure ()

compileLiteral :: Expr -> CodeGenM ()
compileLiteral (EInt n) = emitConst (CInt n)
compileLiteral (EBool b) = emitConst (CBool b)
compileLiteral (EString s) = emitConst (CString s)
compileLiteral _ = pure ()

compileVar :: Name -> CodeGenM ()
compileVar name =
  getLocal name >>= maybe (emitConst (CFuncRef name)) (emit . ILoad)

compileIf :: Expr -> Expr -> Expr -> CodeGenM ()
compileIf cond thenE elseE =
  compileExpr cond
    >> emit (IJumpIfFalse 0)
    >> recordPosition >>= compileThenBranch thenE elseE

compileLambdaLiteral :: [Name] -> Expr -> CodeGenM ()
compileLambdaLiteral _ _ = pure ()

compileApplication :: Expr -> [Expr] -> CodeGenM ()
compileApplication (EVar name) args = compileNamedApp name args
compileApplication func args = compileGeneralApp func args

compileNamedApp :: Name -> [Expr] -> CodeGenM ()
compileNamedApp name args =
  mapM_ compileExpr args >> emitCall
  where
    arity = length args
    emitCall =
      case Map.lookup name primitiveInstrs of
        Just instr -> emit instr
        Nothing -> emit (ICall arity name)

compileGeneralApp :: Expr -> [Expr] -> CodeGenM ()
compileGeneralApp func args =
  compileExpr func
    >> mapM_ compileExpr args
    >> emit (ICall (length args) "<lambda>")

compileDefinition :: Name -> Expr -> CodeGenM ()
compileDefinition name value =
  case value of
    ELambda params body -> compileLambdaDefinition name params body
    _ -> compileValueDefinition name value

compileThenBranch :: Expr -> Expr -> Int -> CodeGenM ()
compileThenBranch thenE elseE elseIdx =
  compileExpr thenE
    >> emit (IJump 0)
    >> recordPosition >>= \endIdx ->
         finalizeThen endIdx
  where
    finalizeThen endIdx =
      patchJump (elseIdx - 1) endIdx >> compileElseBranch elseE endIdx

compileElseBranch :: Expr -> Int -> CodeGenM ()
compileElseBranch elseE endIdx =
  compileExpr elseE
    >> recordPosition >>= finalize
  where
    finalize finalIdx =
      patchJump (endIdx - 1) finalIdx

compileLambdaDefinition :: Name -> [Name] -> Expr -> CodeGenM ()
compileLambdaDefinition name params body =
  compileLambda name params body >>= storeLambdaDefinition name

storeLambdaDefinition :: Name -> CodeObject -> CodeGenM ()
storeLambdaDefinition name codeObj =
  registerCodeObject name codeObj
    >> emitConst (CFuncRef name)
    >> storeWithDummy name

compileValueDefinition :: Name -> Expr -> CodeGenM ()
compileValueDefinition name value =
  compileExpr value >> storeWithDummy name

compileList :: [Expr] -> CodeGenM ()
compileList [] = pure ()
compileList [e] = compileExpr e
compileList (e:rest) =
  compileExpr e >> emit IPop >> compileList rest

storeWithDummy :: Name -> CodeGenM ()
storeWithDummy name =
  allocLocal name >>= storeSlotWithDummy

storeSlotWithDummy :: Int -> CodeGenM ()
storeSlotWithDummy slot =
  emit (IStore slot) >> emitConst (CInt 0)

primitiveInstrs :: Map.Map Name Instr
primitiveInstrs = Map.fromList primitiveInstrEntries

primitiveInstrEntries :: [(Name, Instr)]
primitiveInstrEntries = map toEntry primitiveSpecs

toEntry :: (Name, String) -> (Name, Instr)
toEntry (name, opcode) = (name, IPrim opcode)

primitiveSpecs :: [(Name, String)]
primitiveSpecs = map (\op -> (op, op)) allPrimitiveNames

allPrimitiveNames :: [String]
allPrimitiveNames =
  arithmeticPrimNames
    ++ comparisonPrimNames
    ++ ioPrimNames
    ++ stringPrimNames
    ++ logicPrimNames

arithmeticPrimNames :: [String]
arithmeticPrimNames = ["+", "-", "*", "div", "mod"]

comparisonPrimNames :: [String]
comparisonPrimNames = ["eq?", "<", ">"]

ioPrimNames :: [String]
ioPrimNames = ["print", "display", "input", "read-line"]

stringPrimNames :: [String]
stringPrimNames =
  [ "string->number"
  , "number->string"
  , "string-length"
  , "string-append"
  , "substring"
  ]

logicPrimNames :: [String]
logicPrimNames = ["not", "and", "or"]

patchJump :: Int -> Int -> CodeGenM ()
patchJump instrIdx target =
  get >>= \state ->
    let reverseIdx = length (cgsInstructions state) - instrIdx - 1
        maybeParts = splitInstruction reverseIdx (cgsInstructions state)
    in case maybeParts of
         Nothing -> pure ()
         Just parts -> applyPatch state target parts

compileLambda :: Name -> [Name] -> Expr -> CodeGenM CodeObject
compileLambda name params body =
  let lambdaState = runLambda params body
      codeObj = buildLambdaCode name params lambdaState
  in registerNestedObjects lambdaState >> pure codeObj

registerCodeObject :: Name -> CodeObject -> CodeGenM ()
registerCodeObject name codeObj =
  modify $ \s ->
    s { cgsCodeObjects = Map.insert name codeObj (cgsCodeObjects s) }

compileLambdaBody :: [Name] -> Expr -> CodeGenM ()
compileLambdaBody params body =
  mapM_ allocLocal params >> compileExpr body >> emit IReturn

splitInstruction :: Int -> [Instr] -> Maybe ([Instr], Instr, [Instr])
splitInstruction idx instrs
  | idx < 0 || idx >= length instrs = Nothing
  | otherwise =
      let (before, rest) = splitAt idx instrs
      in case rest of
           instr:after -> Just (before, instr, after)
           _ -> Nothing

applyPatch :: CodeGenState -> Int -> ([Instr], Instr, [Instr]) -> CodeGenM ()
applyPatch state target (before, instr, after) =
  let patched = patchInstruction instr target
      rebuilt = before ++ patched : after
  in put state { cgsInstructions = rebuilt }

patchInstruction :: Instr -> Int -> Instr
patchInstruction (IJumpIfFalse _) target = IJumpIfFalse target
patchInstruction (IJump _) target = IJump target
patchInstruction other _ = other

runLambda :: [Name] -> Expr -> CodeGenState
runLambda params body =
  snd (runCodeGen (compileLambdaBody params body) emptyCodeGenState)

buildLambdaCode :: Name -> [Name] -> CodeGenState -> CodeObject
buildLambdaCode name params state =
  CodeObject
    { coName = name
    , coArity = length params
    , coMaxLocals = cgsMaxLocals state
    , coConsts = vectorFromReverse (cgsConstants state)
    , coInstrs = vectorFromReverse (cgsInstructions state)
    , coLabelMap = cgsLabels state
    }

registerNestedObjects :: CodeGenState -> CodeGenM ()
registerNestedObjects lambdaState =
  modify $ \state ->
    let merged =
          Map.union
            (cgsCodeObjects state)
            (cgsCodeObjects lambdaState)
    in state { cgsCodeObjects = merged }

vectorFromReverse :: [a] -> Vector.Vector a
vectorFromReverse = Vector.fromList . reverse
