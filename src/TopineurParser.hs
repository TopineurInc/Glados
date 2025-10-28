{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- TopineurParser
-}

{-# LANGUAGE DeriveGeneric #-}

module TopineurParser
  ( parseTopineurSource
  ) where

import Control.Applicative (many, empty)
import Control.Monad (void, when)
import Data.Char (isAlphaNum, isLetter, isAsciiUpper)
import Data.List (intercalate)
import GHC.Generics (Generic)
import qualified Text.Parsec as P
import Text.Parsec.Prim (getState, putState)
import Text.Parsec
    ( (<|>)
      , ParseError
      , Parsec
      , anyChar
      , between
      , char
      , choice
      , eof
      , getPosition
      , lookAhead
      , many1
      , manyTill
      , newline
      , noneOf
      , notFollowedBy
      , oneOf
      , option
      , optionMaybe
      , optional
      , runParser
      , satisfy
      , sepBy
      , sepBy1
      , skipMany
      , sourceColumn
      , sourceLine
      , string
      , try
      , (<?>)
      )

data CompileError
  = ParseError String Loc
  | SyntaxError String Loc
  deriving (Eq, Show, Generic)

data SourcePos = SourcePos
  { spLine :: Int
  , spCol  :: Int
  } deriving (Eq, Show, Ord, Generic)

type Loc = SourcePos

type Name = String

toLoc :: P.SourcePos -> Loc
toLoc p = SourcePos (sourceLine p) (sourceColumn p - 1)

data Topineur = Topineur
  { tPackage :: Name
  , tImports :: [Name]
  , tDecls   :: [Decl]
  } deriving (Eq, Show, Generic)

data Decl
  = DLet Loc Pattern Expression
  | DFunc Loc [Decorator] Name [Param] (Maybe TypeAnn) Block
  | DObjectType Loc Name [ObjMember]
  deriving (Eq, Show, Generic)

data Decorator = Decorator Loc Name [Expression]
  deriving (Eq, Show, Generic)

data ObjMember
  = OMField Loc Name TypeAnn (Maybe Expression)
  | OMFunc Decl
  deriving (Eq, Show, Generic)

data Param
  = PVar Loc Name TypeAnn
  | PTuple Loc [Param]
  deriving (Eq, Show, Generic)

data Pattern
  = PVarPat Loc Name (Maybe TypeAnn)
  | PTuplePat Loc [Pattern]
  deriving (Eq, Show, Generic)

data Block = Block Loc [Stmt]
  deriving (Eq, Show, Generic)

data Stmt
  = STop Loc Expression
  | SLet Loc Pattern Expression
  | SIf Loc Expression SimpleStmt (Maybe SimpleStmt)
  | SWhile Loc Expression [Stmt]
  | SFor Loc Name Range [Stmt]
  | SAssign Loc LValue Expression
  | SExpression Loc Expression
  deriving (Eq, Show, Generic)

data SimpleStmt
  = SSLet Loc Pattern Expression
  | SSAssign Loc LValue Expression
  | SSTop Loc Expression
  | SSExpression Loc Expression
  deriving (Eq, Show, Generic)

data LValue
  = LVar Loc Name
  | LMember Loc Expression Name
  deriving (Eq, Show, Generic)

data Range = Range Expression Expression
  deriving (Eq, Show, Generic)

data Expression
  = EVar Loc Name
  | ESelf Loc
  | EInt Loc Integer
  | EFloat Loc Double
  | EString Loc String
  | ETuple Loc [Expression]
  | EArray Loc [Expression]
  | EObject Loc Name [FieldAssign]
  | ECall Loc Expression [Expression]
  | EMethodCall Loc Expression Name [Expression]
  | EMember Loc Expression Name
  | EIf Loc Expression Expression (Maybe Expression)
  | ELet Loc Name Expression Expression
  | ELambda Loc [Param] (Maybe TypeAnn) Expression
  | EBinOp Loc String Expression Expression
  | EParens Loc Expression
  deriving (Eq, Show, Generic)

data FieldAssign = FieldAssign Loc (Either (Expression, Name) Name) Expression
  deriving (Eq, Show, Generic)

data TypeAnn
  = TIdent Loc Name
  | TUpperIdent Loc Name
  | TGeneric Loc Name [TypeAnn]
  | TTuple Loc [TypeAnn]
  deriving (Eq, Show, Generic)

locOfE :: Expression -> Loc
locOfE e =
  case e of
    EVar l _ -> l
    ESelf l -> l
    EInt l _ -> l
    EFloat l _ -> l
    EString l _ -> l
    ETuple l _ -> l
    EArray l _ -> l
    EObject l _ _ -> l
    ECall l _ _ -> l
    EMethodCall l _ _ _ -> l
    EMember l _ _ -> l
    EIf l _ _ _ -> l
    ELet l _ _ _ -> l
    ELambda l _ _ _ -> l
    EBinOp l _ _ _ -> l
    EParens l _ -> l

data IndentState = IndentState
  { isUnit   :: Maybe Int
  , isStack  :: [Int]
  } deriving (Show, Eq)

type Parser a = Parsec String IndentState a

initState :: IndentState
initState = IndentState { isUnit = Nothing, isStack = [0] }

noTabs :: Parser ()
noTabs = do
  pos <- getPosition
  let go = do
        _ <- try (char '\t')
        failAt pos "Tab characters are not allowed; use spaces."
      in return () <|> go

failAt :: P.SourcePos -> String -> Parser a
failAt p msg = fail (msg ++ " at " ++ show (sourceLine p) ++ ":" ++ show (sourceColumn p))

lineComment :: Parser ()
lineComment = do
  _ <- try (string "|-")
  _ <- manyTill anyChar (try newline <|> (eof >> return '\n'))
  return ()

scn :: Parser ()
scn = skipMany (noTabs *> void (oneOf " \r"))

scnWithComments :: Parser ()
scnWithComments = skipMany (noTabs *> (void (oneOf " \r\n") <|> lineComment))

sc :: Parser ()
sc = skipMany (noTabs *> (void (oneOf " \t\r") <|> lineCommentNoNewline))
  where
    lineCommentNoNewline =
      try (string "|-") >> skipMany (satisfy (/= '\n'))

scML :: Parser ()
scML = skipMany (noTabs *> (void (oneOf " \t\r\n") <|> lineComment))

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  sc
  return x

lexemeML :: Parser a -> Parser a
lexemeML p = do
  x <- p
  scML
  return x

symbol :: String -> Parser String
symbol = lexeme . string

symbolML :: String -> Parser String
symbolML = lexemeML . string

newline1 :: Parser ()
newline1 = do
  _ <- many1 (char '\n')
  scn

withIndentedBlock :: Parser a -> Parser [a]
withIndentedBlock p = do
  st <- getState
  let baseCol = case isStack st of
                  (top:_) -> top
                  [] -> 0
  _ <- many1 (char '\n')
  col <- countSpacesAtLineStart
  unit <- case isUnit st of
    Nothing ->
      if col - baseCol == 2 || col - baseCol == 4
        then do
          putState st { isUnit = Just (col - baseCol), isStack = col : isStack st }
          return (col - baseCol)
        else do
          pos <- getPosition
          failAt pos
            "Indentation must be 2 or 4 spaces and consistent across the file."
    Just u -> return u
  when (col - baseCol /= unit && col - baseCol /= unit * 2) $ do
    pos <- getPosition
    failAt pos ("Expected indentation of exactly " ++ show unit ++ " or " ++ show (unit * 2) ++ " spaces.")
  many1 (indentedLine col p)
  where
    countSpacesAtLineStart = do
      n <- length <$> many (char ' ')
      return n
    indentedLine targetCol p' = do
      x <- p'
      choice
        [ try $ do
            _ <- newline
            n <- length <$> many (char ' ')
            sc
            if n == targetCol
              then return x
              else fail "__DEDENT__"
        , return x
        ]

isIdentStart :: Char -> Bool
isIdentStart c = isLetter c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

reservedWords :: [String]
reservedWords = ["for", "in", "do", "end", "if", "then", "else", "while", "let", "top", "def", "object", "type", "self", "package", "import", "fun"]

ident :: Parser (Loc, Name)
ident = lexeme $ do
  pos <- getPosition
  h <- satisfy isIdentStart <?> "identifier"
  t <- many (satisfy isIdentChar)
  let name = h : t
  if name `elem` reservedWords
    then fail $ "keyword " ++ name ++ " cannot be used as identifier"
    else return (toLoc pos, name)

identML :: Parser (Loc, Name)
identML = lexemeML $ do
  pos <- getPosition
  h <- satisfy isIdentStart <?> "identifier"
  t <- many (satisfy isIdentChar)
  let name = h : t
  if name `elem` reservedWords
    then fail $ "keyword " ++ name ++ " cannot be used as identifier"
    else return (toLoc pos, name)

upperIdent :: Parser (Loc, Name)
upperIdent = lexeme $ do
  pos <- getPosition
  h <- satisfy isAsciiUpper <?> "type identifier"
  t <- many (satisfy isIdentChar)
  return (toLoc pos, h : t)

stringLit :: Parser (Loc, String)
stringLit = lexeme $ do
  pos <- getPosition
  _ <- char '"'
  s <- many (noneOf "\"")
  _ <- char '"'
  return (toLoc pos, s)

intLit :: Parser (Loc, Integer)
intLit = lexeme $ do
  pos <- getPosition
  ds <- many1 (oneOf "0123456789")
  notFollowedBy (try $ char '.' >> oneOf "0123456789")
  return (toLoc pos, read ds)

floatLit :: Parser (Loc, Double)
floatLit = lexeme $ do
  pos <- getPosition
  a <- many1 (oneOf "0123456789")
  _ <- char '.'
  b <- many1 (oneOf "0123456789")
  return (toLoc pos, read (a ++ "." ++ b))

selfKw :: Parser Loc
selfKw = lexeme $ do
  pos <- getPosition
  _ <- try (string "self" >> notFollowedBy (satisfy isIdentChar))
  return (toLoc pos)

keyword :: String -> Parser Loc
keyword kw = lexeme $ do
  pos <- getPosition
  _ <- try (string kw >> notFollowedBy (satisfy isIdentChar))
  return (toLoc pos)

keywordML :: String -> Parser Loc
keywordML kw = lexemeML $ do
  pos <- getPosition
  _ <- try (string kw >> notFollowedBy (satisfy isIdentChar))
  return (toLoc pos)

typeAnn :: Parser TypeAnn
typeAnn = try tupleType <|> try genericType <|> try upperType <|> identType
  where
    identType = do
      (l, n) <- ident
      return (TIdent l n)
    upperType = do
      (l, n) <- upperIdent
      return (TUpperIdent l n)
    genericType = do
      (l, n) <- ident
      _ <- symbol "["
      ts <- typeAnn `sepBy1` symbol ","
      _ <- symbol "]"
      return (TGeneric l n ts)
    tupleType = do
      l0 <- keyword "Tuple"
      _ <- symbol "["
      ts <- typeAnn `sepBy1` symbol ","
      _ <- symbol "]"
      return (TTuple l0 ts)

param :: Parser Param
param = try tupleParam <|> singleParam
  where
    singleParam = do
      (l, n) <- ident
      mt <- optionMaybe (symbol ":" >> typeAnn)
      return (PVar l n (maybe (TIdent l "Any") id mt))
    tupleParam = do
      pos <- getPosition
      _ <- symbol "("
      ps <- param `sepBy1` symbol ","
      _ <- symbol ")"
      return (PTuple (toLoc pos) ps)

patternP :: Parser Pattern
patternP = try tuplePat <|> varPat
  where
    varPat = do
      (l, n) <- ident
      mt <- optionMaybe (symbol ":" >> typeAnn)
      return (PVarPat l n mt)
    tuplePat = do
      pos <- getPosition
      _ <- symbol "("
      ps <- patternP `sepBy1` symbol ","
      _ <- symbol ")"
      return (PTuplePat (toLoc pos) ps)

expr :: Parser Expression
expr = makeBinOps

basicTerm :: Parser Expression
basicTerm =
  choice
    [ do ESelf <$> selfKw
    , try $ do
        (l, i) <- intLit
        return (EInt l i)
    , try $ do
        (l, d) <- floatLit
        return (EFloat l d)
    , try $ do (l, s) <- stringLit; return (EString l s)
    , try tupleExpr
    , try arrayExpr
    , try objectLit
    , try anonFun
    , try parensExpr
    , varOrUpper
    ]

rangeBound :: Parser Expression
rangeBound =
  choice
    [ do ESelf <$> selfKw
    , try $ do
        (l, i) <- intLit
        return (EInt l i)
    , try $ do
        (l, d) <- floatLit
        return (EFloat l d)
    , try $ do (l, s) <- stringLit; return (EString l s)
    , try tupleExpr
    , try arrayExpr
    , try objectLit
    , try anonFun
    , try parensExpr
    , varOrUpper
    ]

singleTerm :: Parser Expression
singleTerm = basicTerm >>= postfix

term :: Parser Expression
term = basicTerm >>= postfix

postfix :: Expression -> Parser Expression
postfix e0 =
  choice
    [ try $ do
        pos <- getPosition
        notFollowedBy (string "..")
        _ <- char '.'
        sc
        (_, n) <- ident
        args <- option [] callArgs
        let l = toLoc pos
        let base = EMember l e0 n
        let res = if null args then base else EMethodCall l e0 n args
        postfix res
    , try $ do
        args <- callArgs
        let l = locOfE e0 in postfix (ECall l e0 args)
    , return e0
    ]

callArgs :: Parser [Expression]
callArgs = between (symbol "(") (symbol ")") (argExpr `sepBy` symbol ",")
  where
    argExpr = do
      scML
      exprML

exprML :: Parser Expression
exprML = makeBinOpsML

makeBinOpsML :: Parser Expression
makeBinOpsML = level1ML
  where
    bin l op r = EBinOp (locOfE l) op l r
    level1ML = chainlML level2ML ["++"]
    level2ML = chainlML level3ML ["=="]
    level3ML = chainlML level4ML ["<=", ">=", "<", ">"]
    level4ML = chainlML level5ML [".."]
    level5ML = chainlML level6ML ["+", "-"]
    level6ML = chainlML termML ["*", "/", "%"]

    chainlML :: Parser Expression -> [String] -> Parser Expression
    chainlML p ops =
      if null ops
        then p
        else do
          l <- p
          rest l
      where
        rest l = do
          mo <- optionMaybe (choice (map tryOpML ops))
          case mo of
            Nothing -> return l
            Just op -> do
              r <- p
              rest (bin l op r)
        tryOpML op = try $ do
          _ <- string op
          notFollowedBy (oneOf "+-*/<>=!")
          scn
          return op

termML :: Parser Expression
termML = basicTermML >>= postfixML

basicTermML :: Parser Expression
basicTermML =
  choice
    [ do ESelf <$> selfKw
    , try $ do
        (l, i) <- intLit
        return (EInt l i)
    , try $ do
        (l, d) <- floatLit
        return (EFloat l d)
    , try $ do (l, s) <- stringLit; return (EString l s)
    , try tupleExprML
    , try arrayExprML
    , try objectLitML
    , try anonFunML
    , try parensExprML
    , varOrUpperML
    ]

tupleExprML :: Parser Expression
tupleExprML = do
  pos <- getPosition
  _ <- symbolML "("
  a <- exprML
  _ <- symbolML ","
  bs <- exprML `sepBy1` symbolML ","
  _ <- symbolML ")"
  return (ETuple (toLoc pos) (a : bs))

arrayExprML :: Parser Expression
arrayExprML = do
  pos <- getPosition
  _ <- symbolML "["
  xs <- exprML `sepBy` symbolML ","
  _ <- symbolML "]"
  return (EArray (toLoc pos) xs)

objectLitML :: Parser Expression
objectLitML = do
  (l, n) <- identML
  _ <- symbolML "{"
  fas <- fieldAssignML `sepBy` symbolML ","
  _ <- symbolML "}"
  return (EObject l n fas)

fieldAssignML :: Parser FieldAssign
fieldAssignML = do
    pos <- getPosition
    lhs <- try
               (do base <- basicTermML
                   _ <- symbolML "."
                   (_, fld) <- identML
                   return (Left (base, fld)))
               <|>
                 (do (_, n) <- identML
                     return (Right n))
    _ <- symbolML "="
    FieldAssign (toLoc pos) lhs <$> exprML

anonFunML :: Parser Expression
anonFunML = do
  pos <- keywordML "fun"
  _ <- symbolML "("
  ps <- param `sepBy` symbolML ","
  _ <- symbolML ")"
  rt <- optionMaybe (symbolML ":" >> typeAnn)
  _ <- symbolML "->"
  body <- exprML
  return (ELambda pos ps rt body)

parensExprML :: Parser Expression
parensExprML = do
  pos <- getPosition
  _ <- symbolML "("
  e <- exprML
  _ <- symbolML ")"
  return (EParens (toLoc pos) e)

varOrUpperML :: Parser Expression
varOrUpperML = do
  (l, n) <- identML
  return (EVar l n)

postfixML :: Expression -> Parser Expression
postfixML e0 =
  choice
    [ try $ do
        pos <- getPosition
        notFollowedBy (string "..")
        _ <- char '.'
        scn
        (_, n) <- identML
        args <- option [] callArgsML
        let l = toLoc pos
        let base = EMember l e0 n
        let res = if null args then base else EMethodCall l e0 n args
        postfixML res
    , try $ do
        args <- callArgsML
        let l = locOfE e0 in postfixML (ECall l e0 args)
    , return e0
    ]

callArgsML :: Parser [Expression]
callArgsML = between (symbolML "(") (symbolML ")") (exprML `sepBy` symbolML ",")

parensExpr :: Parser Expression
parensExpr = do
  pos <- getPosition
  _ <- symbol "("
  e <- expr
  _ <- symbol ")"
  return (EParens (toLoc pos) e)

tupleExpr :: Parser Expression
tupleExpr = do
  pos <- getPosition
  _ <- symbol "("
  a <- expr
  _ <- symbol ","
  bs <- expr `sepBy1` symbol ","
  _ <- symbol ")"
  return (ETuple (toLoc pos) (a : bs))

arrayExpr :: Parser Expression
arrayExpr = do
  pos <- getPosition
  _ <- symbol "["
  xs <- expr `sepBy` symbol ","
  _ <- symbol "]"
  return (EArray (toLoc pos) xs)

objectLit :: Parser Expression
objectLit = do
  (l, n) <- ident
  _ <- symbol "{"
  fas <- fieldAssign `sepBy` symbol ","
  _ <- symbol "}"
  return (EObject l n fas)

fieldAssign :: Parser FieldAssign
fieldAssign = do
    pos <- getPosition
    lhs <- try
               (do base <- basicTerm
                   _ <- symbol "."
                   (_, fld) <- ident
                   return (Left (base, fld)))
               <|>
                 (do (_, n) <- ident
                     return (Right n))
    _ <- symbol "="
    FieldAssign (toLoc pos) lhs <$> expr

varOrUpper :: Parser Expression
varOrUpper = do
  (l, n) <- ident
  return (EVar l n)

anonFun :: Parser Expression
anonFun = do
  pos <- keyword "fun"
  _ <- symbol "("
  ps <- param `sepBy` symbol ","
  _ <- symbol ")"
  rt <- optionMaybe (symbol ":" >> typeAnn)
  _ <- symbol "->"
  body <- expr
  return (ELambda pos ps rt body)

makeBinOps :: Parser Expression
makeBinOps = level1
  where
    bin l op r = EBinOp (locOfE l) op l r
    level1 = chainl level2 ["++"]
    level2 = chainl level3 ["=="]
    level3 = chainl level4 ["<=", ">=", "<", ">"]
    level4 = chainl level5 [".."]
    level5 = chainl level6 ["+", "-"]
    level6 = chainl term ["*", "/", "%"]

    chainl :: Parser Expression -> [String] -> Parser Expression
    chainl p ops =
      if null ops
        then p
        else do
          l <- p
          rest l
      where
        rest l = do
          mo <- optionMaybe (choice (map tryOp ops))
          case mo of
            Nothing -> return l
            Just op -> do
              r <- p
              rest (bin l op r)
        tryOp op = try $ do
          _ <- string op
          notFollowedBy (oneOf "+-*/<>=!")
          sc
          return op

simpleStmt :: Parser SimpleStmt
simpleStmt =
  choice
    [ try $ do
        l <- keyword "top"
        e <- expr
        return (SSTop l e)
    , try $ do
        l <- keyword "let"
        pat <- patternP
        _ <- symbol "="
        v <- expr
        return (SSLet l pat v)
    , try $ do
        (lv, l) <- lvalue
        _ <- symbol "="
        v <- expr
        return (SSAssign l lv v)
    , do
        e <- expr
        return (SSExpression (locOfE e) e)
    ]

stmt :: Parser Stmt
stmt =
  choice
    [ try $ do
        l <- keyword "top"
        e <- expr
        return (STop l e)
    , try $ do
        l <- keyword "let"
        pat <- patternP
        _ <- symbol "="
        v <- expr
        return (SLet l pat v)
    , try ifStmt
    , try whileStmt
    , try forStmt
    , try $ do
        (lv, l0) <- lvalue
        _ <- symbol "="
        v <- expr
        return (SAssign l0 lv v)
    , try $ do
        e <- expr
        return (SExpression (locOfE e) e)
    ]

lvalue :: Parser (LValue, Loc)
lvalue =
  try
    ( do
        base <- term
        _ <- symbol "."
        (l, n) <- ident
        return (LMember l base n, l)
    )
    <|> do
      (l, n) <- ident
      return (LVar l n, l)

ifStmt :: Parser Stmt
ifStmt = do
  l <- keyword "if"
  c <- expr
  _ <- keyword "then"
  sThen <-
    try (inlineSimple) <|> do
      ss <- withIndentedBlock simpleStmt
      case ss of
        [x] -> return x
        _ -> let p = l in fail ("then-branch must contain exactly one simple statement at " ++ show (spLine p) ++ ":" ++ show (spCol p))
  mElse <-
    optionMaybe $ do
      _ <- try (newline1 >> return ()) <|> sc
      _ <- keyword "else"
      try inlineSimple <|> do
        ss <- withIndentedBlock simpleStmt
        case ss of
          [x] -> return x
          _ -> let p = l in fail ("else-branch must contain exactly one simple statement at " ++ show (spLine p) ++ ":" ++ show (spCol p))
  return (SIf l c sThen mElse)
  where
    inlineSimple = do
      sc
      simpleStmt

whileStmt :: Parser Stmt
whileStmt = do
  l <- keyword "while"
  c <- expr
  _ <- keyword "do"
  body <- blockIndentedStmts
  _ <- keyword "end"
  return (SWhile l c body)

forStmt :: Parser Stmt
forStmt = do
  l <- keyword "for"
  (_, n) <- ident
  _ <- keyword "in"
  rangeExpr <- expr
  case rangeExpr of
    EBinOp _ ".." a b -> do
      _ <- keyword "do"
      body <- blockIndentedStmts
      _ <- keyword "end"
      return (SFor l n (Range a b) body)
    _ -> fail "Expected range expression with '..'"

blockIndentedStmts :: Parser [Stmt]
blockIndentedStmts = do
  ss <- withIndentedBlockML stmtML
  return ss

withIndentedBlockML :: Parser a -> Parser [a]
withIndentedBlockML p = do
  st <- getState
  let baseCol = case isStack st of
                  (top:_) -> top
                  [] -> 0
  _ <- many1 (char '\n')
  col <- countSpacesAtLineStartML
  unit <- case isUnit st of
    Nothing ->
      if col - baseCol == 2 || col - baseCol == 4
        then do
          putState st { isUnit = Just (col - baseCol), isStack = col : isStack st }
          return (col - baseCol)
        else do
          pos <- getPosition
          failAt pos
            "Indentation must be 2 or 4 spaces and consistent across the file."
    Just u -> return u
  when (col - baseCol /= unit && col - baseCol /= unit * 2) $ do
    pos <- getPosition
    failAt pos ("Expected indentation of exactly " ++ show unit ++ " or " ++ show (unit * 2) ++ " spaces.")
  first <- p
  rest <- many $ try $ do
    _ <- char '\n'
    n <- length <$> many (char ' ')
    if n == col
      then do
        sc
        p
      else empty
  return (first : rest)
  where
    countSpacesAtLineStartML = do
      n <- length <$> many (char ' ')
      return n

stmtML :: Parser Stmt
stmtML =
  choice
    [ try $ do
        l <- keywordML "top"
        e <- exprML
        return (STop l e)
    , try $ do
        l <- keywordML "let"
        pat <- patternP
        _ <- symbolML "="
        v <- exprML
        return (SLet l pat v)
    , try ifStmtML
    , try whileStmtML
    , try forStmtML
    , try $ do
        (lv, l0) <- lvalueML
        _ <- symbolML "="
        v <- exprML
        return (SAssign l0 lv v)
    , do
        e <- exprML
        return (SExpression (locOfE e) e)
    ]

lvalueML :: Parser (LValue, Loc)
lvalueML =
  try
    ( do
        base <- termML
        _ <- symbolML "."
        (l, n) <- identML
        return (LMember l base n, l)
    )
    <|> do
      (l, n) <- identML
      return (LVar l n, l)

ifStmtML :: Parser Stmt
ifStmtML = do
  l <- keywordML "if"
  c <- exprML
  _ <- keywordML "then"
  sThen <-
    try (inlineSimpleML) <|> do
      ss <- withIndentedBlock simpleStmtML
      case ss of
        [x] -> return x
        _ -> let p = l in fail ("then-branch must contain exactly one simple statement at " ++ show (spLine p) ++ ":" ++ show (spCol p))
  mElse <-
    optionMaybe $ do
      _ <- try (newline1 >> return ()) <|> sc
      _ <- keywordML "else"
      try inlineSimpleML <|> do
        ss <- withIndentedBlock simpleStmtML
        case ss of
          [x] -> return x
          _ -> let p = l in fail ("else-branch must contain exactly one simple statement at " ++ show (spLine p) ++ ":" ++ show (spCol p))
  return (SIf l c sThen mElse)
  where
    inlineSimpleML = do
      sc
      simpleStmtML

whileStmtML :: Parser Stmt
whileStmtML = do
  l <- keywordML "while"
  c <- exprML
  _ <- keywordML "do"
  body <- blockIndentedStmts
  newline1
  _ <- string "end"
  return (SWhile l c body)

forStmtML :: Parser Stmt
forStmtML = do
  l <- keywordML "for"
  (_, n) <- identML
  _ <- keywordML "in"
  rangeExpr <- exprML
  case rangeExpr of
    EBinOp _ ".." a b -> do
      _ <- keywordML "do"
      body <- blockIndentedStmts
      newline1
      _ <- string "end"
      return (SFor l n (Range a b) body)
    _ -> fail "Expected range expression with '..'"

simpleStmtML :: Parser SimpleStmt
simpleStmtML =
  choice
    [ try $ do
        l <- keywordML "top"
        e <- exprML
        return (SSTop l e)
    , try $ do
        l <- keywordML "let"
        pat <- patternP
        _ <- symbolML "="
        v <- exprML
        return (SSLet l pat v)
    , try $ do
        (lv, l) <- lvalueML
        _ <- symbolML "="
        v <- exprML
        return (SSAssign l lv v)
    , do
        e <- exprML
        return (SSExpression (locOfE e) e)
    ]

blockBraced :: Parser Block
blockBraced = do
  pos <- getPosition
  _ <- symbol "{"
  _ <- newline1
  ss <- many (stmt <* optional newline1)
  _ <- symbol "}"
  return (Block (toLoc pos) ss)

decorator :: Parser Decorator
decorator = do
  pos <- getPosition
  _ <- symbol "@"
  (_, n) <- ident
  args <-
    option
      []
      ( try (between (symbol "(") (symbol ")") (expr `sepBy` symbol ",")) )
  return (Decorator (toLoc pos) n args)

defDecl :: Parser Decl
defDecl = do
  decs <- many decorator
  scnWithComments
  l <- keyword "def"
  (_, n) <- ident
  ps <- between (symbol "(") (symbol ")") (param `sepBy` symbol ",")
  rt <- optionMaybe (symbol ":" >> typeAnn)
  b <- blockBraced
  return (DFunc l decs n ps rt b)

objectTypeDecl :: Parser Decl
objectTypeDecl = do
  _ <- keyword "object"
  _ <- keyword "type"
  (l, n) <- ident
  _ <- symbol "{"
  scnWithComments
  mems <- many (objMember <* optional newline1)
  _ <- symbol "}"
  return (DObjectType l n mems)

objMember :: Parser ObjMember
objMember =
  try
    ( do
        (l, n) <- ident
        _ <- symbol ":"
        t <- typeAnn
        dv <- optionMaybe (symbol "=" >> expr)
        return (OMField l n t dv)
    )
    <|> (do d <- defDecl; return (OMFunc d))

letDecl :: Parser Decl
letDecl = do
  l <- keyword "let"
  pat <- patternP
  _ <- symbol "="
  v <- exprML
  return (DLet l pat v)

decl :: Parser Decl
decl = try defDecl <|> try objectTypeDecl <|> letDecl

packageLine :: Parser Name
packageLine = do
  _ <- keyword "package"
  (_, n) <- ident
  optional newline1
  return n

importLine :: Parser Name
importLine = do
  _ <- keyword "import"
  (_, n) <- identPath
  optional newline1
  return n
  where
    identPath = do
      (l, a) <- ident
      as <- many (char '.' >> fmap snd ident)
      sc
      return (l, intercalate "." (a : as))

topineur :: Parser Topineur
topineur = do
  scnWithComments
  pkg <- packageLine <?> "package declaration"
  scnWithComments
  imps <- many (try importLine)
  scnWithComments
  ds <- many (decl <* optional newline1)
  eof
  return (Topineur pkg imps ds)

parseTopineurSource :: String -> Either CompileError Topineur
parseTopineurSource src =
  case runParser topineur initState "source.topineur" src of
    Left pe ->
      let msg = show pe
          loc =
            case findFirstPos pe of
              Just p -> toLoc p
              Nothing -> SourcePos 0 0
       in Left (ParseError msg loc)
    Right ast -> Right ast


findFirstPos :: ParseError -> Maybe P.SourcePos
findFirstPos e =
  case show e of
    _ -> Nothing
