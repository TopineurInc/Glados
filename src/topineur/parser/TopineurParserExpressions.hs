{-
-- EPITECH PROJECT, 2025
-- G-FUN-500-LYN-5-1-glados-1
-- File description:
-- src/topineur/parser/TopineurParserExpressions.hs
-}

module TopineurParserExpressions
  ( expr
  , exprML
  , term
  , termML
  , basicTerm
  , basicTermML
  ) where

import Text.Parsec
  ( getPosition
  , char
  , many
  , noneOf
  , many1
  , oneOf
  , notFollowedBy
  , try
  , choice
  , string
  , between
  , sepBy
  , optionMaybe
  , sepBy1
  , lookAhead
  , (<|>)
  )

import AST (Loc)
import TopineurParserTypes
import TopineurParserSymbols
import TopineurParserUtils
import TopineurParserPattern

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
  sign <- optionMaybe $ do
    _ <- char '-'
    _ <- lookAhead (oneOf "0123456789")
    return ()
  ds <- many1 (oneOf "0123456789")
  notFollowedBy (try $ char '.' >> oneOf "0123456789")
  let num = read ds
  return (toLoc pos, case sign of
    Nothing -> num
    Just _  -> -num)

floatLit :: Parser (Loc, Double)
floatLit = lexeme $ do
  pos <- getPosition
  sign <- optionMaybe $ do
    _ <- char '-'
    _ <- lookAhead (oneOf "0123456789")
    return ()
  a <- many1 (oneOf "0123456789")
  _ <- char '.'
  b <- many1 (oneOf "0123456789")
  let num = read (a ++ "." ++ b)
  return (toLoc pos, case sign of
    Nothing -> num
    Just _  -> -num)


expr :: Parser Expression
expr = makeUnOps
  where
    makeUnOps = do
      mOp <- optionMaybe $ try $ do
        l <- keyword "not"
        return l
      case mOp of
        Just l -> do
          e <- makeUnOps
          return (EUnOp l "not" e)
        Nothing -> makeBinOps

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
    , try $ do
        l <- keyword "true"
        return (EBool l True)
    , try $ do
        l <- keyword "false"
        return (EBool l False)
    , try $ do (l, s) <- stringLit; return (EString l s)
    , try tupleExpr
    , try arrayExpr
    , try objectLit
    , try anonFun
    , try parensExpr
    , varOrUpper
    ]

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
        mArgs <- optionMaybe callArgs
        let l = toLoc pos
        let res = case mArgs of
              Nothing -> EMember l e0 n
              Just args -> EMethodCall l e0 n args
        postfix res
    , try $ do
        args <- callArgs
        let l = locOfE e0 in postfix (ECall l e0 args)
    , try $ do
        pos <- getPosition
        idx <- between (symbol "[") (symbol "]") expr
        let l = toLoc pos
        postfix (EIndex l e0 idx)
    , return e0
    ]

callArgs :: Parser [Expression]
callArgs = between (symbol "(") (symbol ")") (argExpr `sepBy` symbol ",")
  where
    argExpr = do
      scML
      exprML

exprML :: Parser Expression
exprML = makeUnOpsML
  where
    makeUnOpsML = do
      mOp <- optionMaybe $ try $ do
        l <- keywordML "not"
        return l
      case mOp of
        Just l -> do
          e <- makeUnOpsML
          return (EUnOp l "not" e)
        Nothing -> makeBinOpsML

makeBinOpsML :: Parser Expression
makeBinOpsML = level1ML
  where
    level1ML = chainlML level2ML ["++"]
    level2ML = chainlML level3ML ["and", "or"]
    level3ML = chainlML level4ML ["=="]
    level4ML = chainlML level5ML ["<=", ">=", "<", ">"]
    level5ML = chainlML level6ML [".."]
    level6ML = chainlML level7ML ["+", "-"]
    level7ML = chainlML termML ["*", "/", "%"]

    chainlML :: Parser Expression -> [String] -> Parser Expression
    chainlML p ops
      | null ops = p
      | otherwise = do
          l <- p
          let parseRest :: Expression -> Parser Expression
              parseRest e = do
                mo <- optionMaybe (choice (map tryOpML ops))
                case mo of
                  Nothing -> return e
                  Just op -> do
                    r <- p
                    parseRest (EBinOp (locOfE e) op e r)
          parseRest l
      where
        tryOpML op = try $ do
          _ <- if op == "and" || op == "or" || op == "not"
            then keywordML op >> return ()
            else string op >> notFollowedBy (oneOf "+-*/<>=!")
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
    , try $ do
        l <- keywordML "true"
        return (EBool l True)
    , try $ do
        l <- keywordML "false"
        return (EBool l False)
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
  ELambda pos ps rt <$> exprML

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
        mArgs <- optionMaybe callArgsML
        let l = toLoc pos
        let res = case mArgs of
              Nothing -> EMember l e0 n
              Just args -> EMethodCall l e0 n args
        postfixML res
    , try $ do
        args <- callArgsML
        let l = locOfE e0 in postfixML (ECall l e0 args)
    , try $ do
        pos <- getPosition
        idx <- between (symbolML "[") (symbolML "]") exprML
        let l = toLoc pos
        postfixML (EIndex l e0 idx)
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
  ELambda pos ps rt <$> expr

makeBinOps :: Parser Expression
makeBinOps = level1
  where
    level1 = chainl level2 ["++"]
    level2 = chainl level3 ["and", "or"]
    level3 = chainl level4 ["=="]
    level4 = chainl level5 ["<=", ">=", "<", ">"]
    level5 = chainl level6 [".."]
    level6 = chainl level7 ["+", "-"]
    level7 = chainl term ["*", "/", "%"]

    chainl :: Parser Expression -> [String] -> Parser Expression
    chainl p ops
      | null ops = p
      | otherwise = do
          l <- p
          let parseRest :: Expression -> Parser Expression
              parseRest e = do
                mo <- optionMaybe (choice (map tryOp ops))
                case mo of
                  Nothing -> return e
                  Just op -> do
                    r <- p
                    parseRest (EBinOp (locOfE e) op e r)
          parseRest l
      where
        tryOp op = try $ do
          _ <- if op == "and" || op == "or" || op == "not"
            then keyword op >> return ()
            else string op >> notFollowedBy (oneOf "+-*/<>=!")
          sc
          return op
