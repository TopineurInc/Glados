{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- TopineurParser - Parser for Topineur language
-}

module TopineurParser
  ( parseTopineur
  , parseTopineurFile
  , parseModule
  ) where

import qualified AST
import TopineurLexer (Token(..), TokenType(..), lexTopineur)
import Control.Monad (void)
import Text.Parsec hiding (token)
import qualified Text.Parsec as P
import Text.Parsec.Pos (newPos)
import qualified Data.Map as Map

-- Parser type using token stream
type Parser = Parsec [Token] ()

-- Main parsing function
parseTopineur :: String -> Either ParseError [AST.Expr]
parseTopineur input = do
  tokens <- lexTopineur input
  P.parse parseModule "<input>" tokens

parseTopineurFile :: FilePath -> String -> Either ParseError [AST.Expr]
parseTopineurFile filename input = do
  tokens <- lexTopineur input
  P.parse parseModule filename tokens

-- Module parser (top-level declarations)
parseModule :: Parser [AST.Expr]
parseModule = many parseDeclaration <* eof

parseDeclaration :: Parser AST.Expr
parseDeclaration = choice
  [ try parseObjectDef
  , try parseTraitDef
  , try parseTraitImpl
  , try parseFunctionDef
  , parseExpr
  ]

-- Token consumption helpers
token :: TokenType -> Parser Token
token expected = tokenPrim showTok updatePos testToken
  where
    showTok (Token tt _ lex) = lex
    testToken tok = if tokType tok == expected then Just tok else Nothing
    updatePos pos _ (Token _ tokPos _:_) = newPos (sourceName pos) (AST.spLine tokPos) (AST.spCol tokPos)
    updatePos pos _ [] = pos

anyToken :: Parser Token
anyToken = tokenPrim showTok updatePos Just
  where
    showTok (Token tt _ lex) = lex
    updatePos pos _ (Token _ tokPos _:_) = newPos (sourceName pos) (AST.spLine tokPos) (AST.spCol tokPos)
    updatePos pos _ [] = pos

satisfyToken :: (TokenType -> Bool) -> Parser Token
satisfyToken test = tokenPrim showTok updatePos testToken
  where
    showTok (Token tt _ lex) = lex
    testToken tok = if test (tokType tok) then Just tok else Nothing
    updatePos pos _ (Token _ tokPos _:_) = newPos (sourceName pos) (AST.spLine tokPos) (AST.spCol tokPos)
    updatePos pos _ [] = pos

-- Identifier parsers
ident :: Parser String
ident = do
  tok <- satisfyToken isIdent
  case tokType tok of
    TokIdent name -> return name
    _ -> fail "Expected identifier"
  where
    isIdent (TokIdent _) = True
    isIdent _ = False

typeIdent :: Parser String
typeIdent = do
  tok <- satisfyToken isTypeIdent
  case tokType tok of
    TokTypeIdent name -> return name
    _ -> fail "Expected type identifier"
  where
    isTypeIdent (TokTypeIdent _) = True
    isTypeIdent _ = False

-- Object definition parser
parseObjectDef :: Parser AST.Expr
parseObjectDef = do
  token TokObject
  token TokType
  name <- typeIdent
  typeParams <- option [] parseTypeParams
  token TokLBrace
  fields <- many parseFieldDef
  methods <- many parseMethodDef
  token TokRBrace
  return $ AST.EObjectDef $ AST.ObjectDef
    { AST.objName = name
    , AST.objTypeParams = typeParams
    , AST.objFields = fields
    , AST.objMethods = methods
    }

parseTypeParams :: Parser [String]
parseTypeParams = do
  token TokLBracket
  params <- sepBy typeIdent (token TokComma)
  token TokRBracket
  return params

parseFieldDef :: Parser (String, AST.Type)
parseFieldDef = do
  name <- ident
  token TokColon
  typ <- parseType
  return (name, typ)

parseMethodDef :: Parser AST.MethodDef
parseMethodDef = do
  token TokDef
  name <- ident
  -- Accept either () as TokUnit or (params) as TokLParen ... TokRParen
  params <- choice
    [ try $ token TokUnit >> return []
    , do token TokLParen
         ps <- sepBy parseParam (token TokComma)
         token TokRParen
         return ps
    ]
  token TokColon
  effects <- option (AST.EffectRow []) parseEffectRow
  retType <- parseType
  token TokAssign
  body <- parseExpr
  return $ AST.MethodDef
    { AST.methName = name
    , AST.methParams = params
    , AST.methReturnType = retType
    , AST.methEffects = effects
    , AST.methBody = body
    }

parseParam :: Parser (String, AST.Type)
parseParam = do
  name <- ident
  token TokColon
  typ <- parseType
  return (name, typ)

-- Trait definition parser
parseTraitDef :: Parser AST.Expr
parseTraitDef = do
  token TokTrait
  name <- typeIdent
  typeParams <- option [] parseTypeParams
  bounds <- option [] parseWhereClauses
  token TokLBrace
  methods <- many parseMethodSig
  token TokRBrace
  return $ AST.ETraitDef $ AST.TraitDef
    { AST.traitName = name
    , AST.traitTypeParams = typeParams
    , AST.traitBounds = bounds
    , AST.traitMethods = methods
    }

parseMethodSig :: Parser AST.MethodSig
parseMethodSig = do
  token TokDef
  name <- ident
  -- Accept either () as TokUnit or (params) as TokLParen ... TokRParen
  params <- choice
    [ try $ token TokUnit >> return []
    , do token TokLParen
         ps <- sepBy parseParam (token TokComma)
         token TokRParen
         return ps
    ]
  token TokColon
  effects <- option (AST.EffectRow []) parseEffectRow
  retType <- parseType
  return $ AST.MethodSig
    { AST.sigName = name
    , AST.sigParams = params
    , AST.sigReturnType = retType
    , AST.sigEffects = effects
    }

parseWhereClauses :: Parser [(String, String)]
parseWhereClauses = do
  token TokWhere
  sepBy parseTraitBound (token TokComma)

parseTraitBound :: Parser (String, String)
parseTraitBound = do
  typeVar <- typeIdent
  token TokColon
  traitName <- typeIdent
  return (typeVar, traitName)

-- Trait implementation parser
parseTraitImpl :: Parser AST.Expr
parseTraitImpl = do
  token TokImpl
  traitName <- typeIdent
  token TokFor
  forType <- parseType
  whereClauses <- option [] parseWhereClauses
  token TokLBrace
  methods <- many parseMethodDef
  token TokRBrace
  return $ AST.ETraitImpl $ AST.TraitImpl
    { AST.implTrait = traitName
    , AST.implForType = forType
    , AST.implWhereClauses = whereClauses
    , AST.implMethods = methods
    }

-- Function definition parser
parseFunctionDef :: Parser AST.Expr
parseFunctionDef = do
  token TokDef
  name <- ident
  typeParams <- option [] parseTypeParams
  -- Accept either () as TokUnit or (params) as TokLParen ... TokRParen
  params <- choice
    [ try $ token TokUnit >> return []
    , do token TokLParen
         ps <- sepBy parseParam (token TokComma)
         token TokRParen
         return ps
    ]
  token TokColon
  effects <- option (AST.EffectRow []) parseEffectRow
  retType <- parseType
  whereClauses <- option [] parseWhereClauses
  token TokAssign
  body <- parseExpr
  -- Convert to lambda and define (always wrap in lambda, even for no params)
  let lambda = if null params
               then AST.ELambda [] body
               else foldr (\(pName, _) e -> AST.ELambda [pName] e) body params
  return $ AST.EDefine name lambda

-- Type parser
parseType :: Parser AST.Type
parseType = choice
  [ try parseFunctionType
  , parseSimpleType
  ]

parseSimpleType :: Parser AST.Type
parseSimpleType = choice
  [ try $ do
      tok <- satisfyToken isTypeIdent
      case tokType tok of
        TokTypeIdent "Int" -> return AST.TInt
        TokTypeIdent "Float" -> return AST.TFloat
        TokTypeIdent "Bool" -> return AST.TBool
        TokTypeIdent "String" -> return AST.TString
        TokTypeIdent "Unit" -> return AST.TUnit
        TokTypeIdent name -> return $ AST.TObject name
        _ -> fail "Expected type"
  , try parseLinearType
  , try parseListType
  , try parseTupleType
  ]
  where
    isTypeIdent (TokTypeIdent _) = True
    isTypeIdent _ = False

parseLinearType :: Parser AST.Type
parseLinearType = do
  token TokLinear
  typ <- parseSimpleType
  return $ AST.TLinear typ

parseListType :: Parser AST.Type
parseListType = do
  token TokLBracket
  typ <- parseType
  token TokRBracket
  return $ AST.TList typ

parseTupleType :: Parser AST.Type
parseTupleType = do
  token TokLParen
  types <- sepBy parseType (token TokComma)
  token TokRParen
  if length types == 1
    then return $ head types
    else return $ AST.TTuple types

parseFunctionType :: Parser AST.Type
parseFunctionType = do
  token TokLParen
  paramTypes <- sepBy parseType (token TokComma)
  token TokRParen
  effects <- option (AST.EffectRow []) parseEffectRow
  token TokArrow
  retType <- parseType
  return $ AST.TFunc paramTypes effects retType

-- Effect row parser
parseEffectRow :: Parser AST.EffectRow
parseEffectRow = do
  token TokEffectStart
  effects <- sepBy parseEffect (token TokComma)
  token TokRBrace
  return $ AST.EffectRow effects

parseEffect :: Parser AST.Effect
parseEffect = do
  tok <- satisfyToken isTypeIdent
  case tokType tok of
    TokTypeIdent "IO" -> return AST.EffIO
    TokTypeIdent "State" -> return AST.EffState
    TokTypeIdent "Network" -> return AST.EffNetwork
    TokTypeIdent "Exception" -> return AST.EffException
    TokTypeIdent "Async" -> return AST.EffAsync
    TokTypeIdent name -> return $ AST.EffCustom name
    _ -> fail "Expected effect name"
  where
    isTypeIdent (TokTypeIdent _) = True
    isTypeIdent _ = False

-- Expression parser with precedence climbing
parseExpr :: Parser AST.Expr
parseExpr = choice
  [ try parseLetExpr
  , try parseIfExpr
  , try parseMatchExpr
  , try parseBlockExpr
  , parseInfixExpr
  ]

parseLetExpr :: Parser AST.Expr
parseLetExpr = do
  token TokLet
  isLinear <- option False (token TokLinear >> return True)
  name <- ident
  maybeType <- optionMaybe $ do
    token TokColon
    parseType
  token TokAssign
  value <- parseExpr
  token TokIn
  body <- parseExpr
  if isLinear
    then return $ AST.ELinearBind name value body
    else return $ AST.ELet name maybeType value body

parseIfExpr :: Parser AST.Expr
parseIfExpr = do
  token TokIf
  cond <- parseExpr
  token TokThen
  thenExpr <- parseExpr
  token TokElse
  elseExpr <- parseExpr
  return $ AST.EIf cond thenExpr elseExpr

parseMatchExpr :: Parser AST.Expr
parseMatchExpr = do
  token TokMatch
  expr <- parseExpr
  token TokLBrace
  cases <- many parseMatchCase
  token TokRBrace
  return $ AST.EMatch expr cases

parseMatchCase :: Parser (AST.Pattern, AST.Expr)
parseMatchCase = do
  token TokCase
  pattern <- parsePattern
  token TokFatArrow
  expr <- parseExpr
  return (pattern, expr)

parsePattern :: Parser AST.Pattern
parsePattern = choice
  [ try parseConstructorPattern
  , try parseLiteralPattern
  , try parseTuplePattern
  , try parseVarPattern
  , parseWildcardPattern
  ]

parseLiteralPattern :: Parser AST.Pattern
parseLiteralPattern = do
  tok <- satisfyToken isLiteral
  case tokType tok of
    TokIntLit n -> return $ AST.PLit (AST.CInt n)
    TokFloatLit f -> return $ AST.PLit (AST.CFloat f)
    TokBoolLit b -> return $ AST.PLit (AST.CBool b)
    TokStringLit s -> return $ AST.PLit (AST.CString s)
    _ -> fail "Expected literal"
  where
    isLiteral (TokIntLit _) = True
    isLiteral (TokFloatLit _) = True
    isLiteral (TokBoolLit _) = True
    isLiteral (TokStringLit _) = True
    isLiteral _ = False

parseVarPattern :: Parser AST.Pattern
parseVarPattern = AST.PVar <$> ident

parseConstructorPattern :: Parser AST.Pattern
parseConstructorPattern = do
  name <- typeIdent
  patterns <- option [] $ do
    token TokLParen
    pats <- sepBy parsePattern (token TokComma)
    token TokRParen
    return pats
  return $ AST.PConstructor name patterns

parseTuplePattern :: Parser AST.Pattern
parseTuplePattern = do
  token TokLParen
  patterns <- sepBy parsePattern (token TokComma)
  token TokRParen
  if length patterns == 1
    then return $ head patterns
    else return $ AST.PTuple patterns

parseWildcardPattern :: Parser AST.Pattern
parseWildcardPattern = do
  tok <- satisfyToken isWildcard
  return AST.PWildcard
  where
    isWildcard (TokIdent "_") = True
    isWildcard _ = False

parseBlockExpr :: Parser AST.Expr
parseBlockExpr = do
  token TokLBrace
  exprs <- sepBy parseExpr (token TokSemi)
  token TokRBrace
  case exprs of
    [] -> return $ AST.EInt 0  -- Empty block
    [e] -> return e
    _ -> return $ AST.EBlock exprs

-- Infix expression parser with precedence climbing
parseInfixExpr :: Parser AST.Expr
parseInfixExpr = parseLogicalOr

parseLogicalOr :: Parser AST.Expr
parseLogicalOr = chainl1 parseLogicalAnd parseOrOp
  where
    parseOrOp = token TokOr >> return (\a b -> AST.EApp (AST.EVar "or") [a, b])

parseLogicalAnd :: Parser AST.Expr
parseLogicalAnd = chainl1 parseEquality parseAndOp
  where
    parseAndOp = token TokAnd >> return (\a b -> AST.EApp (AST.EVar "and") [a, b])

parseEquality :: Parser AST.Expr
parseEquality = chainl1 parseComparison parseEqOp
  where
    parseEqOp = choice
      [ token TokEq >> return (\a b -> AST.EApp (AST.EVar "eq?") [a, b])
      , token TokNeq >> return (\a b -> AST.EApp (AST.EVar "not") [AST.EApp (AST.EVar "eq?") [a, b]])
      ]

parseComparison :: Parser AST.Expr
parseComparison = chainl1 parseAdditive parseCmpOp
  where
    parseCmpOp = choice
      [ token TokLt >> return (\a b -> AST.EApp (AST.EVar "<") [a, b])
      , token TokGt >> return (\a b -> AST.EApp (AST.EVar ">") [a, b])
      , token TokLte >> return (\a b -> AST.EApp (AST.EVar "<=") [a, b])
      , token TokGte >> return (\a b -> AST.EApp (AST.EVar ">=") [a, b])
      ]

parseAdditive :: Parser AST.Expr
parseAdditive = chainl1 parseMultiplicative parseAddOp
  where
    parseAddOp = choice
      [ token TokPlus >> return (\a b -> AST.EApp (AST.EVar "+") [a, b])
      , token TokMinus >> return (\a b -> AST.EApp (AST.EVar "-") [a, b])
      , token TokConcat >> return (\a b -> AST.EApp (AST.EVar "string-append") [a, b])
      ]

parseMultiplicative :: Parser AST.Expr
parseMultiplicative = chainl1 parsePrefix parseMulOp
  where
    parseMulOp = choice
      [ token TokStar >> return (\a b -> AST.EApp (AST.EVar "*") [a, b])
      , token TokSlash >> return (\a b -> AST.EApp (AST.EVar "div") [a, b])
      , token TokPercent >> return (\a b -> AST.EApp (AST.EVar "mod") [a, b])
      ]

parsePrefix :: Parser AST.Expr
parsePrefix = choice
  [ try $ do
      token TokNot
      expr <- parsePrefix
      return $ AST.EApp (AST.EVar "not") [expr]
  , try $ do
      token TokMinus
      expr <- parsePrefix
      return $ AST.EApp (AST.EVar "-") [AST.EInt 0, expr]
  , parsePostfix
  ]

parsePostfix :: Parser AST.Expr
parsePostfix = do
  base <- parsePrimary
  suffixes <- many parseSuffix
  return $ foldl applySuffix base suffixes

data Suffix
  = SMethodCall String [AST.Expr]
  | SFieldAccess String
  | SFunctionCall [AST.Expr]
  | STypeAnnotation AST.Type

parseSuffix :: Parser Suffix
parseSuffix = choice
  [ try $ do
      token TokDot
      name <- ident
      token TokLParen
      args <- sepBy parseExpr (token TokComma)
      token TokRParen
      return $ SMethodCall name args
  , try $ do
      token TokDot
      name <- ident
      return $ SFieldAccess name
  , try $ do
      token TokLParen
      args <- sepBy parseExpr (token TokComma)
      token TokRParen
      return $ SFunctionCall args
  , try $ do
      token TokColon
      typ <- parseType
      return $ STypeAnnotation typ
  ]

applySuffix :: AST.Expr -> Suffix -> AST.Expr
applySuffix base (SMethodCall name args) = AST.EMethodCall base name args
applySuffix base (SFieldAccess name) = AST.EFieldAccess base name
applySuffix base (SFunctionCall args) = AST.EApp base args
applySuffix base (STypeAnnotation typ) = AST.ETyped base typ

-- Primary expressions
parsePrimary :: Parser AST.Expr
parsePrimary = choice
  [ try parseLambda
  , try parseObjectLiteral
  , try parseListLiteral
  , try parseTupleLiteral
  , try parseLiteral
  , try parseVariable
  , parseParenExpr
  ]

parseLambda :: Parser AST.Expr
parseLambda = do
  token TokFn
  -- Accept either () as TokUnit or (params) as TokLParen ... TokRParen
  params <- choice
    [ try $ token TokUnit >> return []
    , do token TokLParen
         ps <- sepBy parseParam (token TokComma)
         token TokRParen
         return ps
    ]
  optionMaybe $ do
    token TokColon
    parseEffectRow
    parseType
  token TokFatArrow
  body <- parseExpr
  let paramNames = map fst params
  return $ if null paramNames
           then body
           else foldr (\p e -> AST.ELambda [p] e) body paramNames

parseObjectLiteral :: Parser AST.Expr
parseObjectLiteral = do
  name <- typeIdent
  token TokLBrace
  fields <- sepBy parseFieldInit (token TokComma)
  token TokRBrace
  return $ AST.EObjectLit name fields

parseFieldInit :: Parser (String, AST.Expr)
parseFieldInit = do
  name <- ident
  token TokAssign
  expr <- parseExpr
  return (name, expr)

parseListLiteral :: Parser AST.Expr
parseListLiteral = do
  token TokLBracket
  exprs <- sepBy parseExpr (token TokComma)
  token TokRBracket
  return $ AST.EList exprs

parseTupleLiteral :: Parser AST.Expr
parseTupleLiteral = do
  token TokLParen
  exprs <- sepBy parseExpr (token TokComma)
  token TokRParen
  case exprs of
    [] -> return $ AST.EInt 0  -- Unit
    [e] -> return e
    _ -> return $ AST.EList exprs  -- Use list for tuple temporarily

parseLiteral :: Parser AST.Expr
parseLiteral = do
  tok <- satisfyToken isLiteral
  case tokType tok of
    TokIntLit n -> return $ AST.EInt n
    TokFloatLit f -> return $ AST.EFloat f
    TokBoolLit b -> return $ AST.EBool b
    TokStringLit s -> return $ AST.EString s
    TokUnit -> return $ AST.EInt 0  -- Unit as 0 temporarily
    _ -> fail "Expected literal"
  where
    isLiteral (TokIntLit _) = True
    isLiteral (TokFloatLit _) = True
    isLiteral (TokBoolLit _) = True
    isLiteral (TokStringLit _) = True
    isLiteral TokUnit = True
    isLiteral _ = False

parseVariable :: Parser AST.Expr
parseVariable = AST.EVar <$> ident

parseParenExpr :: Parser AST.Expr
parseParenExpr = do
  token TokLParen
  expr <- parseExpr
  token TokRParen
  return expr
