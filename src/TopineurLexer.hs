{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- TopineurLexer - Lexical analysis for Topineur language
-}

module TopineurLexer
  ( Token(..)
  , TokenType(..)
  , lexTopineur
  , tokenize
  ) where

import qualified AST
import Data.Char (isAlpha, isDigit, isAlphaNum, isUpper, isSpace)
import Control.Monad (void)
import Text.Parsec hiding (SourcePos)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Pos as Pos

-- Token type with position information
data Token = Token
  { tokType :: TokenType
  , tokPos :: AST.SourcePos
  , tokLexeme :: String
  } deriving (Eq, Show)

-- All token types in Topineur
data TokenType
  -- Keywords
  = TokObject
  | TokType
  | TokTrait
  | TokImpl
  | TokFor
  | TokDef
  | TokLet
  | TokMut
  | TokIf
  | TokThen
  | TokElse
  | TokMatch
  | TokCase
  | TokLazy
  | TokActor
  | TokReceive
  | TokFn
  | TokIn
  | TokWhere
  | TokSpawn

  -- Literals
  | TokIntLit Integer
  | TokFloatLit Double
  | TokBoolLit Bool
  | TokStringLit String
  | TokUnit                    -- ()

  -- Identifiers
  | TokIdent String            -- lowercase identifier
  | TokTypeIdent String        -- uppercase identifier (types)

  -- Operators
  | TokPlus                    -- +
  | TokMinus                   -- -
  | TokStar                    -- *
  | TokSlash                   -- /
  | TokPercent                 -- %
  | TokEq                      -- ==
  | TokNeq                     -- !=
  | TokLt                      -- <
  | TokGt                      -- >
  | TokLte                     -- <=
  | TokGte                     -- >=
  | TokAnd                     -- &&
  | TokOr                      -- ||
  | TokNot                     -- !
  | TokConcat                  -- ++
  | TokAssign                  -- =
  | TokColon                   -- :
  | TokArrow                   -- ->
  | TokFatArrow                -- =>
  | TokDot                     -- .
  | TokComma                   -- ,
  | TokSemi                    -- ;
  | TokPipe                    -- |

  -- Delimiters
  | TokLParen                  -- (
  | TokRParen                  -- )
  | TokLBrace                  -- {
  | TokRBrace                  -- }
  | TokLBracket                -- [
  | TokRBracket                -- ]

  -- Special
  | TokEffectStart             -- !{
  | TokLinear                  -- !lin

  -- End of file
  | TokEOF
  deriving (Eq, Show)

-- Lexer definition using Parsec
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style = Lang.emptyDef
      { Tok.commentLine     = "//"
      , Tok.commentStart    = "/*"
      , Tok.commentEnd      = "*/"
      , Tok.nestedComments  = True
      , Tok.identStart      = letter <|> char '_'
      , Tok.identLetter     = alphaNum <|> char '_'
      , Tok.reservedNames   = keywords
      , Tok.reservedOpNames = operators
      , Tok.caseSensitive   = True
      }

keywords :: [String]
keywords =
  [ "object", "type", "trait", "impl", "for"
  , "def", "let", "mut", "if", "then", "else"
  , "match", "case", "lazy", "actor", "receive"
  , "fn", "in", "where", "spawn"
  ]

operators :: [String]
operators =
  [ "+", "-", "*", "/", "%"
  , "==", "!=", "<", ">", "<=", ">="
  , "&&", "||", "!", "++"
  , "=", ":", "->", "=>"
  , ".", ",", ";", "|"
  ]

-- Parsec token parsers
whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

natural :: Parser Integer
natural = Tok.natural lexer

float :: Parser Double
float = Tok.float lexer

stringLit :: Parser String
stringLit = Tok.stringLiteral lexer

-- Convert Parsec SourcePos to our SourcePos
convertPos :: Pos.SourcePos -> AST.SourcePos
convertPos pos = AST.SourcePos
  { AST.spLine = Pos.sourceLine pos
  , AST.spCol = Pos.sourceColumn pos
  }

-- Token parsers
tokenParser :: Parser Token
tokenParser = do
  pos <- getPosition
  let sourcePos = convertPos pos
  choice
    [ try $ tokenKeyword sourcePos
    , try $ tokenOperator sourcePos
    , try $ tokenLiteral sourcePos
    , try $ tokenIdent sourcePos
    , tokenDelimiter sourcePos
    ]

tokenKeyword :: AST.SourcePos -> Parser Token
tokenKeyword pos = choice
  [ reserved "object" >> return (Token TokObject pos "object")
  , reserved "type" >> return (Token TokType pos "type")
  , reserved "trait" >> return (Token TokTrait pos "trait")
  , reserved "impl" >> return (Token TokImpl pos "impl")
  , reserved "for" >> return (Token TokFor pos "for")
  , reserved "def" >> return (Token TokDef pos "def")
  , reserved "let" >> return (Token TokLet pos "let")
  , reserved "mut" >> return (Token TokMut pos "mut")
  , reserved "if" >> return (Token TokIf pos "if")
  , reserved "then" >> return (Token TokThen pos "then")
  , reserved "else" >> return (Token TokElse pos "else")
  , reserved "match" >> return (Token TokMatch pos "match")
  , reserved "case" >> return (Token TokCase pos "case")
  , reserved "lazy" >> return (Token TokLazy pos "lazy")
  , reserved "actor" >> return (Token TokActor pos "actor")
  , reserved "receive" >> return (Token TokReceive pos "receive")
  , reserved "fn" >> return (Token TokFn pos "fn")
  , reserved "in" >> return (Token TokIn pos "in")
  , reserved "where" >> return (Token TokWhere pos "where")
  , reserved "spawn" >> return (Token TokSpawn pos "spawn")
  ]

tokenOperator :: AST.SourcePos -> Parser Token
tokenOperator pos = choice
  [ try $ string "!{" >> return (Token TokEffectStart pos "!{")
  , try $ string "!lin" >> return (Token TokLinear pos "!lin")
  , try $ reservedOp "==" >> return (Token TokEq pos "==")
  , try $ reservedOp "!=" >> return (Token TokNeq pos "!=")
  , try $ reservedOp "<=" >> return (Token TokLte pos "<=")
  , try $ reservedOp ">=" >> return (Token TokGte pos ">=")
  , try $ reservedOp "&&" >> return (Token TokAnd pos "&&")
  , try $ reservedOp "||" >> return (Token TokOr pos "||")
  , try $ reservedOp "++" >> return (Token TokConcat pos "++")
  , try $ reservedOp "->" >> return (Token TokArrow pos "->")
  , try $ reservedOp "=>" >> return (Token TokFatArrow pos "=>")
  , reservedOp "+" >> return (Token TokPlus pos "+")
  , reservedOp "-" >> return (Token TokMinus pos "-")
  , reservedOp "*" >> return (Token TokStar pos "*")
  , reservedOp "/" >> return (Token TokSlash pos "/")
  , reservedOp "%" >> return (Token TokPercent pos "%")
  , reservedOp "<" >> return (Token TokLt pos "<")
  , reservedOp ">" >> return (Token TokGt pos ">")
  , reservedOp "!" >> return (Token TokNot pos "!")
  , reservedOp "=" >> return (Token TokAssign pos "=")
  , reservedOp ":" >> return (Token TokColon pos ":")
  , reservedOp "." >> return (Token TokDot pos ".")
  , reservedOp "," >> return (Token TokComma pos ",")
  , reservedOp ";" >> return (Token TokSemi pos ";")
  , reservedOp "|" >> return (Token TokPipe pos "|")
  ]

tokenLiteral :: AST.SourcePos -> Parser Token
tokenLiteral pos = choice
  [ try $ do
      f <- float
      return (Token (TokFloatLit f) pos (show f))
  , try $ do
      n <- natural
      return (Token (TokIntLit n) pos (show n))
  , try $ string "#t" >> return (Token (TokBoolLit True) pos "#t")
  , try $ string "#f" >> return (Token (TokBoolLit False) pos "#f")
  , try $ do
      s <- stringLit
      return (Token (TokStringLit s) pos ("\"" ++ s ++ "\""))
  , try $ string "()" >> return (Token TokUnit pos "()")
  ]

tokenIdent :: AST.SourcePos -> Parser Token
tokenIdent pos = do
  name <- identifier
  let tokType = if isUpper (head name)
                then TokTypeIdent name
                else TokIdent name
  return (Token tokType pos name)

tokenDelimiter :: AST.SourcePos -> Parser Token
tokenDelimiter pos = choice
  [ char '(' >> return (Token TokLParen pos "(")
  , char ')' >> return (Token TokRParen pos ")")
  , char '{' >> return (Token TokLBrace pos "{")
  , char '}' >> return (Token TokRBrace pos "}")
  , char '[' >> return (Token TokLBracket pos "[")
  , char ']' >> return (Token TokRBracket pos "]")
  ]

-- Main tokenization function
tokenize :: Parser [Token]
tokenize = do
  whiteSpace
  tokens <- many (whiteSpace >> tokenParser)
  eof
  return tokens

lexTopineur :: String -> Either ParseError [Token]
lexTopineur input = parse tokenize "<input>" input
