{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Adder.Parser where

import Control.Monad.Identity (Identity)
import Data.Char (isDigit)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import Text.Parsec.String (Parser)

data SourceSpan = SS
  { start :: SourcePos,
    end :: SourcePos
  }
  deriving (Show)

data Prim1 = Add | Sub deriving (Show)

data Expr a
  = Num Integer a
  | Var String a
  | PrimOp Prim1 (Expr a) a
  | Let String (Expr a) (Expr a) a
  deriving (Show, Functor)

type ExprU = Expr SourceSpan

parseExpr = letExpr <|> parseadd1 parseExpr <|> parsesub1 parseExpr <|> var <|> parsenum

ss p = (\start e end -> (e, SS start end)) <$> getPosition <*> p <*> getPosition

var = (uncurry Var) <$> ss identifier

parseadd1 :: ParsecT String () Identity ExprU -> ParsecT String () Identity ExprU
parseadd1 p = uncurry (PrimOp Add) <$> ss (reserved "add1" *> parens p)

parsesub1 :: ParsecT String u Identity ExprU -> ParsecT String u Identity ExprU
parsesub1 p = uncurry (PrimOp Sub) <$> ss (reserved "sub1" *> parens p)

parsenum :: ParsecT String u Identity ExprU
parsenum = uncurry Num <$> ss integer

lethelper = (,) <$> (identifier <* reservedOp "=") <*> parseExpr

let2 = do
  start <- getPosition
  x <- identifier
  reservedOp "="
  e1 <- parseExpr
  e2 <-
    ( do
        reservedOp ";"
        let2
      )
      <|> reserved "in" *> parseExpr
  end <- getPosition
  return $ Let x e1 e2 (SS start end)

letExpr :: Parser (Expr SourceSpan)
letExpr = reserved "let" *> let2

parserTest :: IO (Either ParseError ExprU)
parserTest =
  parse parseExpr "adder.boran" <$> readFile "programs/adder.boran"

x = (fmap . fmap . fmap) (const ()) parserTest

-- >>> x
-- Right (Let "x" (Num 12 ()) (Let "y" (PrimOp Sub (PrimOp Add (PrimOp Add (Var "x" ()) ()) ()) ()) (PrimOp Sub (Var "x" ()) ()) ()) ())
--

parser xs = case parse (whiteSpace >> parseExpr) "" xs of
  Right r -> Right r
  (Left e) -> Left $ show e

languageDef :: GenLanguageDef String u Identity
languageDef =
  emptyDef
    { Token.commentStart = "/*",
      Token.commentEnd = "*/",
      Token.commentLine = "//",
      Token.identStart = letter,
      Token.identLetter = alphaNum,
      Token.reservedNames =
        [ "let",
          "in",
          "add1",
          "sub1"
        ],
      Token.reservedOpNames =
        [ "+",
          "-",
          "="
        ]
    }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer

reserved = Token.reserved lexer

reservedOp = Token.reservedOp lexer

parens = Token.parens lexer

integer = Token.integer lexer

whiteSpace = Token.whiteSpace lexer
