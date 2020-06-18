{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Boa.Parser where

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

data BinOp = Add | Sub | Mul | Div deriving (Show)

data Expr a
  = Num Integer a
  | Var String a
  | PrimOp BinOp (Expr a) (Expr a) a
  | Let String (Expr a) (Expr a) a
  | If (Expr a) (Expr a) (Expr a) a
  deriving (Show, Functor)

type ExprU = Expr SourceSpan

instance Foldable Expr where
  foldMap f (Num i a) = f a
  foldMap f (Var i a) = f a
  foldMap f (PrimOp _ l r a) = foldMap f l <> foldMap f r <> f a
  foldMap f (Let _ l r a) = foldMap f l <> foldMap f r <> f a
  foldMap f (If c l r a) = foldMap f c <> foldMap f l <> foldMap f r <> f a

instance Traversable Expr where
  traverse f (Num i a) = Num i <$> f a
  traverse f (Var i a) = Var i <$> f a
  traverse f (PrimOp i l r a) = PrimOp i <$> traverse f l <*> traverse f r <*> f a
  traverse f (Let i l r a) = Let i <$> traverse f l <*> traverse f r <*> f a
  traverse f (If c l r a) = If <$> traverse f c <*> traverse f l <*> traverse f r <*> f a

parseExpr = letExpr <|> ifthenelse <|> parseArithmetic

ss p = (\start e end -> (e, SS start end)) <$> getPosition <*> p <*> getPosition

var = uncurry Var <$> ss identifier

num :: ParsecT String u Identity ExprU
num = uncurry Num <$> ss integer

parseArithmetic = mychainl1 term op
  where
    op = PrimOp Add <$ reservedOp "+" <|> PrimOp Sub <$ reservedOp "-"

term = mychainl1 factor op
  where
    op = PrimOp Mul <$ reservedOp "*" <|> PrimOp Add <$ reservedOp "/"

mychainl1 p op = do
  start <- getPosition
  r <- p
  process start r
  where
    process start a = rest <|> return a
      where
        rest = do
          f <- op
          b <- p
          end <- getPosition
          process start (f a b (SS start end))

factor = parens parseExpr <|> num <|> var

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

ifthenelse = do
  start <- getPosition
  cond <- reserved "if" *> parseExpr <* reservedOp ":"
  e1 <- parseExpr
  e2 <- reserved "else:" *> parseExpr
  end <- getPosition
  return (If cond e1 e2 (SS start end))

parserTest :: IO (Either ParseError ExprU)
parserTest =
  parse parseExpr "boa.boran" <$> readFile "programs/boa.boran"

-- >>> x
-- Right (Let "x" (PrimOp Mul (PrimOp Mul (Num 12 ()) (Num 2 ()) ()) (Let "a" (PrimOp Mul (Num 2 ()) (Num 2 ()) ()) (PrimOp Mul (If (Var "a" ()) (Var "a" ()) (Num 1 ()) ()) (Num 3 ()) ()) ()) ()) (PrimOp Add (PrimOp Add (Num 2 ()) (Var "x" ()) ()) (If (Num 2 ()) (Num 2 ()) (Num 1 ()) ()) ()) ())
--

x = (fmap . fmap . fmap) (const ()) parserTest

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
          "if",
          "else:"
        ],
      Token.reservedOpNames =
        [ "+",
          "-",
          "=",
          "*",
          "/",
          ","
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
