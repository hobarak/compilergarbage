{-# LANGUAGE FlexibleContexts #-}

module Cobra.Parser (runParser, printerT) where

import Cobra.Types
import Control.Monad.Identity (Identity)
import Text.Parsec hiding (runParser)
import Text.Parsec.Language
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token

data SourceSpan = SS
  { start :: SourcePos,
    end :: SourcePos
  }
  deriving (Show)

type ExprU = Expr SourceSpan

programParser :: ParsecT String () Identity (Program SourceSpan)
programParser = Program <$> many declParser <*> exprParser

declParser :: ParsecT String () Identity (Decl SourceSpan)
declParser = do
  start <- getPosition
  reserved "def"
  name <- identifier
  args <- parens (sepBy identifier (reservedOp ",")) <* reservedOp ":"
  body <- exprParser
  end <- getPosition
  return $ Decl name args body (SS start end)

exprParser :: ParsecT String () Identity ExprU
exprParser = letExpr <|> ifthenelse <|> parseOperators

runParser :: String -> Either String (Program SourceSpan)
runParser xs = case parse (whiteSpace >> programParser) "" xs of
  Right r -> Right r
  (Left e) -> Left $ show e

ss :: Monad m => ParsecT s u m a -> ParsecT s u m (a, SourceSpan)
ss p = (\start e end -> (e, SS start end)) <$> getPosition <*> p <*> getPosition

ss2 p = (\start e end -> e $ SS start end) <$> getPosition <*> p <*> getPosition

var :: ParsecT String u Identity ExprU
var = uncurry Var <$> ss identifier

num :: ParsecT String u Identity ExprU
num = uncurry Num <$> ss integer

bool :: ParsecT String u Identity ExprU
bool = uncurry Bool <$> ss (False <$ reserved "False" <|> True <$ reserved "True")

parseOperators = mychainl1 arit op
  where
    op = PrimOp Equal <$ reservedOp "==" <|> PrimOp Less <$ reservedOp "<" <|> PrimOp Greater <$ reservedOp ">"

arit = mychainl1 term op
  where
    op = PrimOp Add <$ reservedOp "+" <|> PrimOp Sub <$ reservedOp "-"

term = mychainl1 logic op
  where
    op = PrimOp Mul <$ reservedOp "*" <|> PrimOp Add <$ reservedOp "/"

logic = mychainl1 index op
  where
    op = PrimOp And <$ reservedOp "&&" <|> PrimOp Or <$ reservedOp "||"

index = do
  start <- getPosition
  r <- factor
  process start r
  where
    process start a = rest <|> return a
      where
        rest = do
          reservedOp "["
          b <- factor
          reservedOp "]"
          end <- getPosition
          process start (Index a b (SS start end))

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

factor = try tuple <|> parens exprParser <|> try app <|> num <|> var <|> bool

app = do
  start <- getPosition
  name <- identifier
  args <- parens $ sepBy exprParser (reservedOp ",")
  end <- getPosition
  return $ App name args (SS start end)

tuple = ss2 $ fmap Tuple $ parens $ (:) <$> exprParser <*> many1 (reservedOp "," *> exprParser)

lethelper = (,) <$> (identifier <* reservedOp "=") <*> exprParser

let2 = do
  start <- getPosition
  x <- identifier
  reservedOp "="
  e1 <- exprParser
  e2 <-
    ( do
        reservedOp ";"
        let2
      )
      <|> reserved "in" *> exprParser
  Let x e1 e2 . SS start <$> getPosition

letExpr :: Parser (Expr SourceSpan)
letExpr = reserved "let" *> let2

ifthenelse = do
  start <- getPosition
  cond <- reserved "if" *> exprParser <* reservedOp ":"
  e1 <- exprParser
  e2 <- reserved "else:" *> exprParser
  If cond e1 e2 . SS start <$> getPosition

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
          "else:",
          "True",
          "False"
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

printerT = do
  s <- readFile "programs/cobra.boran"
  case runParser s of
    (Right r) -> putStrLn (show (fmap (const ()) r))
    (Left e) -> print e

-- >>> printerT
-- Program {pFuns = [], pbody = Let "y" (Tuple [Num 111 (),Num 123 ()] ()) (Index (Var "y" ()) (Num 1 ()) ()) ()}
--
