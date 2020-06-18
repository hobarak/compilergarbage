{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Csep505.One where

import Control.Applicative
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Except (runExceptT)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader.Class
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Reader (runReaderT, withReader)
import Data.Typeable (Typeable)
import Debug.Trace (trace)
import System.Console.Haskeline
import Text.Parsec (chainl, chainl1, eof, many1, parse, try)
import Text.Parsec.Error (ParseError)
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+", "*", "-", "="]
    names = ["if", "then", "else", "let", "in"]
    style =
      emptyDef
        { Tok.commentLine = "#",
          Tok.reservedOpNames = ops,
          Tok.reservedNames = names
        }

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

type Name = String

data Op = Add | Sub | Mul deriving (Eq, Ord, Show)

data Expr a
  = Integer Integer a
  | Boolean Bool a
  | BinaryOp String (Expr a) (Expr a) a
  | If (Expr a) (Expr a) (Expr a) a
  | Let [(Name, Expr a)] (Expr a) a
  | Var Name a
  | Lam Name (Expr a) a
  | Call (Expr a) (Expr a) a
  | Seq (Expr a) (Expr a) a
  | Assign Name (Expr a) a
  | Define Name (Expr a) a
  deriving (Eq, Ord, Show)

binary :: String -> String -> Ex.Assoc -> Ex.Operator String () Identity (Expr ())
binary s f assoc = Ex.Infix (reservedOp s >> return (\a b -> BinaryOp s a b ())) assoc

table =
  [ [ binary "&&" "&&" Ex.AssocLeft,
      binary "||" "||" Ex.AssocLeft
    ],
    [ binary "==" "==" Ex.AssocNone,
      binary "<" "<" Ex.AssocNone,
      binary ">" ">" Ex.AssocNone
    ],
    [ binary "*" "*" Ex.AssocLeft
    ],
    [ binary "+" "+" Ex.AssocLeft,
      binary "-" "-" Ex.AssocLeft
    ]
  ]

int :: Parser (Expr ())
int = flip Integer () <$> integer

bool :: Parser (Expr ())
bool =
  flip Boolean () <$> (True <$ reserved "true")
    <|> flip Boolean () <$> (False <$ reserved "false")

expr = do
  xs <- semiSep term
  return $ foldr1 (\l r -> Seq l r ()) xs

term = def <|> ifthenelse <|> letin <|> lambda <|> try assign <|> try expr'

def = do
  reserved "def"
  i <- identifier
  reservedOp "="
  e <- term
  return $ Define i e ()

assign = do
  i <- identifier
  reservedOp "="
  t <- term
  return $ Assign i t ()

--expr' = arith >>= p
--  where
--    p x = op x <|> return x
--    op x = do
--      xs <- parens $ commaSep expr
--      p $ foldl (\z x -> Call z x ()) x xs
expr' = arith >>= p
  where
    p x = op x <|> return x
    op x = do
      z <- arith
      p $ Call x z ()

arith :: Parser (Expr ())
arith = Ex.buildExpressionParser table factor

lambda = do
  reservedOp "\\"
  cs <- many1 identifier
  reserved "->"
  e1 <- term
  return $ foldr (\a b -> Lam a b ()) e1 cs

ifthenelse :: Parser (Expr ())
ifthenelse = do
  reserved "if"
  c <- term
  reserved "then"
  e1 <- term
  reserved "else"
  e2 <- term
  return $ If c e1 e2 ()

letin :: Parser (Expr ())
letin = do
  reserved "let"
  ne <- commaSep $ do
    n <- identifier
    reservedOp "="
    e1 <- term
    return (n, e1)
  reserved "in"
  e2 <- expr
  return $ Let ne e2 ()

factor :: Parser (Expr ())
factor =
  try int <|> try bool <|> try var <|> parens expr

-- try lambda <|>    try call <|>
var = flip Var () <$> identifier

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError (Expr ())
parseExpr s = parse (contents expr) "<stdin>" s

testOne = do
  str <- readFile "./programs/Csep505/one.p"
  case parseExpr str of
    Left e -> print e
    Right r -> runInterp r >>= print

parseOne = do
  str <- readFile "./programs/Csep505/one.p"
  let t = parseExpr str
  return t

---print $ parseExpr str

-- >>> testOne
-- ("init",10)
-- ("a",12)
-- ("a",13)
-- thunk "a"
--

sameType (VBool _) (VBool _) = True
sameType (VInt _) (VInt _) = True
sameType _ _ = False

ifthen (VBool b) l r = if b then l else r

iften _ _ _ = throwM TypeError

data SemanticError = TypeError | OtherError | EnvError String deriving (Show, Typeable)

instance Exception SemanticError

data Value = VBool Bool | VInt Integer | forall a. VThunk Env Name (Expr a) | VPrim (Value -> Either SemanticError (Value))

instance Eq Value where
  (VBool a) == (VBool b) = a == b
  (VInt a) == (VInt b) = a == b
  _ == _ = False

instance Ord Value where
  (VBool a) <= (VBool b) = a <= b
  (VInt a) <= (VInt b) = a <= b
  _ <= _ = error "cannot compare"

instance Show Value where
  show (VBool b) = show b
  show (VInt i) = show i
  show (VThunk e n expr) = "thunk " ++ show n
  show (VPrim f) = "prim"

type Env = [(String, Int)]

type Store = (Int, [(Int, Value)])

type M m = (Monad m, MonadState Store m, MonadReader Env m, MonadThrow m)

getIndex s = do
  v <- asks (lookup s)
  case v of
    Nothing -> throwM OtherError
    Just v' -> return v'

getStore i = do
  v <- gets (lookup i . snd)
  case v of
    Nothing -> throwM OtherError
    Just v' -> return v'

getVar :: (M m) => String -> m Value
getVar s = do
  v <- getIndex s
  getStore v

setVar :: (M m) => String -> Value -> m ()
setVar n e = do
  i <- getIndex n
  (a, s) <- get
  put (a, change i e s)
  where
    change i n [] = []
    change i n ((i', n') : xs) = if i == i' then (i, n) : xs else (i', n') : change i n xs

addVar (n, e') = do
  (i, s) <- get
  put (i + 1, (i, e') : s)
  --trace (show ((i + 1, (i, e') : s)))
  (return ((n, i)))

eval1 (VPrim f) l = f l
eval1 _ _ = Left OtherError

eval2 (VPrim f) l r = either throwM pure res
  where
    res = do
      f' <- f l
      eval1 f' r
eval2 _ _ _ = throwM OtherError

interp :: (M m) => Expr a -> m Value
interp (Integer a _) = pure $ VInt a
interp (Boolean b _) = pure $ VBool b
interp (BinaryOp o l r _) = do
  l' <- interp l
  r' <- interp r
  op' <- getVar o
  case op' of
    v@(VPrim f) -> eval2 v l' r'
    _ -> throwM OtherError
interp (If c e1 e2 _) = do
  c' <- interp c
  ifthen c' (interp e1) (interp e2)
interp (Let [] e2 _) = interp e2
interp (Let ((v, e) : xs) e2 a) = do
  e' <- interp e
  x' <- addVar (v, e')
  local (x' :) $ interp (Let xs e2 a)
interp (Var v _) = getVar v
interp l@(Lam v e _) = do
  env <- ask
  return $ VThunk env v e
interp (Call e1 e2 _) = do
  e1' <- interp e1
  case e1' of
    f@(VPrim _) -> do
      e2' <- interp e2
      either throwM (return) $ eval1 f e2'
    (VThunk env v expr) -> do
      e2' <- interp e2
      x <- addVar (v, e2')
      s <- get
      a <- ask
      trace (show s) $ trace (show a) $ trace ("asd" ++ show x)
        $ local (\_ -> x : env)
        $ interp expr
    x ->
      throwM $ EnvError (show x)
--interp (Define n e1 _) = do
--  e <- interp e1
--  addVar (n, e1)
--  return e
interp (Seq e1 e2 _) = interp e1 >> interp e2
interp (Assign n e1 _) = do
  e <- interp e1
  setVar n e
  return e
interp _ = undefined

op o a = pure $ VPrim $ op' o a

op' "+" (VInt a) (VInt b) = pure $ VInt (a + b)
op' "-" (VInt a) (VInt b) = pure $ VInt (a - b)
op' "*" (VInt a) (VInt b) = pure $ VInt (a * b)
op' "&&*" (VBool a) (VBool b) = pure $ VBool (a && b)
op' "||" (VBool a) (VBool b) = pure $ VBool (a || b)
op' "==" a b = pure $ VBool (a == b)
op' "<" a b = pure $ VBool (a < b)
op' ">" a b = pure $ VBool (a > b)
op' _ _ _ = Left TypeError

initialEnv = unzip [((o, i), (i, VPrim $ op o)) | (i, o) <- zip [0 ..] ["+", "-", "*", "==", "||", "&&", "<", ">"]]

runInterp :: Expr a -> IO Value
runInterp s = runReaderT (evalStateT (interp s) (l + 2, store')) env'
  where
    (env, store) = initialEnv
    l = length env
    env' = ("print", l) : env
    store' = (l, print') : store

print' = VPrim $ \v -> Right $ trace (show v) v