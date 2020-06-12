{-# LANGUAGE DeriveFunctor #-}

module Cobra.Types where

data BinOp
  = Add
  | Sub
  | Mul
  | Less
  | Greater
  | Equal
  | And
  | Or
  deriving (Eq)

instance Show BinOp where
  show Add = "+"
  show Mul = "*"
  show Sub = "-"
  show Less = "<"
  show Greater = ">"
  show Equal = "=="
  show And = "&&"
  show Or = "||"

data Expr a
  = Num Integer a
  | Bool Bool a
  | Var String a
  | PrimOp BinOp (Expr a) (Expr a) a
  | Let String (Expr a) (Expr a) a
  | If (Expr a) (Expr a) (Expr a) a
  | App String [Expr a] a

  deriving (Show, Functor)
instance Foldable Expr where
  foldMap f (App _ __ a) = f a
  foldMap f (Num _ a) = f a
  foldMap f (Bool _ a) = f a
  foldMap f (Var _ a) = f a
  foldMap f (PrimOp _ l r a) = foldMap f l <> foldMap f r <> f a
  foldMap f (Let _ l r a) = foldMap f l <> foldMap f r <> f a
  foldMap f (If c l r a) = foldMap f c <> foldMap f l <> foldMap f r <> f a

instance Traversable Expr where
  traverse f (App i j a) = App i <$> (traverse . traverse) f j <*> f a
  traverse f (Num i a) = Num i <$> f a
  traverse f (Bool i a) = Bool i <$> f a
  traverse f (Var i a) = Var i <$> f a
  traverse f (PrimOp i l r a) = PrimOp i <$> traverse f l <*> traverse f r <*> f a
  traverse f (Let i l r a) = Let i <$> traverse f l <*> traverse f r <*> f a
  traverse f (If c l r a) = If <$> traverse f c <*> traverse f l <*> traverse f r <*> f a

data Decl a = Decl
  { dName :: String,
    dArgs :: [String],
    dBody :: Expr a,
    dLabel :: a
  }
  deriving (Functor, Show)

data Program a = Program
  { pFuns :: [Decl a],
    pbody :: Expr a
  }
  deriving (Functor, Show)