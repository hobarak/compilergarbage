{-# LANGUAGE Strict #-}
module Exercises.Chapter1 where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State.Strict
import Data.Maybe (fromJust)
import Data.Semigroup (Max (Max), getMax)

type Id = String

data BinOp = Plus | Minus | Times | Div

data Stm = Compound Stm Stm | Assign Id Expr | Print [Expr]

data Expr = Id Id | Num Int | Op BinOp Expr Expr | Eseq Stm Expr

t = Compound (Assign "a" (Op Plus (Num 2) (Num 3))) (Compound (Assign "b" (Eseq (Print [Id "a"]) (Op Times (Num 2) (Num 10)))) (Print [Id "b"]))

maxargs :: Stm -> Int
maxargs = getMax . go
  where
    go :: Stm -> Max Int
    go (Compound l r) = go l <> go r
    go (Print xs) = foldMap goExpr xs <> Max (length xs)
    go (Assign _ expr) = goExpr expr
    goExpr (Eseq stm expr) = go stm <> goExpr expr
    goExpr (Op o l r) = goExpr l <> goExpr r
    goExpr _ = mempty

interp s = runStateT (go s) []
  where
    go (Compound l r) = go l >> go r
    go (Print xs) = mapM goExpr xs >>= (lift . print)
    go (Assign id expr) = goExpr expr >>= (\v -> modify' ((id, v) :))
    goExpr :: Expr -> StateT [(String, Int)] IO Int
    goExpr (Eseq stm expr) = go stm >> goExpr expr
    goExpr (Op o l r) = op o <$> goExpr l <*> goExpr r
    goExpr (Id i) = gets (fromJust . lookup i)
    goExpr (Num n) = return n

op Plus = (+)
op Minus = (-)
op Times = (*)
op Div = div

t1 = Print [Num 2]

-- >>> interp t

