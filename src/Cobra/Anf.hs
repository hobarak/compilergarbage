module Cobra.Anf (normalize) where

import Cobra.Types
import Data.Traversable (mapAccumL)

mkVar :: Show a => a -> String
mkVar i = "x" ++ show i

imm :: Int -> Expr a -> (Int, ([(String, Expr a)], Expr a))
imm i ap@(App name args a) = g $ mapAccumL f (i, []) args
  where
    f (i, lets) expr = ((newi, lets ++ newlets), newexp)
      where
        (newi, (newlets, newexp)) = imm i expr
    g ((i, lets), exprs) = (i + 1, (lets ++ [(mkVar (i + 1), App name exprs a)], Var (mkVar (i + 1)) a))
imm i v@(Var _ _) = (i, ([], v))
imm i b@(Bool _ _) = (i, ([], b))
imm i n@(Num _ _) = (i, ([], n))
imm i p@(PrimOp x l r a) = (i2 + 1, (letl ++ letr ++ [(name, PrimOp x vl vr a)], Var name a))
  where
    (i1, (letl, vl)) = imm i l
    (i2, (letr, vr)) = imm i1 r
    name = mkVar (i2 + 1)
imm i l@(Let v e1 e2 a) = (i2 + 1, ([(name, Let v e11 e21 a)], Var name a))
  where
    (i1, e11) = anf i e1
    (i2, e21) = anf i1 e2
    name = mkVar (i2 + 1)
imm i l@(If v e1 e2 a) = (i2 + 1, (letl ++ [(name, If e11 e21 e31 a)], Var name a))
  where
    (i1, (letl, e11)) = imm i v
    (i2, e21) = anf i1 e1
    (i3, e31) = anf i2 e2
    name = mkVar (i3 + 1)

mklet :: [(String, Expr a)] -> Expr a -> Expr a
mklet [] expr = expr
mklet ((var, value) : xs) expr = Let var value (mklet xs expr) undefined

anf :: Int -> Expr a -> (Int, Expr a)
anf i ap@(App name args a) = g $ mapAccumL f (i, []) args
  where
    f (i, lets) expr = ((newi, lets ++ newlets), newexp)
      where
        (newi, (newlets, newexp)) = imm i expr
    g ((i, lets), exprs) = (i, mklet lets (App name exprs a))
anf i v@(Var _ _) = (i, v)
anf i n@(Num _ _) = (i, n)
anf i b@(Bool _ _) = (i, b)
anf i p@(PrimOp x l r a) = (i2, mklet (letl ++ letr) (PrimOp x vl vr a))
  where
    (i1, (letl, vl)) = imm i l
    (i2, (letr, vr)) = imm i1 r
anf i l@(Let v e1 e2 a) = (i2, Let v e11 e21 a)
  where
    (i1, e11) = anf i e1
    (i2, e21) = anf i1 e2
anf i l@(If e1 e2 e3 a) = (i3, mklet letl $ If e11 e21 e31 a)
  where
    (i1, (letl, e11)) = imm i e1
    (i2, e21) = anf i1 e2
    (i3, e31) = anf i2 e3

normalize :: Expr a -> Expr a
normalize = snd . anf 0