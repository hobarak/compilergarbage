module Boa.Printer where

import Boa.Parser

isImm (Var _ _) = True
isImm (Num _ _) = True
isImm _ = False

toPretty :: Int -> Expr a -> String
toPretty i v@(Var x _) = x
toPretty i n@(Num x _) = show x
toPretty i p@(PrimOp x l r a) = parens l ++ " " ++ str x ++ " " ++ parens r
  where
    parens l = if isImm l then toPretty i l else "(" ++ toPretty i l ++ ")"
toPretty i l@(Let v e1 e2 a) = "let " ++ v ++ " = " ++ toPretty i e1 ++ " in " ++ toPretty i e2
toPretty i l@(If c e1 e2 a) = "if " ++ toPretty i c ++ ":\n" ++ toPretty i e1 ++ " else:\n" ++ toPretty i e2

str Add = "+"
str Mul = "*"
str Div = "/"
str Sub = "-"