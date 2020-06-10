{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Boa.Compiler where

import Boa.Parser
import Boa.Printer
import Control.Applicative hiding (Const)
import Control.Monad
import Control.Monad.Identity (Identity)
import Data.Char
import Data.Traversable (mapAccumL)
import Maybes (fromMaybe)
import Text.Printf (printf)

mkVar i = "x" ++ show i

imm :: Int -> Expr a -> (Int, ([(String, Expr a)], Expr a))
imm i v@(Var _ _) = (i, ([], v))
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
imm i l@(If v e1 e2 a) = (i2 + 1, ([(name, If e11 e21 e31 a)], Var name a))
  where
    (i1, e11) = anf i v
    (i2, e21) = anf i1 e1
    (i3, e31) = anf i2 e2
    name = mkVar (i3 + 1)

mklet [] expr = expr
mklet ((var, value) : xs) expr = Let var value (mklet xs expr) undefined

anf :: Int -> Expr a -> (Int, Expr a)
anf i v@(Var _ _) = (i, v)
anf i n@(Num _ _) = (i, n)
anf i p@(PrimOp x l r a) = (i2, mklet (letl ++ letr) (PrimOp x vl vr a))
  where
    (i1, (letl, vl)) = imm i l
    (i2, (letr, vr)) = imm i1 r
anf i l@(Let v e1 e2 a) = (i2, Let v e11 e21 a)
  where
    (i1, e11) = anf i e1
    (i2, e21) = anf i1 e2
anf i l@(If e1 e2 e3 a) = (i3, If e11 e21 e31 a)
  where
    (i1, e11) = anf i e1
    (i2, e21) = anf i1 e2
    (i3, e31) = anf i2 e3

normalize = snd . anf 0

---

tag :: Expr a -> (Int, Expr (a, Int))
tag e = mapAccumL (\acc a -> (acc + 1, (a, acc + 1))) 0 e

--tag :: Int -> Expr a -> (Int, Expr (a, Int))
--tag i (Var v a) = (i + 1, Var v (a, i + 1))
--tag i (Num n a) = (i + 1, Num n (a, i + 1))
--tag i (PrimOp x l r a) = (i2 + 1, PrimOp x vl vr (a, i2 + 1))
--  where
--    (i1, vl) = tag i l
--    (i2, vr) = tag i1 r
--tag i l@(Let v e1 e2 a) = (i2 + 1, Let v e11 e21 (a, i2 + 1))
--  where
--    (i1, e11) = tag i e1
--    (i2, e21) = tag i1 e2
--tag i l@(If e1 e2 e3 a) = (i3 + 1, If e11 e21 e31 (a, i3 + 1))
--  where
--    (i1, e11) = tag i e1
--    (i2, e21) = tag i1 e2
--    (i3, e31) = tag i2 e3

tagger = snd . tag

--
data Reg = EAX | EBP deriving (Show)

data Arg = Reg Reg | Const Integer | RegOffset Reg Int deriving (Show)

data Label = BranchTrue Int | BrachEnd Int deriving (Show)

data Instruction
  = IMov Arg Arg
  | IRet
  | IAdd Arg Arg
  | ISub Arg Arg
  | IMul Arg Arg
  | IDiv Arg Arg
  | ICmp Arg Arg
  | ILabel Label
  | IJmp Label
  | IJe Label
  | IJne Label
  deriving (Show)

type Env = [(String, Int)]

emptyEnv = []

get :: String -> Env -> Int
get x = fromMaybe (error "asd") . lookup x

push :: String -> Env -> (Int, Env)
push x xs =
  let n = 1 + length xs
   in (n, (x, n) : xs)

immArg e (Num r _) = Const r
immArg e (Var x _) = RegOffset EBP $ get x e

compile :: [(String, Int)] -> Expr (a, Int) -> [Instruction]
compile e (Num a _) = [IMov (Reg EAX) (Const a)]
compile e (PrimOp x a b _) = compile e a ++ [instr x (Reg EAX) (immArg e b)]
  where
    instr Add = IAdd
    instr Sub = ISub
    instr Mul = IMul
    instr Div = IDiv
compile e (Let v e1 e2 _) = compile e e1 ++ [IMov (RegOffset EBP n) (Reg EAX)] ++ compile e' e2
  where
    (n, e') = push v e
compile e (Var v _) = [IMov (Reg EAX) (RegOffset EBP (get v e))]
compile e (If cond e1 e2 (a, i)) = compile e cond ++ [ICmp (Reg EAX) (Const 0), IJne (BranchTrue i)] ++ falseBranch ++ trueBranch ++ end
  where
    trueBranch = ILabel (BranchTrue i) : compile e e1
    falseBranch = compile e e2 ++ [IJmp (BrachEnd i)]
    end = [ILabel (BrachEnd i)]

class ASM a where
  asm :: a -> String

instance ASM [Instruction] where
  asm = unlines . fmap asm

label :: Label -> [Char]
label (BranchTrue i) = "label_true_" ++ show i
label (BrachEnd i) = "label_end_" ++ show i

instance ASM Instruction where
  asm (IMov a b) = "mov " ++ asm a ++ ", " ++ asm b
  asm (IAdd a b) = "add " ++ asm a ++ ", " ++ asm b
  asm (IMul a b) = "imul  " ++ asm a ++ ", " ++ asm b
  asm (ISub a b) = "sub " ++ asm a ++ ", " ++ asm b
  asm (IDiv a b) = "div " ++ asm a ++ ", " ++ asm b
  asm (IJmp lbl) = "jmp " ++ label lbl
  asm (IJne lbl) = "jne " ++ label lbl
  asm (ILabel lbl) = label lbl ++ ":"
  asm (ICmp a b) = "cmp " ++ asm a ++ ", " ++ asm b
  asm IRet = "ret"

instance ASM Reg where
  asm = fmap toLower . show

instance ASM Arg where
  asm (Const a) = show a
  asm (Reg r) = asm r
  asm (RegOffset reg i) = printf "[%s - 4 * %d] " (asm reg) i

header =
  unlines
    [ "section .text",
      "extern error",
      "extern print",
      "global our_code_label",
      "our_code_label:"
    ]

toAsm :: (ASM a) => a -> String
toAsm = (++ "\nret") . (header ++) . asm

comp :: String -> Either String String
comp =
  fmap (toAsm . compile emptyEnv . tagger . normalize) . parser

prog = do
  str <- readFile "programs/boa.boran"
  let result = comp str
  case result of
    (Right r) -> putStrLn r >> writeFile "target/Adder/prog.s" r
    (Left e) -> print e

printer = do
  str <- readFile "programs/boa.boran"
  case parser str of
    (Right r) -> putStrLn (toPretty 0 $ snd $ anf 0 r)
    (Left e) -> print e

-- >>> printer
-- let x = let x1 = 12 * 2 in let x3 = let a = 2 * 2 in let x2 = if a:
-- a else:
-- 1 in x2 * 3 in x1 * x3 in let x4 = 2 + x in let x5 = if 2:
-- 2 else:
-- 1 in x4 + x5
--

-- >>> k
-- 300
--

-- >>> prog
-- section .text
-- extern error
-- extern print
-- global our_code_label
-- our_code_label:
-- mov eax, 12
-- imul  eax, 2
-- mov [ebp - 4 * 1] , eax
-- mov eax, 2
-- imul  eax, 2
-- mov [ebp - 4 * 2] , eax
-- mov eax, [ebp - 4 * 2] 
-- cmp eax, 0
-- jne label_true_10
-- mov eax, 1
-- jmp label_end_10
-- label_true_10:
-- mov eax, [ebp - 4 * 2] 
-- label_end_10:
-- mov [ebp - 4 * 3] , eax
-- mov eax, [ebp - 4 * 3] 
-- imul  eax, 3
-- mov [ebp - 4 * 2] , eax
-- mov eax, [ebp - 4 * 1] 
-- imul  eax, [ebp - 4 * 2] 
-- mov [ebp - 4 * 1] , eax
-- mov eax, 2
-- add eax, [ebp - 4 * 1] 
-- mov [ebp - 4 * 2] , eax
-- mov eax, 2
-- cmp eax, 0
-- jne label_true_27
-- mov eax, 1
-- jmp label_end_27
-- label_true_27:
-- mov eax, 2
-- label_end_27:
-- mov [ebp - 4 * 3] , eax
-- mov eax, [ebp - 4 * 2] 
-- add eax, [ebp - 4 * 3] 
-- <BLANKLINE>
-- ret
--
