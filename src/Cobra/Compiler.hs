{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Cobra.Compiler where

import Cobra.Anf
import Cobra.Asm
import Cobra.Parser
import Cobra.Printer
import Cobra.Types
import Control.Applicative hiding (Const)
import Control.Monad
import Control.Monad.Identity (Identity)
import Data.Bits ((.&.), Bits (shiftL))
import Data.Char
import Data.Traversable (mapAccumL)
import Debug.Trace (trace)
import Maybes (fromMaybe)
import Text.Printf (printf)

noTails = fmap (,False)

tails :: String -> Expr a -> Expr (a, Bool)
tails n ap@(App name args a) = if n == name then (App name (fmap noTails args) (a, True)) else noTails ap
tails n l@(If e1 e2 e3 a) = If (noTails e1) (tails n e2) (tails n e3) (a, False)
tails n v@(Var _ _) = noTails v
tails _ n@(Num _ _) = noTails n
tails n b@(Bool _ _) = noTails b
tails n p@(PrimOp x l r a) = PrimOp x (noTails l) (noTails r) (a, False)
tails n l@(Let v e1 e2 a) = Let v (noTails e1) (tails n e2) (a, False)

---

tag :: Expr a -> (Int, Expr Int)
tag = mapAccumL (\acc a -> (acc + 1, acc + 1)) 0

tagger = snd . tag

--

class Repr a where
  repr :: a -> Arg

instance Repr Integer where
  repr i = Const $ shiftL (fromInteger i) 1

instance Repr Bool where
  repr False = Hex 0x00000001
  repr True = Hex 0x80000001

--

type Env = [(String, Int)]

emptyEnv = []

get :: String -> Env -> Int
get x = fromMaybe (error "asd") . lookup x

push :: String -> Env -> (Int, Env)
push x xs =
  let n = 1 + length xs
   in (n, (x, n) : xs)

immArg e (Num r _) = repr r
immArg e (Bool b _) = repr b
immArg e (Var x _) = RegOffset EBP $ get x e
immArg _ _ = undefined

tailCall e name args = concat (zipWith f (fmap negate [2 ..]) (immArg e <$> (args))) ++ [IJmp (DeclImpl name)]
  where
    f index arg = [IMov (Reg EBX) arg, IMov (RegOffset EBP index) (Reg EBX)]

compile :: [(String, Int)] -> Expr (Int, Bool) -> [Instruction]
compile e (App name args (_, False)) = (IPush . immArg e <$> (reverse $ args)) ++ [ICall (DeclLabel name)]
compile e (App name args (_, True)) = tailCall e name args
compile e (Bool a _) = [IMov (Reg EAX) (repr a)]
compile e (Num a _) = [IMov (Reg EAX) (repr a)]
compile e (Var v _) = [IMov (Reg EAX) (RegOffset EBP (get v e))]
compile e (PrimOp x a b _) | x == And || x == Or = assertType e a TBoolean ++ assertType e b TBoolean ++ compile e a ++ [i x (Reg EAX) (immArg e b)]
  where
    i And = IAnd
    i Or = IOr
compile e (PrimOp x a b _) = assertType e a TNumber ++ assertType e b TNumber ++ compile e a ++ instr x
  where
    --instr Equal = [ISub (Reg EAX) (immArg e b), IOr (Reg EAX) (Hex 0x00000001)]
    instr Greater = [ISub (Reg EAX) (immArg e b), IOr (Reg EAX) (Hex 0x7FFFFFFF), INot (Reg EAX), IOr (Reg EAX) (Hex 0x00000001)]
    instr Add = [IAdd (Reg EAX) (immArg e b)]
    instr Sub = [ISub (Reg EAX) (immArg e b)]
    instr Mul = [IMul (Reg EAX) (immArg e b), ISar (Reg EAX) (Const 1)]
    instr Less = [ISub (Reg EAX) (immArg e b), IAnd (Reg EAX) (Hex 0x80000000), IOr (Reg EAX) (Hex 0x00000001)]
compile e (Let v e1 e2 _) = compile e e1 ++ [IMov (RegOffset EBP n) (Reg EAX)] ++ compile e' e2
  where
    (n, e') = push v e
compile e (If cond e1 e2 (i, _)) = assertType e cond TBoolean ++ compile e cond ++ [ICmp (Reg EAX) (repr False), IJne (BranchTrue i)] ++ falseBranch ++ trueBranch ++ end
  where
    trueBranch = ILabel (BranchTrue (i)) : compile e e1
    falseBranch = compile e e2 ++ [IJmp (BrachEnd i)]
    end = [ILabel (BrachEnd i)]

assertType :: Env -> Expr a -> Ty -> [Instruction]
assertType env v ty =
  [ IMov (Reg EAX) (immArg env v),
    IMov (Reg EBX) (Reg EAX),
    IAnd (Reg EBX) (Hex 0x00000001),
    ICmp (Reg EBX) (typeTag ty),
    IJne BranchError
  ]
  where
    typeTag :: Ty -> Arg
    typeTag TNumber = Hex 0x00000000
    typeTag TBoolean = Hex 0x00000001

data Ty = TNumber | TBoolean

runCompile :: String -> String
runCompile str =
  toAsm $ compileBody emptyEnv "" (pbody program) ++ (concatMap compDecl $ pFuns program)
  where
    program = case runParser str of
      Left e -> error (show e)
      (Right r) -> r

compExpr env name = compile env . tails name . tagger . normalize

compDecl :: Decl a -> [Instruction]
compDecl (Decl name args body _) =
  ILabel (DeclLabel name)
    : compileBody (createEnv args) name body

createEnv args = zip args $ fmap negate [2 ..]

countVars :: Num p1 => p2 -> p1
countVars s = 100

compileBody env name body = entryCode body ++ compExpr env name body ++ exitCode
  where
    entryCode e =
      [ IPush (Reg EBP),
        IMov (Reg EBP) (Reg ESP),
        ISub (Reg ESP) (Const $ 4 * n),
        ILabel (DeclImpl name)
      ]
      where
        n = countVars e
    exitCode =
      [ IMov (Reg ESP) (Reg EBP),
        IPop (Reg EBP),
        IRet
      ]

prog = do
  str <- readFile "programs/cobra.boran"
  case runParser str of
    Left e -> error (show e)
    (Right r) -> print $ tails "fac2" $ fmap (const ()) $ normalize $ dBody $ head $ (pFuns r)
  let r = runCompile str
  putStrLn r >> writeFile "target/Adder/prog.s" r

-- >>> prog
-- PrimOp + (Var "a" ((),False)) (Var "b" ((),False)) ((),False)
-- section .text
-- extern error
-- extern print
-- global our_code_label
-- our_code_label:
-- push dword  ebp
-- mov ebp, esp
-- sub esp, 400
-- _impl:
-- mov eax, 4
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, 24
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, 4
-- imul  eax, 24
-- sar eax, 1
-- mov [ebp - 4 * 1] , eax
-- mov eax, 4
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, 4
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, 4
-- imul  eax, 4
-- sar eax, 1
-- mov [ebp - 4 * 2] , eax
-- mov eax, [ebp - 4 * 2] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, 6
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * 2] 
-- sub eax, 6
-- and eax, 0x80000000
-- or eax, 0x00000001
-- mov [ebp - 4 * 3] , eax
-- mov eax, [ebp - 4 * 3] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000001
-- jne label_error
-- mov eax, [ebp - 4 * 3] 
-- cmp eax, 0x00000001
-- jne label_true_13
-- mov eax, 2
-- jmp label_end_13
-- label_true_13:
-- mov eax, [ebp - 4 * 2] 
-- label_end_13:
-- mov [ebp - 4 * 4] , eax
-- mov eax, [ebp - 4 * 4] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, 6
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * 4] 
-- imul  eax, 6
-- sar eax, 1
-- mov [ebp - 4 * 2] , eax
-- mov eax, [ebp - 4 * 1] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * 2] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * 1] 
-- imul  eax, [ebp - 4 * 2] 
-- sar eax, 1
-- mov [ebp - 4 * 1] , eax
-- mov eax, 4
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * 1] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, 4
-- add eax, [ebp - 4 * 1] 
-- mov [ebp - 4 * 2] , eax
-- push dword  8
-- push dword  6
-- call duygus
-- mov [ebp - 4 * 3] , eax
-- mov eax, 8
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, 4
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, 8
-- add eax, 4
-- mov [ebp - 4 * 4] , eax
-- push dword  [ebp - 4 * 4] 
-- push dword  [ebp - 4 * 3] 
-- call boran
-- mov [ebp - 4 * 5] , eax
-- mov eax, [ebp - 4 * 2] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * 5] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * 2] 
-- add eax, [ebp - 4 * 5] 
-- mov [ebp - 4 * 6] , eax
-- push dword  10
-- call fac
-- mov [ebp - 4 * 7] , eax
-- mov eax, [ebp - 4 * 6] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * 7] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * 6] 
-- add eax, [ebp - 4 * 7] 
-- mov [ebp - 4 * 8] , eax
-- push dword  10
-- push dword  2
-- call fac2
-- mov [ebp - 4 * 9] , eax
-- mov eax, [ebp - 4 * 8] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * 9] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * 8] 
-- add eax, [ebp - 4 * 9] 
-- mov esp, ebp
-- pop dword ebp
-- ret
-- boran:
-- push dword  ebp
-- mov ebp, esp
-- sub esp, 400
-- boran_impl:
-- mov eax, [ebp - 4 * -2] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * -3] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * -2] 
-- add eax, [ebp - 4 * -3] 
-- mov esp, ebp
-- pop dword ebp
-- ret
-- duygus:
-- push dword  ebp
-- mov ebp, esp
-- sub esp, 400
-- duygus_impl:
-- mov eax, [ebp - 4 * -2] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * -3] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * -2] 
-- imul  eax, [ebp - 4 * -3] 
-- sar eax, 1
-- mov esp, ebp
-- pop dword ebp
-- ret
-- fac:
-- push dword  ebp
-- mov ebp, esp
-- sub esp, 400
-- fac_impl:
-- mov eax, [ebp - 4 * -2] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, 4
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * -2] 
-- sub eax, 4
-- and eax, 0x80000000
-- or eax, 0x00000001
-- mov [ebp - 4 * 2] , eax
-- mov eax, [ebp - 4 * 2] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000001
-- jne label_error
-- mov eax, [ebp - 4 * 2] 
-- cmp eax, 0x00000001
-- jne label_true_16
-- mov eax, [ebp - 4 * -2] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, 2
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * -2] 
-- sub eax, 2
-- mov [ebp - 4 * 3] , eax
-- push dword  [ebp - 4 * 3] 
-- call fac
-- mov [ebp - 4 * 4] , eax
-- mov eax, [ebp - 4 * -2] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * 4] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * -2] 
-- imul  eax, [ebp - 4 * 4] 
-- sar eax, 1
-- jmp label_end_16
-- label_true_16:
-- mov eax, 2
-- label_end_16:
-- mov esp, ebp
-- pop dword ebp
-- ret
-- fac2:
-- push dword  ebp
-- mov ebp, esp
-- sub esp, 400
-- fac2_impl:
-- mov eax, [ebp - 4 * -3] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, 4
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * -3] 
-- sub eax, 4
-- and eax, 0x80000000
-- or eax, 0x00000001
-- mov [ebp - 4 * 3] , eax
-- mov eax, [ebp - 4 * 3] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000001
-- jne label_error
-- mov eax, [ebp - 4 * 3] 
-- cmp eax, 0x00000001
-- jne label_true_17
-- mov eax, [ebp - 4 * -2] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * -3] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * -2] 
-- imul  eax, [ebp - 4 * -3] 
-- sar eax, 1
-- mov [ebp - 4 * 4] , eax
-- mov eax, [ebp - 4 * -3] 
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, 2
-- mov ebx, eax
-- and ebx, 0x00000001
-- cmp ebx, 0x00000000
-- jne label_error
-- mov eax, [ebp - 4 * -3] 
-- sub eax, 2
-- mov [ebp - 4 * 5] , eax
-- mov ebx, [ebp - 4 * 4] 
-- mov [ebp - 4 * -2] , ebx
-- mov ebx, [ebp - 4 * 5] 
-- mov [ebp - 4 * -3] , ebx
-- jmp fac2_impl
-- jmp label_end_17
-- label_true_17:
-- mov eax, [ebp - 4 * -2] 
-- label_end_17:
-- mov esp, ebp
-- pop dword ebp
-- ret
-- label_error:
-- push eax
-- push 0
-- call error
-- <BLANKLINE>
--
