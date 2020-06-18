{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Adder.Compiler () where

import Adder.Parser
import Control.Applicative hiding (Const)
import Control.Monad
import Control.Monad.Identity (Identity)
import Data.Char
import Maybes (fromMaybe)
import Text.Printf (printf)

data Reg = EAX | EBP deriving (Show)

data Arg = Reg Reg | Const Integer | RegOffset Reg Int deriving (Show)

data Instruction = ILabel String | IMov Arg Arg | IRet | IAdd Reg Arg deriving (Show)

type Env = [(String, Int)]

emptyEnv = []

get :: String -> Env -> Int
get x = fromMaybe (error "asd") . lookup x

push :: String -> Env -> (Int, Env)
push x xs =
  let n = 1 + length xs
   in (n, (x, n) : xs)

compile :: Env -> Expr a -> [Instruction]
compile e (Num a _) = [IMov (Reg EAX) (Const a)]
compile e (PrimOp Add a _) = compile e a ++ [IAdd EAX (Const 1)]
compile e (PrimOp Sub a _) = compile e a ++ [IAdd EAX (Const (-1))]
compile e (Let v e1 e2 _) = compile e e1 ++ [IMov (RegOffset EBP n) (Reg EAX)] ++ compile e' e2
  where
    (n, e') = push v e
compile e (Var v _) = [IMov (Reg EAX) (RegOffset EBP (get v e))]

class ASM a where
  asm :: a -> String

instance ASM [Instruction] where
  asm = unlines . fmap asm

instance ASM Instruction where
  asm (IMov a b) = "mov " ++ asm a ++ ", " ++ asm b
  asm (IAdd a b) = "add " ++ asm a ++ ", " ++ asm b
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
comp = fmap (toAsm . compile emptyEnv) . parser

prog = do
  str <- readFile "programs/adder.boran"
  let result = comp str
  case result of
    (Right r) -> putStrLn r >> writeFile "target/Adder/prog.s" r
    (Left e) -> print e

-- >>> prog
-- section .text
-- extern error
-- extern print
-- global our_code_label
-- our_code_label:
-- mov eax, 12
-- mov [ebp - 4 * 1] , eax
-- mov eax, [ebp - 4 * 1]
-- add eax, 1
-- add eax, 1
-- add eax, -1
-- mov [ebp - 4 * 2] , eax
-- mov eax, [ebp - 4 * 1]
-- add eax, -1
-- <BLANKLINE>
-- ret
--
