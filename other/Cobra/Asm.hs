{-# LANGUAGE FlexibleInstances #-}

module Cobra.Asm where

import Data.Char (toLower)
import Text.Printf (printf)

data Reg = EAX | EBX | EBP | ESP | ESI deriving (Show)

data Arg = Reg Reg | Const Integer | RegOffset Reg Int | Hex Int deriving (Show)

data Label = BranchTrue Int | BrachEnd Int | BranchError | DeclLabel String | DeclImpl String deriving (Show)

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
  | ISar Arg Arg
  | IAnd Arg Arg
  | IOr Arg Arg
  | INot Arg
  | IPush Arg
  | IPop Arg
  | ICall Label
  deriving (Show)

class ASM a where
  asm :: a -> String

instance ASM [Instruction] where
  asm = unlines . fmap asm

instance ASM Label where
  asm (BranchTrue i) = "label_true_" ++ show i
  asm (BrachEnd i) = "label_end_" ++ show i
  asm BranchError = "label_error"
  asm (DeclLabel s) = s
  asm (DeclImpl s) = s ++ "_impl"

instance ASM Instruction where
  asm (IMov a b) = "mov dword " ++ asm a ++ ", " ++ asm b
  asm (IAdd a b) = "add " ++ asm a ++ ", " ++ asm b
  asm (IMul a b) = "imul  " ++ asm a ++ ", " ++ asm b
  asm (ISub a b) = "sub " ++ asm a ++ ", " ++ asm b
  asm (IDiv a b) = "div " ++ asm a ++ ", " ++ asm b
  asm (IJmp lbl) = "jmp " ++ asm lbl
  asm (IJne lbl) = "jne " ++ asm lbl
  asm (IJe lbl) = "je " ++ asm lbl
  asm (ILabel lbl) = asm lbl ++ ":"
  asm (ICmp a b) = "cmp " ++ asm a ++ ", " ++ asm b
  asm IRet = "ret"
  asm (IAnd a b) = "and " ++ asm a ++ ", " ++ asm b
  asm (IOr a b) = "or " ++ asm a ++ ", " ++ asm b
  asm (ISar a b) = "sar " ++ asm a ++ ", " ++ asm b
  asm (INot a) = "not " ++ asm a
  asm (IPop a) = "pop dword " ++ asm a
  asm (IPush a) = "push dword  " ++ asm a
  asm (ICall a) = unwords ["call", asm a]

instance ASM Reg where
  asm = fmap toLower . show

instance ASM Arg where
  asm (Const a) = show a
  asm (Reg r) = asm r
  asm (Hex r) = printf "0x%08x" r
  asm (RegOffset reg i) = printf "[%s - 4 * %d] " (asm reg) i
header =

  unlines
    [ "section .text",
      "extern error",
      "extern print",
      "global our_code_label",
      "our_code_label:",
      "mov dword esi, [esp + 4] "
    ]

footer = unlines ["label_error:", "push eax", "push 0", "call error"]

toAsm :: (ASM a) => a -> String
toAsm = (header ++) . (++ footer) . asm
