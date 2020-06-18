module Exercises.Frame where

import Exercises.AST
type Label = Symbol


data Location = InFrame Int | InReg Int
  deriving (Show)

data Escape = Escape | NoEscape

class Frame f where
  newFrame :: Label -> [Escape] -> f
  name :: f -> Label
  formals :: f -> [Location]
  allocLocal :: Escape -> f -> Location
  locals :: f -> [Location]

data X86Frame = X86Frame
  {
  }
