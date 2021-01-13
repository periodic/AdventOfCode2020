module Types where

data MaskValue
  = MaskZero
  | MaskOne
  | MaskUnset
  deriving (Eq)

instance Show MaskValue where
  show MaskZero = "0"
  show MaskOne = "1"
  show MaskUnset = "X"

newtype MemLocation = MemLocation Int
  deriving (Show, Ord, Eq, Num)

data Instruction
  = SetMask [MaskValue]
  | Store MemLocation Int
  deriving (Show)

newtype Program = Program
  { instructions :: [Instruction]
  }
  deriving (Show)