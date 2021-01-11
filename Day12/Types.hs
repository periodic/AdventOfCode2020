module Types where

data AbsDirection
  = East
  | West
  | South
  | North
  deriving (Show, Eq)

data RotationDirection
  = Clockwise
  | CounterCW
  deriving (Show, Eq)

data Instruction
  = AbsMovement AbsDirection Int
  | Rotation RotationDirection Int
  | RelativeMovement Int
