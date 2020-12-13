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

addVector :: (Int, Int) -> (Int, Int) -> (Int, Int)
addVector (x1, y1) (x2, y2) =
  (x1 + x2, y1 + y2)
