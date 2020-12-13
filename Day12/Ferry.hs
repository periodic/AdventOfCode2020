module Ferry where

import Types

data FerryState = FerryState AbsDirection (Int, Int)
  deriving (Show)
  
updatePosition :: (Int, Int) -> FerryState -> FerryState
updatePosition delta (FerryState facing position) =
  FerryState facing . addVector delta $ position

executeRotation :: RotationDirection -> Int -> FerryState -> FerryState
executeRotation direction degrees (FerryState facing position) =
  let
    steps = degrees `div` 90
    relativeSteps =
      if direction == Clockwise
        then steps
        else -steps
    dirOffset North = 0
    dirOffset East = 1
    dirOffset South = 2
    dirOffset West = 3
    newDirSteps = (dirOffset facing + relativeSteps) `mod` 4
    directions = [North, East, South, West]
  in
    flip FerryState position . head . drop newDirSteps $ directions

executeAbsMovement :: AbsDirection -> Int -> FerryState -> FerryState
executeAbsMovement North dist =
  updatePosition (0, dist)
executeAbsMovement East dist =
  updatePosition (dist, 0)
executeAbsMovement South dist =
  updatePosition (0, -dist)
executeAbsMovement West dist =
  updatePosition (-dist, 0)

executeRelMovement :: Int -> FerryState -> FerryState
executeRelMovement dist ferry@(FerryState facing _) =
  executeAbsMovement facing dist ferry

execute :: Instruction -> FerryState -> FerryState
execute (AbsMovement dir dist) =
  executeAbsMovement dir dist
execute (Rotation dir amount) =
  executeRotation dir amount
execute (RelativeMovement dist) =
  executeRelMovement dist

initialState :: FerryState
initialState =
  FerryState East (0, 0)
