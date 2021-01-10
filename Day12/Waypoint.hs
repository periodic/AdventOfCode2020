module Waypoint where

import Types

data FerryState = FerryState AbsDirection (Int, Int) (Int, Int)
  deriving (Show)

totalDistance :: FerryState -> Int
totalDistance (FerryState _ (n, e) _) =
  abs n + abs e
  
updatePosition :: (Int, Int) -> FerryState -> FerryState
updatePosition delta (FerryState facing position waypoint) =
  FerryState facing (addVector delta position) waypoint

updateWaypoint :: (Int, Int) -> FerryState -> FerryState
updateWaypoint delta (FerryState facing position waypoint) =
  FerryState facing position (addVector delta waypoint)

rotateDirection :: RotationDirection -> Int -> AbsDirection -> AbsDirection
rotateDirection rotation degrees prevDirection =
  let
    steps = degrees `div` 90
    relativeSteps =
      if rotation == Clockwise
        then steps
        else -steps
    dirOffset North = 0
    dirOffset East = 1
    dirOffset South = 2
    dirOffset West = 3
    newDirSteps = (dirOffset prevDirection + relativeSteps) `mod` 4
    directions = [North, East, South, West]
  in
    directions !! max 0 newDirSteps

executeRotation :: RotationDirection -> Int -> FerryState -> FerryState
executeRotation rotation degrees (FerryState facing position waypoint) =
  let
    horizontalDir = rotateDirection rotation degrees East
    verticalDir = rotateDirection rotation degrees North
  in
    executeWaypointMovement horizontalDir (fst waypoint)
    . executeWaypointMovement verticalDir (snd waypoint)
    $ FerryState facing position (0, 0)
    

executeWaypointMovement :: AbsDirection -> Int -> FerryState -> FerryState
executeWaypointMovement North dist =
  updateWaypoint (0, dist)
executeWaypointMovement East dist =
  updateWaypoint (dist, 0)
executeWaypointMovement South dist =
  updateWaypoint (0, -dist)
executeWaypointMovement West dist =
  updateWaypoint (-dist, 0)

executeRelativeMovement :: Int -> FerryState -> FerryState
executeRelativeMovement count ferry@(FerryState facing position waypoint) =
  let
    executeMultiple = foldr (.) id . replicate count . updatePosition $ waypoint
  in
    executeMultiple ferry


execute :: Instruction -> FerryState -> FerryState
execute (AbsMovement dir dist) =
  executeWaypointMovement dir dist
execute (Rotation dir amount) =
  executeRotation dir amount
execute (RelativeMovement dist) =
  executeRelativeMovement dist

initialState :: FerryState
initialState =
  FerryState East (0, 0) (10, 1)
