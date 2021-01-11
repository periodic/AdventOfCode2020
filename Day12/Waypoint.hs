module Waypoint where

import Linear.V2
import Linear.Vector
import Types

data FerryState = FerryState (V2 Int) (V2 Int)
  deriving (Show)

totalDistance :: FerryState -> Int
totalDistance (FerryState (V2 n e) _) =
  abs n + abs e

updatePosition :: V2 Int -> FerryState -> FerryState
updatePosition delta (FerryState position waypoint) =
  FerryState (delta + position) waypoint

updateWaypoint :: V2 Int -> FerryState -> FerryState
updateWaypoint delta (FerryState position waypoint) =
  FerryState position (delta + waypoint)

rotateDirection :: RotationDirection -> Int -> AbsDirection -> AbsDirection
rotateDirection rotation degrees prevDirection =
  let steps = degrees `div` 90
      relativeSteps =
        if rotation == Clockwise
          then steps
          else - steps
      dirOffset North = 0
      dirOffset East = 1
      dirOffset South = 2
      dirOffset West = 3
      newDirSteps = (dirOffset prevDirection + relativeSteps) `mod` 4
      directions = [North, East, South, West]
   in directions !! max 0 newDirSteps

executeRotation :: RotationDirection -> Int -> FerryState -> FerryState
executeRotation rotation degrees (FerryState position waypoint@(V2 x y)) =
  let newDirection = rotateDirection rotation degrees East
      newWaypoint =
        case newDirection of
          East ->
            waypoint
          North ->
            V2 (- y) x
          South ->
            V2 y (- x)
          West ->
            V2 (- x) (- y)
   in FerryState position newWaypoint

executeWaypointMovement :: AbsDirection -> Int -> FerryState -> FerryState
executeWaypointMovement North dist =
  updateWaypoint $ V2 0 dist
executeWaypointMovement East dist =
  updateWaypoint $ V2 dist 0
executeWaypointMovement South dist =
  updateWaypoint $ V2 0 (- dist)
executeWaypointMovement West dist =
  updateWaypoint $ V2 (- dist) 0

executeRelativeMovement :: Int -> FerryState -> FerryState
executeRelativeMovement count ferry@(FerryState position waypoint) =
  updatePosition (waypoint ^* count) ferry

execute :: Instruction -> FerryState -> FerryState
execute (AbsMovement dir dist) =
  executeWaypointMovement dir dist
execute (Rotation dir amount) =
  executeRotation dir amount
execute (RelativeMovement dist) =
  executeRelativeMovement dist

initialState :: FerryState
initialState =
  FerryState zero (V2 10 1)
