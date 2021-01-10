{-# LANGUAGE OverloadedStrings #-}
module Main where


import Data.Attoparsec.Text as P
import Data.Functor

import Exercise
import Types
import qualified Ferry
import qualified Waypoint
import Text.Printf

direction =
  P.choice
    [ P.string "R" $> Rotation Clockwise
    , P.string "L" $> Rotation CounterCW
    , P.string "F" $> RelativeMovement
    , P.string "E" $> AbsMovement East
    , P.string "W" $> AbsMovement West
    , P.string "S" $> AbsMovement South
    , P.string "N" $> AbsMovement North
    ]
  
instruction =
  direction
  <*> P.decimal

executeAll :: (Instruction -> a -> a) -> [Instruction] -> a -> a
executeAll execute =
  foldr (flip (.) . execute) id

main = do
  instructions <- parseInput (P.sepBy instruction P.endOfLine)
  result1 <- runExercise "Part 1" (Ferry.totalDistance . executeAll Ferry.execute instructions) Ferry.initialState
  printf "Total Manhattan distance after ferry movement: %d\n" result1
  result2 <- runExercise "Part 2" (Waypoint.totalDistance . executeAll Waypoint.execute instructions) Waypoint.initialState
  printf "Total Manhattan distance after waypoint movement: %d\n" result2