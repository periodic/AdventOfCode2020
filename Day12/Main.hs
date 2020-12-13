{-# LANGUAGE OverloadedStrings #-}
module Main where


import Data.Attoparsec.Text as P
import Data.Functor

import Common
import Types
import qualified Ferry
import qualified Waypoint

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
  foldl (\prev inst -> execute inst . prev) id

main = do
  instructions <- loadAndParseInput (P.sepBy instruction P.endOfLine)
  putStr "Executing all instructions as ferry movement: "
  print $ executeAll Ferry.execute instructions Ferry.initialState
  putStr "Executing all instructions with waypoint movement: "
  print $ executeAll Waypoint.execute instructions Waypoint.initialState