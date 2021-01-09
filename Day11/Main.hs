{-# LANGUAGE OverloadedStrings #-}
module Main where

import Exercise
import Data.Attoparsec.Text as P
import qualified Data.Text as T
import Text.Printf
import Life

testGrid :: Grid
testGrid = makeGrid . T.intercalate "\n" $
  [ "L.LL.LL.LL"
  , "LLLLLLL.LL"
  , "L.L.L..L.."
  , "LLLL.LL.LL"
  , "L.LL.LL.LL"
  , "L.LLLLL.LL"
  , "..L.L....."
  , "LLLLLLLLLL"
  , "L.LLLLLL.L"
  , "L.LLLLL.LL"
  ]

main = do
  grid <- parseInput (makeGrid <$> P.takeText)
  result1 <- runExercise "Part 1" (numOccupied . runUntilSettled simpleRules) grid
  printf "Number of occupied seats after settling (Adjacent): %d\n" result1
  result2 <- runExercise "Part 2" (numOccupied . runUntilSettled losRules) grid
  printf "Number of occupied seats after settling (LOS): %d\n" result2
