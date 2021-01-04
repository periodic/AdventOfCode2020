module Main where

import Data.List (sort, find)
import Exercise
import Text.Printf
import qualified Data.Text as T

main :: IO ()
main = do
  contents <- readInput
  seatIds <- runExercise "Parsing" (map parseSeatId . T.lines) contents
  maximumId <- runExercise "Part 1" maximum seatIds
  printf "Maximum seat ID: %d\n" maximumId
  missingId <- runExercise "Part 2" findMissingSeat seatIds
  printf "Missing seat ID: %s\n" (show missingId)

findMissingSeat :: [Int] -> Maybe Int
findMissingSeat seatIds =
    let sortedIds = sort seatIds
    in (+1) . fst <$> (find (\(a, b) -> a /= b - 1) . zip sortedIds . tail $ sortedIds)

parseSeatId :: T.Text -> Int
parseSeatId =
  T.foldl addDigit 0
  where
    addDigit num 'B' = num * 2 + 1
    addDigit num 'R' = num * 2 + 1
    addDigit num _ = num * 2