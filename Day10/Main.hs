module Main where

import Data.Attoparsec.Text as P
import qualified Data.IntMap as M
import Data.List as L
import Exercise
import Text.Printf

main :: IO ()
main = do
  sortedJoltages <- parseInput (L.sort . addFirstAndLast <$> P.sepBy P.decimal P.endOfLine)
  result1 <- runExercise "Part 1" (uncurry (*) . count1and3 . differences) sortedJoltages
  putStr "ones * threes = "
  print result1
  result2 <- runExercise "Part 2" totalCombinations sortedJoltages
  printf "total combinations = %d\n" result2

addFirstAndLast :: [Int] -> [Int]
addFirstAndLast joltages =
  0 : (maximum joltages + 3) : joltages

differences :: [Int] -> [Int]
differences sortedJoltages =
  zipWith (-) (tail sortedJoltages) sortedJoltages

count1and3 :: [Int] -> (Int, Int)
count1and3 =
  foldr addDiff (0, 0)
  where
    addDiff x (ones, threes) =
      case x of
        1 ->
          (ones + 1, threes)
        3 ->
          (ones, threes + 1)
        _ ->
          (ones, threes)

totalCombinations :: [Int] -> Int
totalCombinations sortedJoltages =
  combinations M.! maximum sortedJoltages
  where
    combinations =
      M.insert 0 1
        . M.mapWithKey
          ( \key _ ->
              sum (L.map (\diff -> M.findWithDefault 0 (key - diff) combinations) [1 .. 3])
          )
        . M.fromList
        . L.map (,1)
        $ sortedJoltages