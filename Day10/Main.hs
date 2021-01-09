module Main where

import Data.Attoparsec.Text as P
import Data.List as L
import Data.IntMap as M
import Text.Printf

import Exercise

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

count1and3 :: [Int] -> (Int, Int)
count1and3 values =
    let
        ones = L.length . L.filter (== 1) $ values
        threes = L.length . L.filter (== 3) $ values
    in
        (ones, threes)

-- Input should be sorted.
differences :: [Int] -> [Int]
differences joltages =
    zipWith (-) (tail joltages) joltages

totalCombinations :: [Int] -> Int
totalCombinations sortedJoltages =
        snd . M.findMax $ L.foldl (flip addJoltage) M.empty sortedJoltages

addJoltage :: Int -> IntMap Int -> IntMap Int
addJoltage j combinationMap =
    let
        combinations = sum . L.map (findWithDiff combinationMap j) $ [1..3]
    in
        if j == 0
            then M.insert j 1 combinationMap
            else M.insert j combinations combinationMap

findWithDiff :: IntMap Int -> Int -> Int -> Int
findWithDiff combinationMap j diff =
    M.findWithDefault 0 (j - diff) combinationMap