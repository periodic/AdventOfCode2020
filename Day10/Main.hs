module Main where

import Data.Attoparsec.Text as P
import Data.List as L
import Data.IntMap as M
import Common


main :: IO ()
main = do
    joltages <- loadAndParseInput (P.sepBy P.decimal P.endOfLine)
    let sortedJoltages = L.sort . (0 :) . (:) (maximum joltages + 3) $ joltages
    let diffs = differences sortedJoltages
    let (ones, threes) = count1and3 diffs
    putStr "# ones = "
    print ones
    putStr "# threes = "
    print threes
    putStr "ones * threes = "
    print (ones * threes)
    putStr "total combinations = "
    print . totalCombinations $ sortedJoltages

count1and3 :: [Int] -> (Int, Int)
count1and3 values =
    let
        ones = L.length . L.filter (== 1) $ values
        threes = L.length . L.filter (== 3) $ values
    in
        (ones, threes)

-- Input should be sorted.
differences :: [Int] -> [Int]
differences (a:b:rest) =
    (b - a) : differences (b:rest)
differences _ =
    []

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