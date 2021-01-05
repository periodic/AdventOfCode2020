module Main where

import Data.List
import System.Environment (getArgs)
import Data.Text (unpack)
import Data.Attoparsec.Text

import Exercise
import Text.Printf

main :: IO ()
main = do
    contents <- readInput
    groups <- runExercise "Parsing" (groupAnswers . lines . unpack) contents
    part1 <- runExercise "Part 1" (sumOfAnswerCount . uniqueAnswersPerGroup) groups
    printf "Sum of unique answers per group: %d\n" part1
    part2 <- runExercise "Part 2" (sumOfAnswerCount . commonAnswersPerGroup) groups
    printf "Sum of common answers per group: %d\n" part2


sumOfAnswerCount :: [String] -> Int
sumOfAnswerCount =
    sum . map length

uniqueAnswersPerGroup :: [[String]] -> [String]
uniqueAnswersPerGroup =
    map (nub . concat)

groupAnswers :: [String] -> [[String]]
groupAnswers =
    filter hasContent . groupBy breakOnEmptyLines
    where
        breakOnEmptyLines "" _  = False
        breakOnEmptyLines _  "" = False
        breakOnEmptyLines _  _  = True

        hasContent [""] = False
        hasContent _    = True

commonAnswersPerGroup :: [[String]] -> [String]
commonAnswersPerGroup =
    map (foldr1 intersect)