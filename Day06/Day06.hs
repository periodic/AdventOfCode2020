module Main where

import Data.List
import System.Environment (getArgs)

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let groups = groupAnswers . lines $ contents
    putStr "Sum of unique answers per group: "
    print . sumOfAnswerCount . uniqueAnswersPerGroup $ groups
    putStr "Sum of common answers per group: "
    print . sumOfAnswerCount . commonAnswersPerGroup $ groups


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