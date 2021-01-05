module Main where

import Data.List
import System.Environment (getArgs)
import Data.Text (unpack)
import Data.Attoparsec.Text
import qualified Data.Set as S

import Exercise
import Text.Printf

type Answers = S.Set Char

main :: IO ()
main = do
    groups <- parseInput groupsP
    part1 <- runExercise "Part 1" (sumOfAnswerCount . uniqueAnswersPerGroup) groups
    printf "Sum of unique answers per group: %d\n" part1
    part2 <- runExercise "Part 2" (sumOfAnswerCount . commonAnswersPerGroup) groups
    printf "Sum of common answers per group: %d\n" part2


sumOfAnswerCount :: [Answers] -> Int
sumOfAnswerCount =
    sum . map S.size

uniqueAnswersPerGroup :: [[Answers]] -> [Answers]
uniqueAnswersPerGroup =
    map S.unions

groupAnswers :: [String] -> [[String]]
groupAnswers =
    filter hasContent . groupBy breakOnEmptyLines
    where
        breakOnEmptyLines "" _  = False
        breakOnEmptyLines _  "" = False
        breakOnEmptyLines _  _  = True

        hasContent [""] = False
        hasContent _    = True

commonAnswersPerGroup :: [[Answers]] -> [Answers]
commonAnswersPerGroup =
    map (foldr1 S.intersection)

groupsP :: Parser [[Answers]]
groupsP =
    group `sepBy` count 2 endOfLine
    where
        group =
            person `sepBy` endOfLine
        person =
            S.fromList <$> many1 letter