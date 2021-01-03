module Main where

import System.Environment (getArgs)
import Data.List ()
import Data.Attoparsec.Text ( sepBy, decimal, endOfLine, Parser )
import Data.IntSet as Set
    ( IntSet, empty, fromList, member, toList )
import Data.Maybe ( listToMaybe )
import Control.Monad ( guard )

import Exercise ( runExercise, parseInput )

inputP :: Parser IntSet
inputP =
    Set.fromList <$> decimal `sepBy` endOfLine

main :: IO ()
main = do
    values <- parseInput inputP
    result1 <- runExercise "Part 1" (solve2 2020) values
    print result1
    result2 <- runExercise "Part 2" (solve3 2020) values
    print result2

solve2 :: Int -> IntSet -> Maybe (Int, Int)
solve2 target values = listToMaybe $ do
    x <- Set.toList values
    guard ((target - x) `member` values)
    pure (x, target - x)

solve3 :: Int -> IntSet -> Maybe (Int, Int, Int)
solve3 target values = listToMaybe $ do
    x <- Set.toList values
    case solve2 (target - x) values of
        Just (y, z) ->
            pure (x, y, z)
        Nothing ->
            []