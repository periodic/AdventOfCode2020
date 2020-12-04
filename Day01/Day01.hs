{-# LANGUAGE TupleSections #-}
module Main where

import System.Environment (getArgs)
import Data.List ()

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let values = map read $ words contents
    print $ solve2 values
    print $ solve3 values

pairs :: [Int] -> [(Int, Int)]
pairs (x:xs) =
    map (x,) xs ++ pairs xs
pairs [] =
    []

triples :: [Int] -> [(Int, Int, Int)]
triples (x:xs) =
    map (\(y, z) -> (x, y, z)) (pairs xs)
        ++ triples xs
triples [] =
    []

solve2 :: [Int] -> (Int, Int)
solve2 = 
    head . dropWhile (\(x, y) -> x + y /= 2020) . pairs 


solve3 :: [Int] -> (Int, Int, Int)
solve3 = 
    head . dropWhile (\(x, y, z) -> x + y + z /= 2020) . triples