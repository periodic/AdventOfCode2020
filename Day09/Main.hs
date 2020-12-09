{-# LANGUAGE TupleSections #-}
module Main where

import System.Environment ( getArgs )
import qualified Data.Attoparsec.Text as P
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO

import Common

main :: IO ()
main = do
    numbers <- loadAndParseInput xmasInput
    case findInvalid numbers of
        Nothing ->
            putStrLn "No invalid value found."
        Just invalid -> do
            putStr "Invalid value: "
            print invalid
            let subset = findContiguousSubsetSum invalid numbers
            putStr "Subset summing to invalid value: "
            print . V.toList $ subset
            putStr "Sum of smallest and largest: "
            print $ V.minimum subset + V.maximum subset


xmasInput :: P.Parser (Vector Int)
xmasInput =
    V.fromList <$> P.sepBy P.decimal P.endOfLine

hasSum :: Int -> Vector Int -> Bool
hasSum target vector =
    elem target
    . fmap add
    . fmap (\(a, b) -> (vector ! a, vector ! b))
    . pairs
    . (\x -> [0..(x - 1)])
    . V.length
    $ vector
    where
        add = uncurry (+)
        pairs :: [a] -> [(a, a)]
        pairs (x : xs) =
            (x,) `fmap` xs ++ pairs xs
        pairs [] =
            []


findInvalid :: Vector Int -> Maybe Int
findInvalid vals =
    findInvalidR 25
    where
        findInvalidR index =
            if index >= V.length vals
            then Nothing
            else
                let
                    prefix = V.slice (index - 25) 25 vals
                    curr = vals ! index
                    valid = hasSum curr prefix 
                in
                    if not valid
                        then Just curr
                        else findInvalidR (index + 1)


findContiguousSubsetSum :: Int -> Vector Int -> Vector Int
findContiguousSubsetSum target values =
    findSum 0 1
    where
        findSum start size =
            let
                subset = V.slice start size values
                subsetSum = V.sum subset
            in
                case compare subsetSum target of
                    EQ ->
                        subset
                    LT ->
                        findSum start (size + 1)
                    GT ->
                        findSum (start + 1) (size - 1)



