module Main where

import System.Environment
import qualified Data.Array.IArray as Array
import qualified Data.List as L
import Data.List.Index (indexed)
import Data.Maybe (isJust, catMaybes)
import qualified Data.Text as T
import Text.Printf

import Exercise

type Index = (Int, Int)
type Element = Bool

addIndexes :: Index -> Index -> Index
addIndexes (r1, c1) (r2, c2) =
    (r1 + r2, c1 + c2)

iRow :: Index -> Int
iRow = fst

iCol :: Index -> Int
iCol = snd

newtype Terrain = Terrain { getArray :: Array.Array Index Element }
    deriving Show

main :: IO ()
main = do
    contents <- T.unpack <$> readInput
    let terrain = makeTerrain contents
    result1 <- runExercise "Part 1" (countTrees (1, 3)) terrain
    printf "Trees on (1, 3): %d\n" result1
    result2 <- runExercise "Part 2" (\t -> map (`countTrees` t) [(1,1), (1,5), (1,7), (2,1)]) terrain
    printf "Tree product: %d\n" . product $ result1 : result2

printTrees :: Index -> Terrain -> IO Int
printTrees vector terrain = 
    let
        count = countTrees vector terrain
    in do
        putStrLn $ "Trees on " ++ show vector ++ ": " ++ show count
        return count

countTrees :: Index -> Terrain -> Int
countTrees vect terrain =
    let
        indexes = L.iterate (addIndexes vect) vect
        trees = map (`get` terrain) indexes
    in
        length . filter id . catMaybes . takeWhile isJust $ trees


makeTerrain :: String -> Terrain
makeTerrain input =
    let
        rows = lines input
        numRows = length rows - 1
        numCols = (\x -> x - 1) . maximum . map length $ rows
        lowerBound = (0, 0)
        upperBound = (numRows, numCols)
        parsedCells = map (map (== '#')) rows
        indexedCells =
            concatMap (\(r, row) -> map (\(c, e) -> ((r, c), e)) row)
            . indexed
            . map indexed
            $ parsedCells
    in
        Terrain $ Array.array (lowerBound, upperBound) indexedCells

get :: Index -> Terrain -> Maybe Bool
get index (Terrain array) =
    let
        (_, (maxRow, maxCol)) = Array.bounds array
        boundedIndex = (iRow index, mod (iCol index) (maxCol + 1))
    in
        if iRow index > maxRow
            then Nothing
            else Just (array Array.! boundedIndex)