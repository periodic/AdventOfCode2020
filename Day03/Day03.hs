module Main where

import System.Environment
import qualified Data.Array.IArray as Array
import qualified Data.List as L
import Data.List.Index (indexed)
import Data.Maybe (isJust, catMaybes)

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
    [path] <- getArgs
    contents <- readFile path
    let terrain = makeTerrain contents
    count_1_1 <- printTrees (1,1) terrain
    count_1_3 <- printTrees (1,3) terrain
    count_1_5 <- printTrees (1,5) terrain
    count_1_7 <- printTrees (1,7) terrain
    count_2_1 <- printTrees (2,1) terrain
    let product = count_1_1 * count_1_3 * count_1_5 * count_1_7 * count_2_1
    putStrLn $ "Tree product: " ++ show product

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