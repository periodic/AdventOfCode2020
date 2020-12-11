{-# LANGUAGE RankNTypes #-}
module Life where

import Control.Monad.ST
import Data.Array as Array
import Data.Array.ST
import Data.Ix
import Data.List as L
import Data.Text as T

data Cell
  = Empty
  | Occupied
  | Invalid
  deriving (Show, Eq)

type Coord = (Int, Int)

parseCell :: Char -> Cell
parseCell 'L' = Empty
parseCell '#' = Occupied
parseCell _ = Invalid

makeGrid :: Text -> Grid
makeGrid input =
  let rows = T.lines input
      numRows = L.length rows - 1
      numCols = (\x -> x - 1) . L.maximum . L.map T.length $ rows
      lowerBound = (0, 0)
      upperBound = (numRows, numCols)
      parsedCells = L.map (L.map parseCell . T.unpack) rows
      indexedCells =
        L.concatMap (\(r, row) -> L.map (\(c, e) -> ((r, c), e)) row)
          . L.zip [0 ..]
          . L.map (L.zip [0 ..])
          $ parsedCells
   in Grid $ Array.array (lowerBound, upperBound) indexedCells

newtype Grid = Grid
  { gridToArray :: Array.Array Coord Cell
  }
  deriving (Show, Eq)

stepGrid :: Grid -> Grid
stepGrid (Grid arr) =
  let
    newArray = runSTArray $ thaw arr >>= step
  in
    Grid newArray

step :: forall s. (STArray s Coord Cell -> ST s (STArray s Coord Cell))
step grid = do
  bounds <- getBounds grid
  mapM_ (`updateCell` grid) $ range bounds
  return grid

updateCell :: Coord -> forall s. (STArray s Coord Cell -> ST s ())
updateCell coord grid = do
  curr <- readArray grid coord
  ns <- neighbors coord grid
  let numOccupied = L.length $ L.filter (== Occupied) ns
  writeArray grid coord $ nextCellValue numOccupied curr

neighbors :: Coord -> forall s. (STArray s Coord Cell -> ST s [Cell])
neighbors (r, c) grid = do
  bounds <- getBounds grid
  mapM (readArray grid) . L.filter (inRange bounds) $ neighborCoords
  where
    neighborCoords =
      L.map
        (\(dr, dc) -> (r + dr, c + dc))
        [ (1, 1),
          (1, 0),
          (1, -1),
          (0, -1),
          (-1, -1),
          (-1, 0),
          (-1, 1),
          (0, 1)
        ]

nextCellValue :: Int -> Cell -> Cell
nextCellValue _ Invalid = Invalid
nextCellValue n prev
  | n == 0 = Occupied
  | n >= 4 = Empty
  | otherwise = prev
