{-# LANGUAGE NamedFieldPuns #-}

module Life (Grid, makeGrid, numOccupied, simpleRules, losRules, step, runNSteps, runUntilSettled) where

import Data.Array as Array
import Data.Ix
import Data.List as L
import Data.Text as T
import Data.Maybe as Maybe

data Cell
  = Empty
  | Occupied
  | Invalid
  deriving (Eq)

instance Show Cell where
  show Empty = "L"
  show Occupied = "#"
  show Invalid = "."

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
  deriving (Eq)

instance Show Grid where
  show (Grid arr) =
    let indexes = range . Array.bounds $ arr
        rows = L.groupBy (\a b -> fst a == fst b) indexes
     in L.intercalate "\n" . L.map (L.concatMap (show . (arr !))) $ rows

data Rules = Rules
  { neighborRule :: Coord -> Grid -> [Cell],
    updateRule :: Int -> Cell -> Cell
  }

numOccupied :: Grid -> Int
numOccupied (Grid arr) =
  L.length . L.filter (== Occupied) . L.map (arr !) . range . bounds $ arr

runUntilSettled :: Rules -> Grid -> Grid
runUntilSettled rules grid =
  let nextGrid = step rules grid
   in if nextGrid == grid
        then grid
        else runUntilSettled rules nextGrid

runNSteps :: Rules -> Int -> Grid -> Grid
runNSteps rules steps =
  L.head . L.drop steps . iterate (step rules)

step :: Rules -> Grid -> Grid
step rules grid =
  let bounds = Array.bounds . gridToArray $ grid
      indexes = range bounds
      newValues = L.zip indexes . L.map (updateCell rules grid) $ indexes
   in Grid $ Array.array bounds newValues

updateCell :: Rules -> Grid -> Coord -> Cell
updateCell Rules {neighborRule, updateRule} grid coord =
  let curr = gridToArray grid ! coord
      neighborCells = neighborRule coord grid
      numOccupied = L.length $ L.filter (== Occupied) neighborCells
   in updateRule numOccupied curr

simpleRules =
  Rules
    { neighborRule = adjacentCells,
      updateRule = greaterThan 4
    }

losRules =
  Rules
    { neighborRule = neighborsInSight,
      updateRule = greaterThan 5
    }

directions =
  [ (1, 1),
    (1, 0),
    (1, -1),
    (0, -1),
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, 1)
  ]

adjacentCells :: Coord -> Grid -> [Cell]
adjacentCells (r, c) (Grid arr) =
  let bounds = Array.bounds arr
   in L.map (arr !) . L.filter (inRange bounds) $ neighborCoords
  where
    neighborCoords =
      L.map
        (\(dr, dc) -> (r + dr, c + dc))
        directions

neighborsInSight :: Coord -> Grid -> [Cell]
neighborsInSight coord (Grid arr) =
  Maybe.mapMaybe (findLos coord) directions
  where
    bounds = Array.bounds arr

    findLos :: Coord -> (Int, Int) -> Maybe Cell
    findLos (r, c) d@(dr, dc) =
      let newCoord = (r + dr, c + dc)
          newCell = arr ! newCoord
       in if not . inRange bounds $ newCoord
            then Nothing
            else
              if newCell /= Invalid
                then Just newCell
                else findLos newCoord d

greaterThan :: Int -> Int -> Cell -> Cell
greaterThan _ _ Invalid = Invalid
greaterThan limit n prev
  | n == 0 = Occupied
  | n >= limit = Empty
  | otherwise = prev
