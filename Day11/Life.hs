module Life where

import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.Ix
import Data.List as L
import Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T

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

calculateIndex :: Coord -> Coord -> Int
calculateIndex size (y, x) =
  let (height, width) = size
   in if x >= 0 && x < width && y >= 0 && y < height
        then y * (width) + x
        else -1

makeGrid :: Text -> Grid
makeGrid input =
  let rows = T.lines input
      maxRows = L.length rows
      maxCols = L.maximum . L.map T.length $ rows
      size = (maxRows, maxCols)
      parsedCells = L.map (L.map parseCell . T.unpack) rows
      indexedCells =
        L.concatMap (\(y, row) -> L.map (\(x, e) -> ((y, x), e)) row)
          . L.zip [0 ..]
          . L.map (L.zip [0 ..])
          $ parsedCells
      occupiedCells = Set.fromList . map (calculateIndex size . fst) . filter ((== Occupied) . snd) $ indexedCells
      invalidCells = Set.fromList . map (calculateIndex size . fst) . filter ((== Invalid) . snd) $ indexedCells
   in Grid occupiedCells invalidCells size

data Grid = Grid
  { occupiedCells :: !IntSet,
    invalidCells :: !IntSet,
    size :: !(Int, Int)
  }
  deriving (Eq)

indexes :: Grid -> [(Int, Int)]
indexes Grid {size} =
  let (y, x) = size
   in range ((0, 0), (y - 1, x - 1))

getCell :: Coord -> Grid -> Cell
getCell coord Grid {occupiedCells, invalidCells, size} =
  let index = calculateIndex size coord
   in if index < 0
        then Empty
        else
          if Set.member index invalidCells
            then Invalid
            else
              if Set.member index occupiedCells
                then Occupied
                else Empty

setCell :: Coord -> Cell -> Grid -> Grid
setCell !coord !cell grid@Grid {occupiedCells, invalidCells, size} =
  let index = calculateIndex size coord
   in if index < 0
        then grid
        else case cell of
          Occupied ->
            Grid (Set.insert index occupiedCells) (Set.delete index invalidCells) size
          Empty ->
            Grid (Set.delete index occupiedCells) (Set.delete index invalidCells) size
          Invalid ->
            Grid (Set.delete index occupiedCells) (Set.insert index invalidCells) size

instance Show Grid where
  show grid =
    let rows = L.groupBy (\a b -> fst a == fst b) . indexes $ grid
     in L.intercalate "\n" . L.map (L.concatMap (show . (`getCell` grid))) $ rows

data Rules = Rules
  { neighborRule :: Coord -> Grid -> [Coord],
    updateRule :: Int -> Cell -> Cell
  }

numOccupied :: Grid -> Int
numOccupied Grid {occupiedCells} =
  Set.size occupiedCells

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
step rules !grid =
  let newGrid = Grid Set.empty (invalidCells grid) (size grid)
   in foldr (\coord -> updateCell coord (nextValue rules coord grid)) newGrid (indexes grid)

updateCell :: Coord -> Cell -> Grid -> Grid
updateCell coord value grid =
  let curr = getCell coord grid
   in if curr == Invalid || value == curr
        then grid
        else setCell coord value grid

nextValue :: Rules -> Coord -> Grid -> Cell
nextValue Rules {neighborRule, updateRule} coord grid =
  let curr = getCell coord grid
      neighborCells = map (`getCell` grid) $ neighborRule coord grid
      numOccupied = L.length . L.filter (== Occupied) $ neighborCells
   in updateRule numOccupied curr

simpleRules :: Rules
simpleRules =
  Rules
    { neighborRule = adjacentCells,
      updateRule = greaterThan 4
    }

losRules :: Rules
losRules =
  Rules
    { neighborRule = neighborsInSight,
      updateRule = greaterThan 5
    }

directions :: [Coord]
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

adjacentCells :: Coord -> Grid -> [Coord]
adjacentCells (r, c) _ =
  L.map
    (\(dr, dc) -> (r + dr, c + dc))
    directions

neighborsInSight :: Coord -> Grid -> [Coord]
neighborsInSight coord grid =
  L.map (findLos coord) directions
  where
    findLos :: Coord -> (Int, Int) -> Coord
    findLos (r, c) d@(dr, dc) =
      let newCoord = (r + dr, c + dc)
          newCell = getCell newCoord grid
       in if newCell /= Invalid
            then newCoord
            else findLos newCoord d

greaterThan :: Int -> Int -> Cell -> Cell
greaterThan _ _ Invalid = Invalid
greaterThan limit n prev
  | n == 0 = Occupied
  | n >= limit = Empty
  | otherwise = prev
