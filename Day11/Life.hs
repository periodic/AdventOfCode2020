module Life where

import Control.Applicative
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State
import Data.Array.ST
import Data.Array.Unboxed as Array
import Data.Ix
import Data.List as L
import Data.Maybe as Maybe
import Data.Text as T


-- | Cell is the representation of the contents of a cell.
type Cell = Int

unoccupied = 0

occupied = 1

invalid = 2

showCell cell
  | cell == occupied = "#"
  | cell == unoccupied = "L"
  | otherwise = "."

-- | Coord represents a position in the grid.
-- This could probably be switched to V2 Int.
type Coord = (Int, Int)

parseCell :: Char -> Cell
parseCell 'L' = unoccupied
parseCell '#' = occupied
parseCell _ = invalid

-- | Grid is the immutable version of the grid.
newtype Grid = Grid
  { gridToArray :: Array.UArray Coord Cell
  }
  deriving (Eq)

instance Show Grid where
  show (Grid arr) =
    let indexes = range . Array.bounds $ arr
        rows = L.groupBy (\a b -> fst a == fst b) indexes
     in L.intercalate "\n" . L.map (L.concatMap (showCell . (arr !))) $ rows

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

-- | Rules describe the logic for updating cells.
data Rules = Rules
  { neighborRule :: forall s. Coord -> Life s [Cell], -- Would be nice if this couldn't modify the grid.
    updateRule :: Int -> Cell -> Cell
  }

numOccupied :: Grid -> Int
numOccupied (Grid arr) =
  L.length . L.filter (== occupied) . L.map (arr !) . range . bounds $ arr

-- | State while we are iterating.
-- Each cell needs to update simultaneously, so this requires we keep two
-- arrays. The first is active and the second is the next one that we are to
-- fill in.  This just avoids extra allocation.
type LifeState s = (STUArray s Coord Cell, STUArray s Coord Cell)

-- | The monad for running the operations.
newtype Life s a = Life
  { runLife :: ReaderT Rules (StateT (LifeState s) (ST s)) a
  }
  deriving (Functor, Applicative, Monad, MonadState (LifeState s), MonadReader Rules)

-- | Runs the grid through steps until there are no more changes.
runUntilSettled :: Rules -> Grid -> Grid
runUntilSettled rules grid =
  evalLife rules grid loop
  where
    loop = do
      step
      isSettled <- not <$> hasChanges
      unless isSettled loop

evalLife :: Rules -> Grid -> (forall s. Life s ()) -> Grid
evalLife rules grid life =
  let st :: forall s. ST s (STUArray s Coord Cell)
      st = execAndExtractActive rules grid life
   in Grid (runSTUArray st)

execAndExtractActive :: Rules -> Grid -> (forall s. Life s () -> ST s (STUArray s Coord Cell))
execAndExtractActive rules grid life = do
  active <- thaw . gridToArray $ grid
  inactive <- newArray (bounds . gridToArray $ grid) 0
  fmap fst . (`execStateT` (active, inactive)) . (`runReaderT` rules) . runLife $ life

liftST :: ST s a -> Life s a
liftST =
  Life . lift . lift

activeArray :: forall s. Life s (STUArray s Coord Cell)
activeArray =
  gets fst

nextArray :: forall s. Life s (STUArray s Coord Cell)
nextArray =
  gets snd

activeBounds :: forall s. Life s (Coord, Coord)
activeBounds = do
  array <- activeArray
  liftST $ getBounds array

inBounds :: forall s. Coord -> Life s Bool
inBounds coord = do
  bounds <- activeBounds
  return $ inRange bounds coord

getCell :: forall s. Coord -> Life s Cell
getCell coord = do
  array <- activeArray
  liftST $ readArray array coord

setCell :: forall s. Coord -> Cell -> Life s ()
setCell coord val = do
  array <- nextArray
  liftST $ writeArray array coord val

hasChanges :: forall s. Life s Bool
hasChanges = do
  active <- activeArray
  inactive <- nextArray
  bounds <- activeBounds
  anyM (liftST . compareIndex active inactive) (range bounds)
  where
    compareIndex arrA arrB index =
      (/=) <$> readArray arrA index <*> readArray arrB index

swapActive :: forall s. Life s ()
swapActive = do
  (active, inactive) <- get
  put (inactive, active)

rules :: forall s. Life s Rules
rules = ask

step :: forall s. Life s ()
step = do
  bounds <- activeBounds
  let indexes = range bounds
  forM_ indexes $ \index ->
    updateCell index >>= setCell index
  swapActive

updateCell :: forall s. Coord -> Life s Cell
updateCell coord = do
  Rules {neighborRule, updateRule} <- rules
  curr <- getCell coord
  neighborCells <- neighborRule coord
  let numoccupied = L.length $ L.filter (== occupied) neighborCells
  return $ updateRule numoccupied curr

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

directions :: [(Int, Int)]
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

adjacentCells :: forall s. Coord -> Life s [Cell]
adjacentCells (r, c) = do
  bounds <- activeBounds
  let neighborCoords =
        L.filter (inRange bounds)
          . L.map
            (\(dr, dc) -> (r + dr, c + dc))
          $ directions
  mapM getCell neighborCoords

neighborsInSight :: forall s. Coord -> Life s [Cell]
neighborsInSight coord =
  Maybe.catMaybes <$> mapM (findLos coord) directions
  where
    findLos :: forall s. Coord -> Coord -> Life s (Maybe Cell)
    findLos (r, c) d@(dr, dc) =
      let newCoord = (r + dr, c + dc)
       in do
            isInBounds <- inBounds newCoord
            if not isInBounds
              then return Nothing
              else do
                newCell <- getCell newCoord
                if newCell /= invalid
                  then return $ Just newCell
                  else findLos newCoord d

greaterThan :: Int -> Int -> Cell -> Cell
greaterThan limit n prev
  | prev == invalid = invalid
  | n == 0 = occupied
  | n >= limit = unoccupied
  | otherwise = prev
