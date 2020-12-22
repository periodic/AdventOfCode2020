module Tile where

import qualified Data.List as L
import Linear.V2
import BitMap

newtype TileId = TileId Int
  deriving (Show, Num, Ord, Eq)

data Tile = Tile
  { tileId :: TileId
  , tileImageData :: BitMap
  } deriving (Show)

instance Eq Tile where
  ta == tb = tileId ta == tileId tb

makeTile :: TileId -> [Coord] -> Tile
makeTile tileId =
  Tile tileId . BitMap.fromList

tileSize :: Int
tileSize = 10

maxIndex :: Int
maxIndex = tileSize - 1

applyTransform :: BitMap.Transformation -> Tile -> Tile
applyTransform f (Tile tileId bitMap) =
  Tile tileId (f bitMap)

isSet :: Coord -> Tile -> Bool 
isSet coord (Tile _ tileImageData) =
  BitMap.isSet coord tileImageData

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Ord)

coordsForEdge :: Tile -> Direction -> [Coord]
coordsForEdge (Tile _ bitMap) direction =
  let
    (V2 minX minY, V2 maxX maxY) = bounds bitMap
  in
    case direction of
      North ->
        map (`makeCoord` minY) [minX..maxX]
      South ->
        map (`makeCoord` maxY) [minX..maxX]
      East ->
        map (maxX `makeCoord`) [minY..maxY]
      West ->
        map (minX `makeCoord`) [minY..maxY]

getEdge :: Direction -> Tile -> [Bool]
getEdge edge tile =
  map (`Tile.isSet` tile) . coordsForEdge tile $ edge
