module BitMap where

import qualified Data.Set as S
import qualified Data.List as L
import Linear.Matrix
import Linear.V2
import Control.Lens

type Coord = V2 Int

makeCoord :: Int -> Int -> Coord
makeCoord =
  V2 

unmakeCoord :: Coord -> (Int, Int)
unmakeCoord (V2 x y) =
  (x, y)

getX :: Coord -> Int
getX =
  (^. _x)

getY :: Coord -> Int
getY =
  (^. _y)

newtype BitMap = BitMap {
  toSet :: S.Set Coord
} deriving (Eq)

instance Show BitMap where
  show bitMap =
    let
      (V2 minX minY, V2 maxX maxY) = bounds bitMap
      charFor True = '#'
      charFor False = '.'
      row y = map (\x -> charFor . (`isSet` bitMap) $ V2 x y) [minX..maxX]
      rows = map row [minY..maxY]
    in
      L.intercalate "\n" rows
    
empty :: BitMap
empty =
  BitMap S.empty

fromList :: [Coord] -> BitMap
fromList =
  BitMap . S.fromList

type Transformation = BitMap -> BitMap

isSet :: Coord -> BitMap -> Bool
isSet v =
  S.member v . toSet

numSet :: BitMap -> Int
numSet =
  S.size . toSet

set :: Coord -> Transformation
set coord =
  BitMap . S.insert coord . toSet

unset :: Coord -> Transformation
unset coord =
  BitMap . S.delete coord . toSet


bounds :: BitMap -> (Coord, Coord)
bounds bitMap =
  let
    keys = S.elems . toSet $ bitMap
    minX = minimum . map getX $ keys
    minY = minimum . map getY $ keys
    maxX = maximum . map getX $ keys
    maxY = maximum . map getY $ keys
  in
    (V2 minX minY, V2 maxX maxY)

mapBits :: (Coord -> Coord) -> BitMap -> BitMap
mapBits f =
  BitMap . S.map f . toSet
  
recenter :: Transformation
recenter bitMap =
  let
    (minCoord, _) = bounds bitMap
  in
    translate (-minCoord) bitMap

identityTransform :: Transformation
identityTransform =
  id

translate :: Coord -> Transformation
translate coord =
    mapBits (+ coord)

rotateLeft :: Transformation
rotateLeft =
  mapBits (\(V2 x y) -> V2 y (-x))
  
rotateRight :: Transformation
rotateRight =
  mapBits (\(V2 x y) -> V2 (-y) x)

flipVert :: Transformation
flipVert =
  mapBits (\(V2 x y) -> V2 x (-y))

flipHoriz :: Transformation
flipHoriz =
  mapBits (\(V2 x y) -> V2 (-x) y)

slice :: Coord -> Coord -> BitMap -> BitMap
slice (V2 minX minY) (V2 maxX maxY) =
  BitMap . S.filter inBounds . toSet
  where
    inBounds (V2 x y) =
      x >= minX && x <= maxX
      && y >= minY && y <= maxY

merge :: BitMap -> BitMap -> BitMap
merge a b =
  BitMap $ S.union (toSet a) (toSet b)


