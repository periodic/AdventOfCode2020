module Conway where


import Data.Ix
import qualified Data.Set as S
import qualified Data.List as L

import Vector3 (Vector3 (..))
import Vector4 (Vector4 (..))
import ExpandableRange

newtype Conway v = Conway {
  toSet :: S.Set v
} deriving Show

showV3 conway =
  let
    (Vector3 xl yl zl, Vector3 xu yu zu) = bounds conway
    xs = [xl .. xu]
    ys = [yl .. yu]
    zs = [zl .. zu]
    showCell v =
      if isActive v conway
        then "#"
        else "."
    showRow z y = concatMap (\x -> showCell (Vector3 x y z)) xs
    showSlice z = L.intercalate "\n" . (("z = " ++ show z) :) . map (showRow z) . reverse $ ys
    showGrid = L.intercalate "\n---\n" . map showSlice .reverse $ zs
  in
    showGrid
showV4 conway =
  let
    (Vector4 wl xl yl zl, Vector4 wu xu yu zu) = bounds conway
    ws = [wl .. wu]
    xs = [xl .. xu]
    ys = [yl .. yu]
    zs = [zl .. zu]
    showCell v =
      if isActive v conway
        then "#"
        else "."
    showRow w z y = concatMap (\x -> showCell (Vector4 w x y z)) xs
    showSlice w z = L.intercalate "\n" . (("z = " ++ show z ++ ", w = " ++ show w) :) . map (showRow w z) . reverse $ ys
    showCube w = L.intercalate "\n\n" . map (showSlice w) $ zs
    showGrid = L.intercalate "\n\n" . map showCube $ ws
  in
    showGrid

empty :: Conway v
empty =
  Conway S.empty

activateCell :: (Ord v) => v -> Conway v -> Conway v
activateCell v =
  Conway . S.insert v . toSet

deactivateCell :: (Ord v) => v -> Conway v -> Conway v
deactivateCell v =
  Conway . S.delete v . toSet

isActive :: (Ord v) => v -> Conway v -> Bool
isActive v =
  S.member v . toSet

numActive :: (Ord v) => Conway v -> Int
numActive =
  S.size . toSet

neighborDirs :: (Monoid v, Eq v, Ix v, ExpandableRange v) => [v]
neighborDirs =
  filter (/= mempty) . range . expand $ (mempty, mempty)

numActiveNeighbors :: (Ord v, Monoid v, Ix v, Eq v, ExpandableRange v) => v -> Conway v -> Int
numActiveNeighbors v conway =
  length . filter (`isActive` conway) . map (v <>) $ neighborDirs

bounds :: (Ord v, Monoid v, Ix v, ExpandableRange v) => Conway v -> (v, v)
bounds =
  S.foldr' expandToInclude (mempty, mempty) . toSet

affectedCells :: (ExpandableRange v, Ix v, Monoid v, Ord v) => Conway v -> (v, v)
affectedCells =
  expand . bounds

step :: (ExpandableRange v, Ix v, Monoid v, Ord v) => Conway v -> Conway v
step conway =
  foldr doUpdate conway . range . affectedCells $ conway
  where
    doUpdate v newConway =
      -- Always look at the last step to decide what to do.
      if updateCell v conway
        then activateCell v newConway
        else deactivateCell v newConway

updateCell :: (Ord v, Monoid v, Ix v, Eq v, ExpandableRange v) => v -> Conway v -> Bool
updateCell v conway =
  let active = isActive v conway
      neighbors = numActiveNeighbors v conway
   in if active
        then
          neighbors == 2 || neighbors == 3
        else
          neighbors == 3