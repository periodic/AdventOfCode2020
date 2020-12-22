module Main where

import Common
import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Tile
import BitMap

--------------------------------------------------
-- Parsing
--------------------------------------------------

tileHeaderP :: Parser TileId
tileHeaderP =
  TileId <$> ("Tile " *> decimal <* ":" <* endOfLine)

dataToCoords :: [[Bool]] -> [Coord]
dataToCoords input =
  let indexed = concatMap (\(y, row) -> map (\(x, d) -> ((x, y), d)) row) . zip [0 ..] . map (zip [0 ..]) $ input
   in map (uncurry makeCoord . fst) . filter snd $ indexed

tileDataP :: Parser [Coord]
tileDataP =
  let offP = char '.' $> False
      onP = char '#' $> True
      tileRowP = many1 (onP <|> offP)
   in dataToCoords <$> tileRowP `sepBy1` endOfLine

tileP :: Parser Tile
tileP =
  makeTile
    <$> tileHeaderP
    <*> tileDataP

tilesP :: Parser [Tile]
tilesP =
  tileP `sepBy` (endOfLine *> endOfLine)

--------------------------------------------------
-- Part One
--------------------------------------------------

newtype TileArrangement = TileArrangement 
  { placements :: M.Map Coord Tile
  } deriving (Show)

directions =
  [ North
  , East
  , South
  , West]

opposite North = South
opposite East = West 
opposite South = North
opposite West = East

offsetForDirection :: Coord -> Direction -> Coord
offsetForDirection origin North = origin + makeCoord 0 (-1)
offsetForDirection origin East  = origin + makeCoord 1    0
offsetForDirection origin South = origin + makeCoord 0    1
offsetForDirection origin West  = origin + makeCoord (-1) 0

transformations :: [Transformation]
transformations =
  let
    flips = [identityTransform, flipHoriz, flipHoriz . flipVert]
    rotations = [identityTransform, rotateRight, rotateLeft, rotateRight . rotateRight]
  in
    [flip . rotation | flip <- flips, rotation <- rotations]

findTile :: Coord -> [Tile] -> TileArrangement -> Maybe Tile
findTile coord availableTiles arrangement@(TileArrangement placements) =
  let
    possibleMatches = [applyTransform transform tile | transform <- transformations, tile <- availableTiles]
  in
    if M.member coord placements
      then Nothing
      else L.find (canFit coord arrangement) possibleMatches

canFit :: Coord -> TileArrangement -> Tile -> Bool
canFit coord (TileArrangement placements) tile =
  all matchesDir directions
  where
    matchesDir direction =
      case M.lookup (offsetForDirection coord direction) placements of
        Nothing ->
          True 
        Just matchingTile ->
          getEdge direction tile == getEdge (opposite direction) matchingTile

arrangeTiles :: [Tile] -> TileArrangement
arrangeTiles tiles =
  buildWithFrontier [makeCoord 0 0] tiles (TileArrangement M.empty)

buildWithFrontier :: [Coord] -> [Tile] -> TileArrangement -> TileArrangement
buildWithFrontier [] tiles arrangement =
  arrangement
buildWithFrontier (nextCoord : frontier) availableTiles arrangement =
  case findTile nextCoord availableTiles arrangement of
    Nothing ->
      buildWithFrontier frontier availableTiles arrangement
    Just tile ->
        let
          newAvailableTiles = filter (/= tile) availableTiles
          newArrangement = TileArrangement . M.insert nextCoord tile . placements $ arrangement
          newFrontier = map (offsetForDirection nextCoord) directions ++ frontier
        in
          buildWithFrontier newFrontier newAvailableTiles newArrangement

bounds :: TileArrangement -> (Coord, Coord)
bounds (TileArrangement placements) =
  let
    coords = M.keys placements
    minX = minimum . map getX $ coords
    maxX = maximum . map getX $ coords
    minY = minimum . map getY $ coords
    maxY = maximum . map getY $ coords
  in
  (makeCoord minX minY, makeCoord maxX maxY)

partOne :: [Tile] -> IO TileArrangement
partOne tiles =
  let
    arrangement@(TileArrangement placements) = arrangeTiles tiles
    coords = M.keys placements
    (minBound, maxBound) = Main.bounds arrangement
    minX = getX minBound
    maxX = getX maxBound
    minY = getY minBound
    maxY = getY maxBound
  in
    case (
      M.lookup (makeCoord minX minY) placements ,
      M.lookup (makeCoord minX maxY) placements ,
      M.lookup (makeCoord maxX maxY) placements ,
      M.lookup (makeCoord maxX minY) placements) of
        (Just a, Just b, Just c, Just d) -> do
          putStrLn "Corners: "
          print $ tileId a
          print $ tileId b
          print $ tileId c
          print $ tileId d
          putStrLn "Product of corner IDs: "
          print (tileId a * tileId b * tileId c * tileId d)
          return arrangement
        _ ->
          fail "Failed to find a square of tiles."

--------------------------------------------------
-- Part Two
--------------------------------------------------

newtype Image = Image BitMap deriving (Eq)

instance Show Image where
  show (Image imageData) =
    show imageData

makeImage :: TileArrangement -> Image
makeImage (TileArrangement placements) =
  Image . M.foldrWithKey' addTileToImage BitMap.empty $ placements
  where
    addTileToImage pos tile imageData =
      let
        tileX = (maxIndex - 1) * getX pos
        tileY = (maxIndex - 1) * getY pos
        offset = makeCoord tileX tileY
        adjustedBitmap = translate offset . slice (makeCoord 1 1) (makeCoord 8 8) . recenter . tileImageData $ tile
      in
        merge adjustedBitmap imageData

removeDragons :: Image -> Image
removeDragons (Image imageData) =
  Image $ foldr removeDragon imageData . toSet $ imageData
  where
    dragonCoords =
      map (uncurry makeCoord)
      [(0, 0)
      ,(1, 1)
      ,(4, 1)
      ,(5, 0)
      ,(6, 0)
      ,(7, 1)
      ,(10, 1)
      ,(11, 0)
      ,(12, 0)
      ,(13, 1)
      ,(16, 1)
      ,(17, 0)
      ,(18, 0)
      ,(18, -1)
      ,(19, 0)
      ]
    removeDragon :: Coord -> BitMap -> BitMap
    removeDragon coord imageData =
      let
        coordsToCheck = map (coord +) dragonCoords
        isDragon = all (`BitMap.isSet` imageData) coordsToCheck
        updatedImage = foldr BitMap.unset imageData coordsToCheck
      in
        if isDragon
          then updatedImage
          else imageData

partTwo :: TileArrangement -> IO ()
partTwo tileArrangement =
  let
    (Image bitMap) = makeImage tileArrangement
    possibleImages = map (\trans -> Image $ trans bitMap) transformations
    imagesAndUpdates = map (\img -> (img, removeDragons img)) possibleImages
    bestImage = L.find (uncurry (/=)) imagesAndUpdates
  in
    case bestImage of
      Nothing ->
        putStrLn "No sea monsters found"
      Just (originalImage, Image imageData) -> do
        putStrLn "After removing sea monsters choppiness is: "
        print . BitMap.numSet $ imageData
  

--------------------------------------------------
-- Main
--------------------------------------------------

main = do
  tiles <- loadAndParseInput tilesP
  arrangement <- partOne tiles
  partTwo arrangement