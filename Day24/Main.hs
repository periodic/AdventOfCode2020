module Main where

import Common
import Control.Applicative
import Data.Attoparsec.Text as P
import Data.Functor
import qualified Data.Set as S
import Linear.V2
import qualified Data.List as L
import Control.Lens

--------------------------------------------------
-- Data Types
--------------------------------------------------

type Coord = V2 Int

getX :: Coord -> Int
getX =
  (^. _x)

getY :: Coord -> Int
getY =
  (^. _y)

newtype Floor = Floor {toSet :: S.Set Coord}

instance Show Floor where
  show (Floor tiles) =
    let graphicalCoords = S.map (\(V2 x y) -> V2 (x - (y + 1) `div` 2) y) tiles
        coordList = S.elems graphicalCoords
        minX = minimum . map getX $ coordList
        maxX = maximum . map getX $ coordList
        minY = minimum . map getY $ coordList
        maxY = maximum . map getY $ coordList
        showCoord y x = if S.member (V2 x y) graphicalCoords then "# " else ". "
        row y = concatMap (showCoord y) [minX..maxX] ++ show y
        rows = L.intercalate "\n" . reverse . map (\y -> if even y then row y else ' ' : row y) $ [minY..maxY]
    in
      rows

east :: Coord
east = V2 1 0

west :: Coord
west = V2 (-1) 0

northeast :: Coord
northeast = V2 1 1

northwest :: Coord
northwest = V2 0 1

southeast :: Coord
southeast = V2 0 (-1)

southwest :: Coord
southwest = V2 (-1) (-1)

neighbors = [east, west, northeast, southeast, northwest, southwest]

--------------------------------------------------
-- Parsing
--------------------------------------------------

eastP :: P.Parser Coord
eastP =
  "e" $> east

westP :: P.Parser Coord
westP =
  "w" $> west

northeastP :: P.Parser Coord
northeastP =
  "ne" $> northeast

southeastP :: P.Parser Coord
southeastP =
  "se" $> southeast

southwestP :: P.Parser Coord
southwestP =
  "sw" $> southwest

northwestP :: P.Parser Coord
northwestP =
  "nw" $> northwest

directionP :: P.Parser Coord
directionP =
  northeastP
    <|> northwestP
    <|> southeastP
    <|> southwestP
    <|> eastP
    <|> westP

instructionP :: P.Parser Coord
instructionP =
  sum <$> many1 directionP

instructionsP :: P.Parser [Coord]
instructionsP =
  instructionP `sepBy` endOfLine

--------------------------------------------------
-- Part 1
--------------------------------------------------

flipTile :: Coord -> Floor -> Floor
flipTile coord floor =
  Floor $
    if S.member coord . toSet $ floor
      then S.delete coord . toSet $ floor
      else S.insert coord . toSet $ floor
  
numBlack :: Floor -> Int
numBlack =
  S.size . toSet

partOne :: [Coord] -> IO Floor
partOne instructions =
  let floor = foldr flipTile (Floor S.empty) instructions
  in
   do
     putStr "Black tiles after processing all instructions: "
     print . numBlack $ floor
     return floor

--------------------------------------------------
-- Part 2
--------------------------------------------------

blackNeighbors :: Coord -> Floor -> Int
blackNeighbors coord (Floor blackTiles) = length . filter (\v -> S.member (coord + v) blackTiles) $ neighbors

shouldFlip :: Coord -> Floor -> Bool
shouldFlip coord floor@(Floor blackTiles) =
  let numBlack = blackNeighbors coord floor
  in
    if S.member coord blackTiles
      then numBlack == 0 || numBlack > 2
      else numBlack == 2

tilesToConsider :: Floor -> S.Set Coord
tilesToConsider (Floor blackTiles) =
  foldr insertNeighbors blackTiles blackTiles
  where
    insertNeighbors v tileSet =
      foldr (S.insert . (+ v)) tileSet neighbors

advanceDay :: Floor -> Floor
advanceDay floor =
  foldr maybeFlip floor . tilesToConsider $ floor
  where
    maybeFlip v newFloor =
      if shouldFlip v floor 
        then flipTile v newFloor
        else newFloor

partTwo :: Floor -> IO ()
partTwo floor =
  let days = iterate advanceDay floor
      tileCounts = zip [0..] . map numBlack $ days
  in
    mapM_ (\(day, count) -> putStrLn $ "Day " ++ show day ++ ": " ++ show count) . Prelude.take 101 $ tileCounts


--------------------------------------------------
-- Main
--------------------------------------------------

main :: IO ()
main = do
  instructions <- loadAndParseInput instructionsP
  floor <- partOne instructions
  partTwo floor