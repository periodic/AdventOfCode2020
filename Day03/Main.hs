module Main where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import qualified Data.List as L
import Data.Maybe (catMaybes, isJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Exercise
import Text.Printf
import Linear.V2

type Index = V2 Int
type Element = Bool

newtype Terrain = Terrain {getArray :: Vector (Vector Element)}
  deriving (Show)

get :: Index -> Terrain -> Maybe Bool
get (V2 x y) (Terrain array) =
  let maxCol = V.length . V.head $ array
      x' =  x `mod` maxCol
   in do
        row <- array V.!? y
        row V.!? x'

terrainP :: Parser Terrain
terrainP =
  Terrain . V.fromList <$> rowP `sepBy` endOfLine
  where
    rowP =
      V.fromList <$> many1 elementP
    elementP =
      "#" $> True <|> "." $> False

main :: IO ()
main = do
  terrain <- parseInput terrainP
  result1 <- runExercise "Part 1" (countTrees $ V2 3 1) terrain
  printf "Trees on (1, 3): %d\n" result1
  result2 <- runExercise "Part 2" (\t -> product . (result1 :) . map (`countTrees` t) $ [V2 1 1, V2 5 1, V2 7 1, V2 1 2]) terrain
  printf "Tree product: %d\n" result2

countTrees :: Index -> Terrain -> Int
countTrees vect terrain =
  let indexes = L.iterate (+ vect) vect
      trees = map (`get` terrain) indexes
   in length . filter id . catMaybes . Prelude.takeWhile isJust $ trees
