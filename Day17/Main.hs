module Main where

import Conway
import Vector3
import Vector4
import Exercise
import Text.Printf
import qualified Data.Text as T

{-
TODO: Performance
-}

parseConway :: T.Text -> [(Int, Int)]
parseConway input =
  let indexedInput = zip [0 ..] . reverse . map (zip [0 ..] . T.unpack) . T.lines $ input
   in 
    map fst . filter ((== '#') . snd) . concatMap (\(y, row) -> map (\(x, c) -> ((x, y), c)) row) $ indexedInput

main = do
  input <- readInput
  let conway = parseConway input
  partOne conway
  partTwo conway

partOne conway = do
  let start = fromList . map (\(x, y) -> Vector3 x y 0) $ conway
  activeCount <- runExercise "Part 1" (numActive . (!! 6) . iterate step) start
  putStrLn "Initializing conway engine (3 dimensions)..."
  printf "Total active cells: %d\n" activeCount
  
partTwo conway = do
  let start = fromList . map (\(x, y) -> Vector4 x y 0 0) $ conway
  activeCount <- runExercise "Part 2" (numActive . (!! 6) . iterate step) start
  putStrLn "Initializing conway engine (4 dimensions)..."
  printf "Total active cells: %d\n" activeCount
  