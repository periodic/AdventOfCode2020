module Main where

import System.Environment
import Conway
import Vector3
import Vector4

{-
TODO: Performance
-}

parseInput :: (Ord v) => (Int -> Int -> v) -> String -> Conway v
parseInput constructor input =
  let indexedInput = zip [0 ..] . reverse . map (zip [0 ..]) . lines $ input
   in foldr
        ( \(y, row) conway ->
            foldr
              ( \(x, char) conway ->
                  if char == '#'
                    then activateCell (constructor x y) conway
                    else conway
              )
              conway
              row
        )
        empty
        indexedInput

main = do
  [path] <- getArgs
  conway3 <- parseInput (\x y -> Vector3 x y 0) <$> readFile path
  partOne conway3
  conway4 <- parseInput (\x y -> Vector4 x y 0 0) <$> readFile path
  partTwo conway4

partOne start = do
  putStrLn "Initializing conway engine (3 dimensions)..."
  let final = (!! 6) . iterate step $ start
  putStr "Total active cells: "
  print . numActive $ final
  
partTwo start = do
  putStrLn "Initializing conway engine (4 dimensions)..."
  let final = (!! 6) . iterate step $ start
  putStr "Total active cells: "
  print . numActive $ final
  