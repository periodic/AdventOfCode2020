module Main where

import Exercise
import Graph
import Parser
import Text.Printf
import Types

main :: IO ()
main = do
  rules <- parseInput bagRules
  let graph = makeGraphFromRules rules
  countContainers <- runExercise "Part 1" (length . allContainers (Bag "shiny" "gold")) graph
  printf "Total containers for 'shiny gold': %d\n" countContainers
  countContents <- runExercise "Part 2" (countContents (Bag "shiny" "gold")) graph
  printf "Total contents for 'shiny gold': %d\n" countContents
