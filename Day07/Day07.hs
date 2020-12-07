{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment ( getArgs )
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO

import Types
import Parser
import Graph

main :: IO ()
main = do
    [path] <- getArgs
    contents <- TextIO.readFile path
    case P.parseOnly bagRules contents of
        Left err -> do
            putStr "Unable to parse rules: "
            putStrLn err
        Right rules -> do
            let graph = makeGraphFromRules rules
            putStr "Total containers for 'shiny gold': "
            print . length . allContainers (Bag "shiny" "gold") $ graph
            putStr "Total contents for 'shiny gold': "
            print . countContents (Bag "shiny" "gold") $ graph

