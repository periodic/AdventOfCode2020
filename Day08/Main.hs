module Main where

import Data.Monoid
import System.Environment ( getArgs )
import qualified Data.Text.IO as TextIO

import Program
import Parser
import Evaluator
import Exercise

main :: IO ()
main = do
    program <- parseInput programP
    result <- runExercise "Part 1" (runProgram untilLoop) program
    case result of
        Success state -> do
            putStr "Finished"
            print $ accumulator state
        Loop state -> do
            putStr "Looped with value "
            print $ accumulator state
    mutationResult <- runExercise "Part 2" findTerminatingMutation program
    case mutationResult of
        Just state -> do
            putStr "Finished with value "
            print $ accumulator state
        Nothing ->
            putStr "No terminating program found."

findTerminatingMutation =
    getFirst
    . mconcat
    . map (First . isSuccess . runProgram untilLoop) 
    . mutations
    where
        isSuccess (Loop _) = Nothing
        isSuccess (Success state) = Just state