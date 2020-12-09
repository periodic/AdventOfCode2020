module Main where

import Data.Monoid
import System.Environment ( getArgs )
import qualified Data.Text.IO as TextIO

import Program
import Parser
import Evaluator

main :: IO ()
main = do
    [path] <- getArgs
    contents <- TextIO.readFile path
    case parseProgram contents of
        Left err -> do
            putStr "Unable to parse program"
            putStrLn err
        Right program -> do
            putStr "First run: "
            case runProgram untilLoop $ program of
                Success state -> do
                    putStr "Finished"
                    print $ accumulator state
                Loop state -> do
                    putStr "Looped with value "
                    print $ accumulator state
            putStr "Finding best mutation: "
            case findTerminatingMutation program of
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