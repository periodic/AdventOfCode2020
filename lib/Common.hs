module Common where

import System.Environment ( getArgs )
import System.Exit
import qualified Data.Attoparsec.Text as P
import Data.Text
import qualified Data.Text.IO as TextIO

loadInput :: IO Text
loadInput = do
    [path] <- getArgs
    TextIO.readFile path

loadAndParseInput :: P.Parser a -> IO a
loadAndParseInput parser = do
    contents <- loadInput
    let result = P.parseOnly parser contents
    case result of 
        Left err -> do
            putStr "Failed to parse input"
            putStrLn err
            exitFailure
        Right value ->
            return value