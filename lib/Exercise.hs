module Exercise (runExercise, readInput, parseInput) where

import Control.Monad
import Options.Applicative
import System.Exit
import qualified Data.Attoparsec.Text as Attoparsec
import System.CPUTime
import Text.Printf
import Data.Text (Text)
import Data.Text.IO ( readFile )
import Criterion

-- TODO: Clean up this file.

inputFileArg :: Parser String
inputFileArg =
  strArgument (metavar "INPUT_FILE")

benchmarkArg :: Parser Bool
benchmarkArg =
  switch (long "benchmark" <> short 'b' <> help "Enable detailed benchmarking")

argsParser :: Parser Arguments
argsParser =
  Arguments <$> inputFileArg <*> benchmarkArg

argsInfo :: ParserInfo Arguments
argsInfo =
  info argsParser mempty

data Arguments = Arguments {
  inputFile :: String,
  benchmarking :: Bool
} deriving Show

parseArgs :: IO Arguments
parseArgs =
  execParser argsInfo

readInput :: IO Text
readInput = do
  args <- parseArgs
  Data.Text.IO.readFile . inputFile $ args

parseInput :: Attoparsec.Parser a -> IO a
parseInput parser = do
  args <- parseArgs
  printf "Parsing input...\n"
  contents <- readInput
  doParsing args parser contents

runExercise :: String -> (a -> b) -> a -> IO b
runExercise name work input = do
  putStrLn . replicate 80 $ '='
  printf "Running %s...\n" name
  args <- parseArgs
  if benchmarking args
    then do
      benchmark $ whnf work input
      return . work $ input
    else time "Work" . work $ input

time :: String -> a -> IO a
time name work = do
  start <- getCPUTime 
  end <- work `seq` getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ 9) :: Double
  printf "%s took %0.3fms\n" name diff
  return work

doParsing :: Arguments -> Attoparsec.Parser a -> Text -> IO a
doParsing args parser contents = do
  start <- getCPUTime 
  case Attoparsec.parseOnly parser contents of
    Left err -> do
      printf "Failed to parse input: %s\n" err
      exitFailure
    Right input -> do
      end <- input `seq` getCPUTime 
      let diff = fromIntegral (end - start) / (10 ^ 9) :: Double
      if benchmarking args
        then benchmark $ whnf (Attoparsec.parseOnly parser) contents
        else printf "Parsing took %0.3fms\n" diff
      return input
      
