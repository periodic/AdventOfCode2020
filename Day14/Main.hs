{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List as L
import           Data.Text as T
import Text.Printf

import           Exercise
import           Eval
import           Parser
import           Types

testProgram :: Text
testProgram = T.intercalate "\n"
  [ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
  , "mem[8] = 11"
  , "mem[7] = 101"
  , "mem[8] = 0"
  ]

main :: IO ()
main = do
  prog <- parseInput program
  partOne prog
  partTwo prog

partOne :: Program -> IO ()
partOne program = do
  total <- runExercise "Part 1" (sumMemory . evalProgramV1) program
  -- Result: 15514035145260
  printf "Sum of memory on termination (V1): %d\n" total

partTwo :: Program -> IO ()
partTwo program = do
  total <- runExercise "Part 2" (sumMemory . evalProgramV2) program
  -- Result: 3926790061594
  printf "Sum of memory on termination (V2): %d\n" total
