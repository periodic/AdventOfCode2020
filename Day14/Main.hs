{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List as L
import           Data.Text as T

import           Common
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
  prog <- loadAndParseInput program
  partOne prog
  partTwo prog

partOne :: Program -> IO ()
partOne program = do
  let memory = evalProgramV1 program
  putStr "Sum of memory on termination (V1): "
  -- 15514035145260
  print . sumMemory $ memory

partTwo :: Program -> IO ()
partTwo program = do
  let memory = evalProgramV2 program
  putStr "Sum of memory on termination (V2): "
  print . sumMemory $ memory
