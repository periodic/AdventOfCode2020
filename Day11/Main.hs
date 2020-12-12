{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common
import Data.Attoparsec.Text as P
import qualified Data.Text as T

import Life

testGrid :: Grid
testGrid = makeGrid . T.intercalate "\n" $
  [ "L.LL.LL.LL"
  , "LLLLLLL.LL"
  , "L.L.L..L.."
  , "LLLL.LL.LL"
  , "L.LL.LL.LL"
  , "L.LLLLL.LL"
  , "..L.L....."
  , "LLLLLLLLLL"
  , "L.LLLLLL.L"
  , "L.LLLLL.LL"
  ]

main = do
  grid <- loadAndParseInput (makeGrid <$> P.takeText)
  putStr "Number of occupied seats after settling (Adjacent): "
  print . numOccupied . runUntilSettled simpleRules $ grid
  putStr "Number of occupied seats after settling (LOS): "
  print . numOccupied . runUntilSettled losRules $ grid
