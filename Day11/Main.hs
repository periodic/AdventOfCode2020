module Main where

import Common
import Data.Attoparsec.Text as P

import Life

main = do
  input <- loadAndParseInput (makeGrid <$> P.takeText)
  print "Hello"
