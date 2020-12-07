{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Applicative
import Data.Attoparsec.Text as P
import Data.Char
import Data.Functor
import Data.Text (Text)

import Types

word :: Parser Text
word = P.takeWhile isAlpha

bag :: Parser Bag
bag =
    Bag
    <$> word
    <*  skipSpace
    <*> word
    <*  skipSpace
    <*  (string "bags" <|> string "bag")

bagCount :: Parser (Int, Bag)
bagCount =
    (,)
    <$> decimal
    <*  skipSpace
    <*> bag

noBags :: Parser [(Int, Bag)]
noBags =
    P.string "no other bags" $> []

bagCounts :: Parser [(Int, Bag)]
bagCounts =
    bagCount `sepBy1` (char ',' *> skipSpace)

bagRule :: Parser BagRule
bagRule =
    BagRule
    <$> bag
    <*  skipSpace
    <*  string "contain"
    <*  skipSpace
    <*> (bagCounts <|> noBags)
    <*  char '.'

bagRules :: Parser [BagRule]
bagRules =
    bagRule `sepBy` endOfLine


