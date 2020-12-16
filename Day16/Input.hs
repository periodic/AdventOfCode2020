module Input where

import           Data.Attoparsec.Text as P
import           Data.Char
import           Data.Text

import           Range

data FieldRule = FieldRule
  { fieldName  :: Text
  , validRange :: Range
  } deriving (Show)

newtype Ticket = Ticket {
  fieldValues :: [Int]
} deriving (Show)

data Input = Input
  { fieldRules    :: [FieldRule]
  , yourTicket    :: Ticket
  , nearbyTickets :: [Ticket]
  } deriving (Show)


----------------------------------------
-- Parsing
----------------------------------------

ticket =
  Ticket
  <$> decimal `sepBy1` string ","

range =
  Range
  <$> decimal
  <*  string "-"
  <*> decimal

ranges =
  mconcat <$> range `sepBy1` string " or "

rule =
  FieldRule
  <$> takeWhile1 (\c -> isAlpha c || c == ' ')
  <*  char ':'
  <*  skipSpace
  <*> ranges

rules =
  rule `sepBy1` endOfLine

tickets =
  ticket `sepBy1` endOfLine

input =
  Input
  <$> rules
  <*  endOfLine
  <*  endOfLine
  <*  string "your ticket:"
  <*  endOfLine
  <*> ticket
  <*  endOfLine
  <*  endOfLine
  <*  string "nearby tickets:"
  <*  endOfLine
  <*> tickets
  <*  endOfLine
  <*  endOfInput
