{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common
import Control.Applicative
import Data.Attoparsec.Text as P
import Data.Functor (($>))
import Data.List as L
import Data.Maybe as Maybe

data BusInfo = BusInfo Integer [Maybe Integer]
  deriving (Show)

-- Parsing stuff

startTime :: Parser Integer
startTime = P.decimal

busNumber :: Parser Integer
busNumber = P.decimal

outOfService :: Parser ()
outOfService = P.string "x" $> ()

busses :: Parser [Maybe Integer]
busses =
  P.sepBy
    ( Just <$> busNumber
        <|> outOfService $> Nothing
    )
    (P.string ",")

input :: Parser BusInfo
input =
  BusInfo
    <$> startTime
    <* P.endOfLine
    <*> busses

-- Calculations

waitTime :: Integer -> Integer -> Integer
waitTime startTime bus =
  let missedBy = startTime `mod` bus
   in bus - missedBy

leastWait :: Integer -> [Integer] -> (Integer, Integer)
leastWait startTime busses =
  L.minimumBy (\a b -> snd a `compare` snd b) . zip busses . map (waitTime startTime) $ busses


{- Goal is to find the least number n such that for all bus (i, m)
 -   m * k + i = n
 - for some positive integer k, where i is the index in the bus list and m is
 - it's number.
 - Notice that we can find times for any pair, then advance the clock by the
 - product of those two to find more times that also work, generating a new
 - pair to use iteratively.
 -}
timeOfSequentialDepartures :: [Maybe Integer] -> Integer
timeOfSequentialDepartures maybeBusses =
  let
    bussesWithOffsets = foldr (\(maybeBus, i) busses ->
      case maybeBus of
        Just bus ->
          (bus, i) : busses
        Nothing ->
          busses) []  $ zip maybeBusses [0..]
    firstTime = fst . head $ bussesWithOffsets
  in
    fst . foldl timeOfSequentialPair (0, firstTime) . tail $ bussesWithOffsets

timeOfSequentialPair :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
timeOfSequentialPair (t, dt) (b, offset) =
  let
    newT = head . dropWhile (\t -> (t + offset) `mod` b /= 0) . iterate (+dt) $ t
    newDT = dt * b
  in
    (newT, newDT)

-- Part 1

calculateAndPrintNextBus :: BusInfo -> IO ()
calculateAndPrintNextBus (BusInfo startTime busses) = do
  let (nextBus, wait) = leastWait startTime $ Maybe.catMaybes busses
  putStr "Next bus is "
  putStr $ show nextBus
  putStr " in "
  print wait
  putStr "Product of those is "
  print $ nextBus * wait

-- Part 2

calculateAndPrintSequentialDepartures :: BusInfo -> IO ()
calculateAndPrintSequentialDepartures (BusInfo _ busses) = do
  let time = timeOfSequentialDepartures busses
  putStr "First time of sequential departures: "
  print time



-- Main

main :: IO ()
main = do
  busInfo <- loadAndParseInput input
  calculateAndPrintNextBus busInfo
  calculateAndPrintSequentialDepartures busInfo