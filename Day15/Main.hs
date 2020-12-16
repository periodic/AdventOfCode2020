module Main where

import           Data.IntMap.Strict  as M
import           Data.List           as L

newtype Time = Time { untime :: Int }
  deriving (Show, Eq, Num, Ord)

newtype History = History { unwrapHistory :: IntMap Time }
  deriving (Show)

data MemoryState = MemoryState
  { history    :: !History
  , lastNumber :: !Int
  , time       :: !Time
  } deriving Show

initialNumbers :: [Int]
initialNumbers = [16,1,0,18,12,14,19]

rememberNumber :: Int -> Time -> History -> History
rememberNumber num t =
  History . M.insert num t . unwrapHistory

deltaOf :: Int -> Time -> History -> Time
deltaOf num t =
  maybe (Time num) (t -) . M.lookup num . unwrapHistory

sayNext :: MemoryState -> MemoryState
sayNext MemoryState{history, lastNumber, time} =
  let
    nextNumber =
      if M.member lastNumber (unwrapHistory history)
        then untime $ deltaOf lastNumber time history
        else 0
    newTime = time + Time 1
    newHistory = rememberNumber lastNumber time history
  in
    MemoryState { history = newHistory, lastNumber = nextNumber, time = newTime }

startGame :: [Int] -> MemoryState
startGame numbers =
  let
    history = History . M.fromList . tail . L.reverse . flip zip (L.map Time [1..]) $ numbers
    time = Time . L.length $ numbers
    lastNumber = L.last numbers
  in
    MemoryState { history, time, lastNumber }

untilTurn :: Time -> MemoryState -> MemoryState
untilTurn targetTime !state =
  if targetTime <= time state
    then state
    else untilTurn targetTime . sayNext $ state

partOne :: IO ()
partOne = do
  putStr "2020th turn starting with "
  putStr . show $ initialNumbers
  putStr ": "
  print . lastNumber . untilTurn (Time 2020) . startGame $ initialNumbers

partTwo :: IO ()
partTwo = do
  putStr "30,000,000th turn starting with "
  putStr . show $ initialNumbers
  putStr ": "
  print . lastNumber . untilTurn (Time 3000000) . startGame $ initialNumbers

main :: IO ()
main = do
  partOne
  partTwo
