module Main where

import Data.IntMap.Strict as M
import Data.List as L
import Exercise
import Text.Printf
import Data.Array.ST
import Data.Array
import Control.Monad.ST
import Data.Int
import Control.Monad.State.Strict
import Control.Monad.Reader

newtype History s = History {unwrapHistory :: STUArray s Int32 Int32 }

data MemoryState = MemoryState
  { currNumber :: !Int32,
    currTurn  :: !Int32
  }
  deriving (Show)

newtype WordGame s a = WordGame {
  runWordGame :: ReaderT (History s) (StateT MemoryState (ST s)) a
} deriving (Functor, Applicative, Monad, MonadState MemoryState, MonadReader (History s))

liftST :: ST s a -> WordGame s a
liftST = WordGame . lift . lift

rememberNumber :: forall s. Int32 -> Int32 -> WordGame s ()
rememberNumber num t = do
  history <- ask
  liftST $ writeArray (unwrapHistory history) num t

recallNumber :: forall s. Int32 -> WordGame s Int32
recallNumber num = do
  history <- ask
  liftST $ readArray (unwrapHistory history) num

sayNext :: forall s. WordGame s ()
sayNext = do
  MemoryState{currNumber, currTurn } <- get
  lastSpoken <- recallNumber currNumber
  let nextNumber =
        if lastSpoken >= 0 
          then currTurn - lastSpoken 
          else 0
      newTurn = currTurn  + 1
  rememberNumber currNumber currTurn
  put $ MemoryState nextNumber newTurn

playGame :: forall s. [Int32] -> Int32 -> MemoryState
playGame numbers maxInt32 =
  let prevNumbers = tail . reverse . flip zip [1 ..] $ numbers
      currTurn = fromIntegral . L.length $ numbers
      currNumber = L.last numbers
      state = MemoryState {currTurn , currNumber}
      turnsLeft = maxInt32 - currTurn 
      game :: forall s. WordGame s ()
      game = do
        forM_ prevNumbers $ uncurry rememberNumber 
        replicateM_ (fromIntegral turnsLeft) sayNext
      stActions :: forall s. ST s MemoryState
      stActions = do
        history <- History <$> newArray (0,maxInt32) (-1)
        (runWordGame game `runReaderT` history) `execStateT` state
  in runST stActions

initialNumbers :: [Int32]
initialNumbers = [16, 1, 0, 18, 12, 14, 19]

partOne :: IO ()
partOne = do
  result <- runExercise "Part 1" (currNumber . playGame initialNumbers) (2020)
  printf "2020th turn starting with %s: %d\n" (show initialNumbers) result

partTwo :: IO ()
partTwo = do
  result <- runExercise "Part 1" (currNumber . playGame initialNumbers) (30000000)
  printf "30,000,000th turn starting with %s: %d\n" (show initialNumbers) result

main :: IO ()
main = do
  partOne
  partTwo
