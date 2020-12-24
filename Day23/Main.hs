module Main where

import Data.Array.ST
import Data.Array.Unboxed
import Data.Maybe
import qualified Data.List as L
import Control.Monad.ST
import Control.Monad

data Cups = Cups
  { currentCup :: Int,
    toMap :: UArray Int Int
  }

data STCups s = STCups Int (STUArray s Int Int)

instance Show Cups where
  show (Cups curr cupMap) =
    let (min, max) = bounds cupMap
    in showFrom curr (max - min + 1)
    where
      showFrom curr 0 = ""
      showFrom curr n =
        show curr ++ showFrom (cupMap ! curr) (n - 1)

move :: forall s. STCups s -> ST s (STCups s)
move (STCups currCup cupMap) = do
  (min, max) <- getBounds cupMap
  oneCupDown <- readArray cupMap currCup
  twoCupsDown <- readArray cupMap oneCupDown
  threeCupsDown <- readArray cupMap twoCupsDown
  fourCupsDown <- readArray cupMap threeCupsDown
  let findInsertionIndex n
        | n <= 0 = findInsertionIndex max
        | otherwise =
          if n `elem` [oneCupDown, twoCupsDown, threeCupsDown]
            then findInsertionIndex (n - 1)
            else n
      insertAfter = findInsertionIndex (currCup - 1)
  afterInsert <- readArray cupMap insertAfter
  writeArray cupMap currCup fourCupsDown
  writeArray cupMap insertAfter oneCupDown
  writeArray cupMap threeCupsDown afterInsert
  return $ STCups fourCupsDown cupMap 

freezeCups :: forall s. STCups s -> ST s Cups
freezeCups (STCups currCup cupMap) = do
  frozenCupMap <- freeze cupMap
  return $ Cups currCup frozenCupMap

thawCups :: forall s. Cups -> ST s (STCups s)
thawCups (Cups currCup cupMap) = do
  mutableCupMap <- thaw cupMap
  return $ STCups currCup mutableCupMap

moveN :: forall s. Int -> Cups -> Cups
moveN n cups = runST $ do
  stCups <- thawCups cups
  (foldr1 (>=>) . replicate n $ move) stCups
  freezeCups stCups

fromList :: [Int] -> Cups
fromList list =
  Cups (head list) . listArray (1, length list). map snd . L.sort . zip list . tail . cycle $ list

exampleCups :: Cups
exampleCups =
  fromList [3, 8, 9, 1, 2, 5, 4, 6, 7]

input :: Cups
input =
  fromList [7, 9, 2, 8, 4, 5, 1, 3, 6]

setCurrentCup :: Int -> Cups -> Cups
setCurrentCup currCup (Cups _ cupMap) =
  Cups currCup cupMap

partOne :: IO ()
partOne =
  let
   in do
        putStr "Test Input: "
        print exampleCups
        putStrLn "After 10 moves: "
        print . setCurrentCup 1 . moveN 10 $ exampleCups
        putStrLn "After 100 moves: "
        print . setCurrentCup 1 . moveN 100 $ exampleCups
        putStrLn "Part 1 Input: "
        print input
        putStrLn "After 10 moves: "
        print . setCurrentCup 1 . moveN 10 $ input
        putStrLn "After 100 moves: "
        print . setCurrentCup 1 . moveN 100 $ input

partTwo :: IO ()
partTwo =
  let inputAsList = [7, 9, 2, 8, 4, 5, 1, 3, 6] ++ [10 .. 1000000]
      cups = fromList inputAsList
      finalArrangement = moveN 10000000 cups
      starCup1 = toMap finalArrangement ! 1
      starCup2 = toMap finalArrangement ! starCup1
   in do
        putStrLn "Cups with stars: "
        print starCup1
        print starCup2
        putStr "Product of star cups: "
        print $ starCup1 * starCup2

main :: IO ()
main = do
  partOne
  partTwo
