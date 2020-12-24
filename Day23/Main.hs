module Main where

import Data.IntMap.Strict ((!))
import qualified Data.IntMap.Strict as Map
import Data.Maybe

data Cups = Cups
  { currentCup :: Int,
    toMap :: Map.IntMap Int
  }

instance Show Cups where
  show (Cups curr cupMap) =
    showFrom curr (Map.size cupMap)
    where
      showFrom curr 0 = ""
      showFrom curr n =
        case Map.lookup curr cupMap of
          Nothing ->
            show curr
          Just next ->
            show curr ++ showFrom next (n - 1)

move :: Cups -> Cups
move (Cups currCup cupMap) =
  let oneCupDown = cupMap ! currCup
      twoCupsDown = cupMap ! oneCupDown
      threeCupsDown = cupMap ! twoCupsDown
      fourCupsDown = cupMap ! threeCupsDown
      findInsertionIndex n
        | n <= 0 = findInsertionIndex $ Map.size cupMap
        | otherwise =
          if n `elem` [oneCupDown, twoCupsDown, threeCupsDown]
            then findInsertionIndex (n - 1)
            else n
      insertAfter = findInsertionIndex (currCup - 1)
      updatedMap =
        Map.insert insertAfter oneCupDown
          . Map.insert threeCupsDown (cupMap ! insertAfter)
          . Map.insert currCup fourCupsDown
          $ cupMap
   in Cups fourCupsDown updatedMap

moveN :: Int -> Cups -> Cups
moveN n =
  foldr1 (.) . replicate n $ move

fromList :: [Int] -> Cups
fromList list =
  Cups (head list) . Map.fromList . zip list . tail . cycle $ list

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
