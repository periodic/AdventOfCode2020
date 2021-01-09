module Main where

import Data.Function
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Exercise (parseInput, runExercise)
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Data.IntSet as S
import qualified Data.IntMap.Strict as M 

newtype MultiSet = MultiSet {
    toMap :: M.IntMap Int
}

empty :: MultiSet
empty =
    MultiSet M.empty

insert :: Int -> MultiSet -> MultiSet
insert k =
    MultiSet . M.alter (Just . maybe 1 (+1)) k . toMap

remove :: Int -> MultiSet -> MultiSet
remove k =
    MultiSet . M.update (\n -> if n > 1 then Just (n - 1) else Nothing) k . toMap

member :: Int -> MultiSet -> Bool
member k =
    M.member k . toMap

fromList :: [Int] -> MultiSet
fromList =
    foldr insert empty

toList :: MultiSet -> [Int]
toList =
    M.keys . toMap

main :: IO ()
main = do
  numbers <- parseInput xmasInput
  result1 <- runExercise "Part 1" findInvalid numbers
  case result1 of
    Nothing ->
      putStrLn "No invalid numbers found."
    Just invalidNumber -> do
      printf "Invalid number: %d\n" invalidNumber
      result2 <- runExercise "Part 2" (findContiguousSubsetSum invalidNumber) numbers
      printf "Sum of smallest and largest: %s\n" . show $ V.minimum result2 + V.maximum result2

xmasInput :: P.Parser (Vector Int)
xmasInput =
  V.fromList <$> P.sepBy P.decimal P.endOfLine

findInvalid :: Vector Int -> Maybe Int
findInvalid values =
    findInvalidR 25 (fromList . V.toList . V.slice 0 25 $ values)
    where
        hasSum target set =
             foldl (\prev a -> prev || (a + a /= target && (target - a) `member` set)) False . toList $ set
        findInvalidR index set =
            if index >= V.length values
                then Nothing
                else
                    let curr = values ! index
                        valid = hasSum curr set
                    in if not valid
                            then Just curr
                            else findInvalidR (index + 1) (insert curr set & remove (values ! (index - 25)))


findContiguousSubsetSum :: Int -> Vector Int -> Vector Int
findContiguousSubsetSum target values =
  findSum 0 1 (values ! 0 + values ! 1)
  where
    findSum start end subsetSum =
       case compare subsetSum target of
        EQ ->
            V.slice start (end - start + 1) values
        LT ->
            findSum start (end + 1) (subsetSum + (values ! (end + 1)))
        GT ->
            findSum (start + 1) end (subsetSum - (values ! start))
