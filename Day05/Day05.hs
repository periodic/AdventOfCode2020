module Main where

import Data.List (sort)
import System.Environment (getArgs)

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let seatIds = map parseSeatId . lines $ contents
    putStr "Maximum seat ID: "
    print $ maximum seatIds
    putStr "Missing seat ID: "
    print $ findMissingSeat seatIds


findMissingSeat :: [Int] -> Either Int Int
findMissingSeat =
    foldr1 isMissing . map Left . sort
    where
        isMissing (Left seatId) (Left prev) =
            if seatId == prev - 1
                then Left seatId
                else Right (seatId + 1)
        isMissing _ found@(Right _) =
            found
        isMissing _ _ =
            error "How did the list contain a Right?"




parseSeatId :: String -> Int
parseSeatId =
    foldl addDigit 0
    where
        addDigit num 'B' = num * 2 + 1
        addDigit num 'R' = num * 2 + 1
        addDigit num  _  = num * 2