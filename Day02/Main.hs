module Main where

import System.Environment
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import Data.Char (isAlpha)

import Exercise

data Password = Password Int Int Char T.Text
    deriving (Show)
    
main = do
    passwords <- parseInput parsePasswords
    result1 <- runExercise "Part 1" (length . filter isValid) passwords
    print result1
    result2 <- runExercise "Part 2" (length . filter isReallyValid) passwords
    print result2

parsePasswords :: P.Parser [Password]
parsePasswords =
    P.sepBy password P.endOfLine
    where
        password =
            Password
                <$> P.decimal
                <*  P.char '-'
                <*> P.decimal
                <*  P.skipSpace
                <*> P.letter
                <*  P.char ':'
                <*  P.skipSpace
                <*> P.takeWhile isAlpha

isValid :: Password -> Bool
isValid (Password min max char password) =
    let
        c = T.count (T.singleton char) password
    in
        c <= max && c >= min

isReallyValid :: Password -> Bool
isReallyValid (Password a b char password) =
    let
        charA = T.index password (a - 1)
        charB = T.index password (b - 1)
        validA = charA == char
        validB = charB == char
    in
        (validA && not validB) || (not validA && validB)
