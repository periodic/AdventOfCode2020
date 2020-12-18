module Main where

import Common
import Control.Applicative
import qualified Data.Attoparsec.Text as P

data Term
  = Parentheses Expression
  | Number Int

instance Show Term where
  show (Number x) = show x
  show (Parentheses e) =
    "(" ++ show e ++ ")"

data Expression
  = Add Term Expression
  | Multiply Term Expression
  | End Term

instance Show Expression where
  show (Multiply l r) =
    show l ++ " * " ++ show r
  show (Add l r) =
    show l ++ " + " ++ show r
  show (End t) =
    show t

--------------------------------------------------
-- Parsing
--------------------------------------------------

number :: P.Parser Term
number =
  Number
    <$> P.decimal

parentheses :: P.Parser Term
parentheses =
  Parentheses
    <$> ("("
    *> expression
    <* ")")

term :: P.Parser Term
term =
  number <|> parentheses

add :: P.Parser Expression
add =
  Add 
    <$> term
    <* P.skipSpace
    <* "+"
    <* P.skipSpace
    <*> expression

multiply :: P.Parser Expression
multiply =
  Multiply
    <$> term
    <* P.skipSpace
    <* "*"
    <* P.skipSpace
    <*> expression

end :: P.Parser Expression
end =
  End <$> term

expression :: P.Parser Expression
expression =
  add <|> multiply <|> end

homework :: P.Parser [Expression]
homework =
  expression `P.sepBy` P.endOfLine
    <* P.endOfLine
    <* P.endOfInput

--------------------------------------------------
-- Part One
--------------------------------------------------

evalExpressionLeft :: Expression -> Int
evalExpressionLeft (Add l r) = evalExpressionLeft $ reduceLeft evalTermLeft (+) l r
evalExpressionLeft (Multiply l r) = evalExpressionLeft $ reduceLeft evalTermLeft (*) l r
evalExpressionLeft (End t) = evalTermLeft t

reduceLeft :: (Term -> Int) -> (Int -> Int -> Int) -> Term -> Expression -> Expression
reduceLeft eval op x (Add l r) = Add (Number $ eval x `op` eval l) r
reduceLeft eval op x (Multiply l r) = Multiply (Number $ eval x `op` eval l) r
reduceLeft eval op x (End t) = End (Number $ eval x `op` eval t)

evalTermLeft :: Term -> Int
evalTermLeft (Number x) = x
evalTermLeft (Parentheses e) = evalExpressionLeft e

partOne :: [Expression] -> IO ()
partOne exprs = do
  putStr "Sum of all evaluated expressions (left-assoc): "
  print . sum . map evalExpressionLeft $ exprs

--------------------------------------------------
-- Part Two
--------------------------------------------------

evalExpressionAddFirst :: Expression -> Int
evalExpressionAddFirst (Add l r) = evalExpressionAddFirst $ reduceLeft evalTermAddFirst (+) l r
evalExpressionAddFirst (Multiply l r) = evalTermAddFirst l * evalExpressionAddFirst r
evalExpressionAddFirst (End t) = evalTermAddFirst t

evalTermAddFirst :: Term -> Int
evalTermAddFirst (Number x) = x
evalTermAddFirst (Parentheses e) = evalExpressionAddFirst e

partTwo :: [Expression] -> IO ()
partTwo exprs = do
  putStr "Sum of all evaluated expressions (addition first): "
  print . sum . map evalExpressionAddFirst $ exprs

--------------------------------------------------
-- Main
--------------------------------------------------
main :: IO ()
main = do
  hw <- loadAndParseInput homework
  partOne hw
  partTwo hw
