{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import Data.Functor
import Data.Text (Text)

import Program

instructionType =
    (string "nop" $> NoOp)
    <|> (string "acc" $> IncAccum)
    <|> (string "jmp" $> Jump)

instructionArg =
    signed decimal

instruction =
    instructionType
    <* skipSpace
    <*> instructionArg

program =
    makeProgram
    <$> sepBy instruction endOfLine

parseProgram :: Text -> Either String Program
parseProgram =
    parseOnly (program <* skipSpace <* endOfInput)