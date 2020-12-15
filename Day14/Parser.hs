{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import Types

bitValue :: Parser MaskValue
bitValue =
  string "1" $> MaskOne
    <|> string "0" $> MaskZero
    <|> string "X" $> MaskUnset

maskInstruction :: Parser Instruction
maskInstruction =
  SetMask
    <$> ( string "mask = "
            *> many1 bitValue
        )

memLocation :: Parser MemLocation
memLocation =
  MemLocation
    <$> decimal

storeInstruction :: Parser Instruction
storeInstruction =
  Store
    <$> ( string "mem["
            *> memLocation
        )
    <* string "] = "
    <*> decimal

instruction :: Parser Instruction
instruction =
  maskInstruction <|> storeInstruction

program :: Parser Program
program =
  Program <$> sepBy instruction endOfLine
