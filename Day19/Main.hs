module Main where

import Common
import Data.Attoparsec.Text as P
import qualified Data.Char as C
import qualified Data.Text as T
import RuleBook

inputP =
  (,)
    <$> ruleBookP
    <* endOfLine
    <* endOfLine
    <*> ((T.unpack <$> takeWhile1 C.isAlpha) `sepBy` endOfLine)

main = do
  input <- loadAndParseInput inputP
  partOne input
  partTwo input

partOne (ruleBook, messages) = do
  putStr "Valid messages: "
  print . length . filter (validate ruleBook) $ messages

partTwo (ruleBook, messages) =
  let updatedRuleBook =
        setRule 8 (RuleAlternative [[42], [42, 8]])
          . setRule 11 (RuleAlternative [[42, 31], [42, 11, 31]])
          $ ruleBook
   in do
        putStr "Valid messages (fixed rules): "
        print . length . filter (validate updatedRuleBook) $ messages