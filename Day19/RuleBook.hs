module RuleBook where

import Control.Applicative
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Data.Attoparsec.Text
import Data.Foldable (asum)
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

newtype RuleId = RuleId Int
  deriving (Show, Eq, Ord, Num)

data Rule
  = RuleChar Char
  | RuleAlternative [[RuleId]]
  deriving (Show)

newtype RuleBook = RuleBook {getRules :: IntMap Rule}
  deriving (Show)

makeRuleBook :: [(RuleId, Rule)] -> RuleBook
makeRuleBook =
  RuleBook . M.fromList . map (\(RuleId i, rule) -> (i, rule))

setRule :: RuleId -> Rule -> RuleBook -> RuleBook
setRule (RuleId ruleId) rule =
  RuleBook . M.insert ruleId rule . getRules

--------------------------------------------------
-- Parsing
--------------------------------------------------

ruleIdP :: Parser RuleId
ruleIdP =
  RuleId <$> decimal

ruleCharP :: Parser Rule
ruleCharP =
  RuleChar
    <$> ( char '"'
            *> anyChar
            <* char '"'
        )

ruleAlternativeP :: Parser Rule
ruleAlternativeP =
  RuleAlternative
    <$> (ruleIdP `sepBy` char ' ' `sepBy` (char ' ' *> char '|' *> char ' '))

ruleP :: Parser (RuleId, Rule)
ruleP =
  (,)
    <$> ruleIdP
    <* char ':'
    <* char ' '
    <*> (ruleCharP <|> ruleAlternativeP)

ruleBookP =
  makeRuleBook <$> (ruleP `sepBy` endOfLine)

--------------------------------------------------
-- Validation
--------------------------------------------------

newtype RuleValidator a = RuleValidator
  { runValidator :: ReaderT RuleBook (StateT String []) a
  }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadState String, MonadReader RuleBook)

validate :: RuleBook -> String -> Bool
validate ruleBook input =
  not . null . flip evalStateT input . flip runReaderT ruleBook . runValidator $ validateRule startRule *> validateEndOfInput

debug :: RuleBook -> String -> RuleValidator a -> [(a, String)]
debug ruleBook input =
  flip runStateT input . flip runReaderT ruleBook . runValidator

getRule :: RuleId -> RuleValidator Rule
getRule (RuleId i) =
  asks $ (! i) . getRules

startRule :: RuleId
startRule = RuleId 0

validateChar :: Char -> RuleValidator ()
validateChar target = do
  input <- get
  case input of
    [] ->
      mzero
    (c : cs) ->
      if c == target
        then put cs
        else mzero

validateRule :: RuleId -> RuleValidator ()
validateRule ruleId = do
  rule <- getRule ruleId
  case rule of
    RuleChar c ->
      validateChar c
    RuleAlternative ruleLists ->
      choice . reverse . map validateRuleList $ ruleLists

validateRuleList :: [RuleId] -> RuleValidator ()
validateRuleList =
  foldr1 (*>) . map validateRule

validateEndOfInput :: RuleValidator ()
validateEndOfInput = do
  input <- get
  guard $ null input