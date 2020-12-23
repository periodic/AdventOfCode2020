module Main where

import Common
import Constraints
import Data.Attoparsec.Text as P
import Data.Char as C (isAlpha)
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

data Food = Food
  { ingredients :: Set Text,
    allergens :: Set Text
  }
  deriving (Show)

--------------------------------------------------
-- Parsing
--------------------------------------------------

ingredientP :: Parser Text
ingredientP =
  takeWhile1 isAlpha

allergenP :: Parser Text
allergenP =
  takeWhile1 isAlpha

allergenListP :: Parser (Set Text)
allergenListP =
  S.fromList
    <$> ( "(contains "
            *> allergenP `sepBy1` ", "
            <* ")"
        )

ingredientListP =
  S.fromList
    <$> ingredientP `sepBy1` " "

foodP =
  Food
    <$> ingredientListP
    <* skipSpace
    <*> allergenListP

foodListP =
  foodP `sepBy` endOfLine

--------------------------------------------------
-- Part One
--------------------------------------------------

newtype AllergenInfo = AllergenInfo
  { toMap :: Map Text (Set Text)
  }
  deriving (Show)

allergenPossibilitiesMap :: [Food] -> AllergenInfo
allergenPossibilitiesMap =
  foldr addFoodToInfo (AllergenInfo M.empty)

addFoodToInfo :: Food -> AllergenInfo -> AllergenInfo
addFoodToInfo Food {ingredients, allergens} allergenInfo =
  AllergenInfo . S.foldr updateAllergen (toMap allergenInfo) $ allergens
  where
    updateAllergen :: Text -> Map Text (Set Text) -> Map Text (Set Text)
    updateAllergen =
      M.alter
        ( \case
            Nothing ->
              Just ingredients
            Just prevIngredients ->
              Just $ S.intersection ingredients prevIngredients
        )

allIngredients :: [Food] -> Set Text
allIngredients =
  foldr (S.union . ingredients) S.empty

partOne :: [Food] -> IO AllergenInfo
partOne foods =
  let allergenInfo = allergenPossibilitiesMap foods
      nonAllergenIngredients =
        M.foldr (flip S.difference) (allIngredients foods) . toMap $ allergenInfo
      numAppearances =
        foldr (\food n -> n + S.size (ingredients food `S.intersection` nonAllergenIngredients)) 0 foods
   in do
        putStrLn "Foods without allergens: "
        putStrLn . T.unpack . T.intercalate ", " . S.toList $ nonAllergenIngredients
        putStrLn "Number of occurrences: "
        print numAppearances
        return allergenInfo

--------------------------------------------------
-- Part One
--------------------------------------------------

partTwo :: AllergenInfo -> IO ()
partTwo allergenInfo =
  let maybeSolution = solve . toMap $ allergenInfo
   in case maybeSolution of
        Nothing ->
          putStrLn "No solution found."
        Just solution -> do
          print solution
          putStrLn "Alphebetized allergic ingredients: "
          putStrLn . T.unpack . T.intercalate "," . M.elems $ solution

--------------------------------------------------
-- Main
--------------------------------------------------

main :: IO ()
main = do
  foods <- loadAndParseInput foodListP
  allergenInfo <- partOne foods
  partTwo allergenInfo