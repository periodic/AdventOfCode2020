{-# LANGUAGE TemplateHaskell, OverloadedStrings, ApplicativeDo #-}
module Main where

import Control.Applicative
import Control.Monad ( guard )
import Data.Char ( isDigit, isSpace )
import qualified Data.Either as E
import qualified Data.Attoparsec.Text as P
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import qualified Control.Lens.TH as LensTH
import qualified Control.Lens as Lens
import Text.Printf

import Exercise

data Passport = Passport
    { _birthYear :: Maybe Text
    , _issueYear :: Maybe Text
    , _expirationYear :: Maybe Text
    , _height :: Maybe Text
    , _hairColor :: Maybe Text
    , _eyeColor :: Maybe Text
    , _passportId :: Maybe Text
    , _countryId :: Maybe Text
    }
    deriving (Show)

LensTH.makeLenses ''Passport

emptyPassport :: Passport
emptyPassport =
    Passport
        { _birthYear = Nothing
        , _issueYear = Nothing
        , _expirationYear = Nothing
        , _height = Nothing
        , _hairColor = Nothing
        , _eyeColor = Nothing
        , _passportId = Nothing
        , _countryId = Nothing
        }

isValidPassport :: Passport -> Bool
isValidPassport Passport
    { _birthYear = Just _
    , _issueYear = Just _
    , _expirationYear = Just _
    , _height = Just _
    , _hairColor = Just _
    , _eyeColor = Just _
    , _passportId = Just _
    } = True
isValidPassport _ = False

validateValue :: P.Parser a -> (a -> Bool) -> Text -> Bool
validateValue parser validator =
    E.isRight . (>>= guard . validator) . P.parseOnly (parser <* P.endOfInput)

isValidYear :: Int -> Int -> Text -> Bool
isValidYear min max =
    validateValue P.decimal (\year -> year >= min && year <= max)

isValidHeight :: Text -> Bool
isValidHeight =
    validateValue
        ((,) <$> P.decimal <*> (P.choice . map P.string $ ["in", "cm"]))
        validateMeasurement
    where
        validateMeasurement (height, "in") =
            height >= 59 && height <= 76
        validateMeasurement (height, "cm") =
            height >= 150 && height <= 193

isValidColor :: Text -> Bool
isValidColor =
    validateValue parser validator
    where
        parser =
            P.char '#'
            *> (T.pack <$> P.count 6 (P.satisfy (P.inClass "0-9a-f")))
        validator =
            const True

isValidEyeColor :: Text -> Bool
isValidEyeColor = 
    validateValue P.takeText validator
    where
        validator =
            flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValidId :: Text -> Bool
isValidId =
    validateValue P.takeText validator
    where
        validator value =
            T.length value == 9 && T.all isDigit value

valueParser :: Text -> Lens.ASetter Passport Passport a1 (Maybe Text) -> (Text -> Bool) -> P.Parser (Passport -> Passport)
valueParser key setter isValid =
    validate
    <$> (P.string key
        *> P.char ':'
        *> P.takeTill isSpace
        )
    where
        validate text =
            if isValid text
                then Lens.set setter . Just $ text
                else id

field :: P.Parser (Passport -> Passport)
field =
    valueParser "byr" birthYear (isValidYear 1920 2002)
    <|> valueParser "iyr" issueYear (isValidYear 2010 2020)
    <|> valueParser "eyr" expirationYear (isValidYear 2020 2030)
    <|> valueParser "hgt" height isValidHeight    
    <|> valueParser "hcl" hairColor isValidColor
    <|> valueParser "ecl" eyeColor isValidEyeColor
    <|> valueParser "pid" passportId isValidId
    <|> valueParser "cid" countryId (const True)

fields :: P.Parser (Passport -> Passport)
fields =
    foldr (.) id
    <$> P.sepBy1 field P.space

passport :: P.Parser Passport
passport =
    fields <*> pure emptyPassport

passportsParser :: P.Parser [Passport]
passportsParser =
    P.sepBy passport (P.endOfLine *> P.endOfLine)

main :: IO ()
main = do
    passports <- parseInput passportsParser
    result <- runExercise "Part 1" (length . filter isValidPassport) passports
    printf "Number of valid passports: %d\n" result
