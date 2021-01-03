{-# LANGUAGE TemplateHaskell, OverloadedStrings, ApplicativeDo #-}
module Main where

import Control.Applicative
import Control.Monad ( guard )
import Data.Char ( isSpace )
import qualified Data.Either as E
import Data.Attoparsec.Text
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import qualified Control.Lens.TH as LensTH
import qualified Control.Lens as Lens
import Text.Printf
import Data.Function

import Exercise

-- | Encodes the trinary state of a parse result.
-- We want to keep track of if something is present or not, not just if it is
-- valid.
data Value a
    = Missing
    | Invalid !Text
    | Valid !a
    deriving Show

data Height = Cm !Int | Inches !Int
    deriving Show

data Passport = Passport
    { _birthYear :: !(Value Int)
    , _issueYear :: !(Value Int)
    , _expirationYear :: !(Value Int)
    , _height :: !(Value Height)
    , _hairColor :: !(Value Text)
    , _eyeColor :: !(Value Text)
    , _passportId :: !(Value Text)
    , _countryId :: !(Value Text)
    }
    deriving Show

LensTH.makeLenses ''Passport

emptyPassport :: Passport
emptyPassport =
    Passport
        { _birthYear = Missing
        , _issueYear = Missing
        , _expirationYear = Missing
        , _height = Missing
        , _hairColor = Missing
        , _eyeColor = Missing
        , _passportId = Missing
        , _countryId = Missing
        }

isValidPassport :: Passport -> Bool
isValidPassport Passport
    { _birthYear = Valid _
    , _issueYear = Valid _
    , _expirationYear = Valid _
    , _height = Valid _
    , _hairColor = Valid _
    , _eyeColor = Valid _
    , _passportId = Valid _
    } = True
isValidPassport _ = False

hasRequiredFields :: Passport -> Bool
hasRequiredFields passport =
    all ($ passport)
        [ isPresent . _birthYear
        , isPresent . _issueYear
        , isPresent . _expirationYear
        , isPresent . _height
        , isPresent . _hairColor
        , isPresent . _eyeColor
        , isPresent . _passportId
        ]
    where
        isPresent Missing = False
        isPresent _ = True


yearP :: Int -> Int -> Parser Int
yearP min max = do
    year <- decimal
    guard (year >= min && year <= max)
    pure year

heightP :: Parser Height
heightP =
    heightInches <|> heightCm
    where
        heightInches = do
            height <- decimal
            "in"
            guard (height >= 59 && height <= 76)
            pure . Inches $ height
        heightCm = do
            height <- decimal
            "cm"
            guard (height >= 150 && height <= 193)
            pure . Cm $ height
            

colorP :: Parser Text
colorP =
        char '#'
        *> (T.pack <$> count 6 (satisfy (inClass "0-9a-f")))

eyeColorP :: Parser Text
eyeColorP = 
            choice ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

idP :: Parser Text
idP =
    T.pack <$> count 9 digit

word :: Parser Text
word =
    takeWhile1 (not . isSpace)

endOfWord :: Parser ()
endOfWord = do
    nextChar <- peekChar
    case nextChar of
        Nothing ->
            pure ()
        Just c ->
            if isSpace c
                then pure ()
                else mempty


valueParser :: Text -> Lens.ASetter Passport Passport a1 (Value b) -> Parser b -> Parser (Passport -> Passport)
valueParser key setter valueP =
    Lens.set setter
    <$> (string key
        *> char ':'
        *> (
            (Valid <$> valueP <* endOfWord)
            <|> (Invalid <$> word)
        )
    )

field :: Parser (Passport -> Passport)
field =
    valueParser "byr" birthYear (yearP 1920 2002)
    <|> valueParser "iyr" issueYear (yearP 2010 2020)
    <|> valueParser "eyr" expirationYear (yearP 2020 2030)
    <|> valueParser "hgt" height heightP    
    <|> valueParser "hcl" hairColor colorP
    <|> valueParser "ecl" eyeColor eyeColorP
    <|> valueParser "pid" passportId idP
    <|> valueParser "cid" countryId word

fields :: Parser (Passport -> Passport)
fields =
    foldr (.) id
    <$> sepBy1 field space

passport :: Parser Passport
passport =
    fields <*> pure emptyPassport

passportsParser :: Parser [Passport]
passportsParser =
    sepBy passport (endOfLine *> endOfLine)

main :: IO ()
main = do
    passports <- parseInput passportsParser
    printf "Total number of passports: %d\n" . length $ passports
    result1 <- runExercise "Part 1" (length . filter hasRequiredFields) passports
    printf "Number of passports with required fields: %d\n" result1
    result2 <- runExercise "Part 2" (length . filter isValidPassport) passports
    printf "Number of valid passports: %d\n" result2
