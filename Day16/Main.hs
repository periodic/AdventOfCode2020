module Main where

import Exercise
import Control.Monad
import qualified Data.List as L
import Data.Monoid
import qualified Data.Text as T
import Input
import Range
import Text.Printf

main = do
  input <- parseInput Input.input
  partOne input
  partTwo input

----------------------------------------
-- Part One
----------------------------------------

combineRuleRanges :: Input -> Range
combineRuleRanges Input {fieldRules} =
  mconcat . map validRange $ fieldRules

extractInvalidValues :: Input -> [Int]
extractInvalidValues input@Input {fieldRules, nearbyTickets} =
  let combinedRange = combineRuleRanges input
   in concatMap (filter (not . (`inRange` combinedRange)) . fieldValues) nearbyTickets

partOne :: Input -> IO ()
partOne input = do
  errorRate <- runExercise "Part 1" (sum . extractInvalidValues) input
  printf "Ticket scanning error rate: %d\n" errorRate

----------------------------------------
-- Part Two
----------------------------------------

isValid :: Input -> Ticket -> Bool
isValid input Ticket {fieldValues} =
  let universalRange = combineRuleRanges input
   in all (`inRange` universalRange) fieldValues

validTicketsByField :: Input -> [(Int, [Int])]
validTicketsByField input =
  zip [0 ..]
    . L.transpose
    . (fieldValues (yourTicket input) :)
    . map fieldValues
    . filter (isValid input)
    . nearbyTickets
    $ input

fieldsMatchingRule :: [(Int, [Int])] -> FieldRule -> [Int]
fieldsMatchingRule fields rule =
  map fst $ filter (all (`inRange` validRange rule) . snd) fields

solveConstraints :: Eq b => [(a, [b])] -> Maybe [(a, b)]
solveConstraints [] = Just []
solveConstraints options =
  let sortedByConstrainedness = L.sortBy (\a b -> length (snd a) `compare` length (snd b)) options
      (restrictedKey, restrictedValues) = head sortedByConstrainedness
      remainingOptions = tail sortedByConstrainedness
   in getFirst . mconcat . map First $ do
        value <- restrictedValues
        let filteredOptions = map (\(k, vs) -> (k, filter (/= value) vs)) remainingOptions
        let subSolution = solveConstraints filteredOptions
        return $ ((restrictedKey, value) :) <$> subSolution

solveAndExtract :: Input -> Maybe Int
solveAndExtract input =
  let fields = validTicketsByField input
      rules = fieldRules input
      rulesWithMatchingFields =
        map (\rule -> (rule, fieldsMatchingRule fields rule)) rules
      solved = solveConstraints rulesWithMatchingFields
   in case solved of
        Nothing ->
          Nothing
        Just fieldAssignments ->
          let yourValues = fieldValues . yourTicket $ input
              departureFields = map snd . filter (("departure" `T.isPrefixOf`) . fieldName . fst) $ fieldAssignments
              productOfDepartureFields = product . map (yourValues !!) $ departureFields
           in Just productOfDepartureFields

partTwo :: Input -> IO ()
partTwo input = do
  productOfDepartureFields <- runExercise "Part 2" solveAndExtract input
  case productOfDepartureFields of
    Just value ->
      printf "Product of departure fields: %d\n" value
    Nothing ->
      putStrLn "Unable to solve constraints"
