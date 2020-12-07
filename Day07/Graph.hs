module Graph where

import Data.Map.Strict as Map
import Data.Set as Set
import Data.List as List
import Data.Maybe as Maybe

import Types

type ReverseMap = Map Bag (Set Bag)

data BagGraph = BagGraph
    { forward :: Map Bag [(Int, Bag)]
    , reverse :: ReverseMap
    } deriving (Show)

makeGraphFromRules :: [BagRule] -> BagGraph
makeGraphFromRules rules =
    let
        forwardEntries = Map.fromList $ List.map (\rule -> (container rule, contents rule)) rules
        reverseEntries = 
            List.foldr addReverseEntries Map.empty rules
    in
        BagGraph forwardEntries reverseEntries
    where
        addReverseEntry container =
            Map.alter (upsertSet container) 
        upsertSet val (Just set) = Just (Set.insert val set)
        upsertSet val Nothing    = Just (Set.singleton val)
        addReverseEntries :: BagRule -> ReverseMap -> ReverseMap
        addReverseEntries (BagRule container contents) =
            List.foldr ((.) . addReverseEntry container . snd) id contents

allContainers :: Bag -> BagGraph -> Set Bag
allContainers source graph =
    Set.delete source $ explore [source] (Graph.reverse graph) Set.empty

explore :: [Bag] -> ReverseMap -> Set Bag -> Set Bag
explore []           _     visited = visited
explore (next:queue) graph visited =
    if Set.member next visited
        then explore queue graph visited
        else
            let
                neighbors = maybe [] Set.toList $ Map.lookup next graph
                newQueue = queue ++ neighbors
                newVisited = Set.insert next visited
            in
                explore newQueue graph newVisited

countContents :: Bag -> BagGraph -> Int
countContents source graph =
    let
        contents = Map.findWithDefault [] source (forward graph)
    in
        sum . List.map (uncurry countMultipleContents) $ contents
    where
        countMultipleContents :: Int -> Bag -> Int
        countMultipleContents mult bag =
            mult * (countContents bag graph + 1)