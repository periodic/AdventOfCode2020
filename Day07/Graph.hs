module Graph where

import Data.Functor
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Set as Set
import Types

type ReverseMap = Map Bag (Set Bag)

data BagGraph = BagGraph
  { forward :: Map Bag [(Int, Bag)],
    reverse :: ReverseMap
  }
  deriving (Show)

makeGraphFromRules :: [BagRule] -> BagGraph
makeGraphFromRules rules =
  let forwardEntries = Map.fromList $ List.map (\rule -> (container rule, contents rule)) rules
      reverseEntries =
        List.foldr addReverseEntries Map.empty rules
   in BagGraph forwardEntries reverseEntries
  where
    upsertSet val = Just . maybe (Set.singleton val) (Set.insert val)
    addReverseEntry container =
      Map.alter (upsertSet container)
    addReverseEntries (BagRule container contents) =
      List.foldr ((.) . addReverseEntry container . snd) id contents

allContainers :: Bag -> BagGraph -> Set Bag
allContainers source graph =
  Map.findWithDefault Set.empty source containers
  where
    containers =
      Map.map
        ( \directContainers ->
            Set.unions . Set.insert directContainers . Set.map (\c -> Map.findWithDefault Set.empty c containers) $ directContainers
        )
        (Graph.reverse graph)

countContents :: Bag -> BagGraph -> Int
countContents source graph =
  Map.findWithDefault 0 source bagSizes
  where
    bagSizes =
      Map.map
        ( (+ 1) . sum . List.map (\(count, bag) -> count * Map.findWithDefault 0 bag bagSizes)
        )
        (forward graph)
