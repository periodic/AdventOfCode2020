module Main where

import Common (loadAndParseInput)
import Control.Monad.Writer
import Data.Attoparsec.Text as P
  ( Parser,
    decimal,
    endOfLine,
    sepBy,
    string,
  )
import qualified Data.List as L
import Data.Monoid (Sum (Sum, getSum))
import Data.Sequence
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Text (Text)
import Data.Foldable

type Deck = Seq Int

type Game = (Deck, Deck)

data GameStatus a
  = Playing !a
  | Victory !Player !Deck
  deriving (Show)

data Player
  = Player1
  | Player2
  deriving (Show, Eq)

scoreDeck :: Deck -> Int
scoreDeck deck =
  getSum . Seq.foldMapWithIndex (\index card -> Sum $ card * (Seq.length deck - index)) $ deck

--------------------------------------------------
-- Parsing
--------------------------------------------------

cardP :: Parser Int
cardP =
  decimal

playerP :: Text -> Parser Deck
playerP name =
  string name
    *> ":"
    *> endOfLine
    *> (Seq.fromList <$> cardP `sepBy` endOfLine)

gameP :: Parser (Deck, Deck)
gameP =
  (,)
    <$> playerP "Player 1"
    <* endOfLine
    <* endOfLine
    <*> playerP "Player 2"

--------------------------------------------------
-- Part One
--------------------------------------------------

runRound :: Game -> GameStatus Game
runRound (card1 :<| player1, card2 :<| player2)
  | card1 > card2 = Playing (player1 |> card1 |> card2, player2)
  | card1 < card2 = Playing (player1, player2 |> card2 |> card1)
  -- No instructions on what to do in this case, so I'll just put theme both on the bottom
  | otherwise = Playing (player1 |> card1, player2 |> card2)
runRound (Empty, deck@(_ :<| _)) = Victory Player2 deck
runRound (deck, Empty) = Victory Player1 deck
runRound (Empty, Empty) = error "Playing a game with no decks"

runGame :: (a -> GameStatus a) -> a -> IO (Player, Deck)
runGame run game =
  case run game of
    Playing nextRound ->
      runGame run nextRound
    Victory player deck ->
      return (player, deck)

partOne :: Game -> IO ()
partOne game = do
  (player, deck) <- runGame runRound game
  putStrLn $ show player ++ " wins!"
  putStr "Deck: "
  print deck
  putStr "Score: "
  print . scoreDeck $ deck

--------------------------------------------------
-- Part One
--------------------------------------------------

runRecursive :: Game -> S.Set Game -> Writer [String] (Player, Deck)
runRecursive !game !prevGames =
  if S.member game prevGames
    then do
      tell ["We've seen this before", "Winner is Player 1!"]
      return (Player1, fst game)
    else case game of
      (deck, Empty) -> do
        tell ["Winner is Player 1!"]
        return (Player1, deck)
      (Empty, deck) -> do
        tell ["Winner is Player 2!"]
        return (Player2, deck)
      (card1 :<| player1, card2 :<| player2) -> do
        tell [
          "Player 1's deck: " ++ show (card1 : toList player1),
          "Player 2's deck: " ++ show (card2 : toList player2),
          "Player 1 plays: " ++ show card1,
          "Player 2 plays: " ++ show card2]
        let prevGames_ = S.insert game prevGames
            recursiveGame =
              let player1_ = Seq.take card1 player1
                  player2_ = Seq.take card2 player2
                  game_ = (player1_, player2_)
               in runRecursive game_ S.empty
            isRecursive =
              card1 <= Seq.length player1 && card2 <= Seq.length player2
            player1Wins = do
              tell ["Player 1 wins the round."]
              runRecursive (player1 |> card1 |> card2, player2) prevGames_
            player2Wins = do
              tell ["Player 2 wins the round."]
              runRecursive (player1, player2 |> card2 |> card1) prevGames_
        if isRecursive
          then do
            tell ["Playing a sub-game to determine the winner"]
            (winner, _) <- recursiveGame
            if winner == Player1
              then player1Wins
              else player2Wins
          else
            if card1 >= card2
              then player1Wins
              else player2Wins

partTwo :: Game -> IO ()
partTwo game =
  let ((player, deck), log) = runWriter $ runRecursive game S.empty
   in do
        -- putStrLn . L.intercalate "\n" $ log
        putStr "Score: "
        print . scoreDeck $ deck

--------------------------------------------------
-- Main
--------------------------------------------------

main :: IO ()
main = do
  game <- loadAndParseInput gameP
  partOne game
  partTwo game