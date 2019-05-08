{-# LANGUAGE DataKinds, FlexibleInstances, LambdaCase, MultiParamTypeClasses, NoImplicitPrelude,
    OverloadedStrings, TupleSections, ViewPatterns #-}
module Arcana.RPS.Discord.Command where

import Control.Applicative (liftA2, pure, (<*>))
import Control.Arrow (left)
import Data.Bool (otherwise)
import Data.Char (isNumber, toUpper)
import Data.Either (Either(Left, Right))
import Data.Either.Combinators (maybeToRight)
import Data.Eq (Eq, (==))
import Data.Function (const, ($), (&), (.))
import Data.Functor (fmap, (<$>))
import Data.Int (Int)
import Data.List (drop, take, words, filter)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String (String)
import Data.Text (unpack, pack)
import Data.Word (Word)
import Discord (
    Channel, Message, Snowflake(Snowflake), User, WebhookId
  , messageText, messageChannel, messageAuthor, userId
  )
import Text.Read (Read, readMaybe)
import Text.Show (Show, show)

import Arcana.RPS
import Discord.Reflex.Command

import qualified Prelude as P
import qualified Reflex as R

type CommandError = (String, Message)

-- | Initiates a new Rock Paper Scissors game
data RPSCommand = RPSCommand
  { rpsCommandChannel :: Snowflake
  , rpsCommandPlayer1 :: Snowflake
  , rpsCommandPlayer2 :: Snowflake
  } deriving Show

instance DiscordCommand "rps" (Either CommandError RPSCommand) where
  parseCommand _ m = do
    let p1 = userId $ messageAuthor m
    p2 <- left ((, m)) $ takePlayer (drop 1 . words . unpack $ messageText m)
    pure $ RPSCommand (messageChannel m) p1 p2
    where
      takePlayer :: [String] -> Either String Snowflake
      takePlayer []  = Left "You are missing an opponent."
      takePlayer [x] = maybeToRight "Invalid Username." (toSnowflake x)
      takePlayer _   = Left "Too many options.." -- TODO
      toSnowflake :: String -> Maybe Snowflake
      toSnowflake = fmap Snowflake . readMaybe . filter isNumber

readRPS :: String -> Maybe RPS
readRPS (fmap toUpper -> t)
  | t == "R"  = pure Rock
  | t == "P"  = pure Paper
  | t == "S"  = pure Scissors
  | otherwise = Nothing

-- | A player attempts to make a move
data RPSPlayCommand = RPSPlayCommand
  { rpsPlayCommandChannel :: Snowflake
  , rpsPlayCommandAuthor  :: User
  , rpsPlayCommandChoice  :: RPS
  } deriving (Eq, Show)

instance DiscordCommand "rps-play" (Either CommandError RPSPlayCommand) where
  parseCommand _ m = left (, m) $ do
    let aut = messageAuthor m
    cho <- takeChoice . drop 1 . words . unpack $ messageText m
    pure $ RPSPlayCommand (messageChannel m) aut cho
    where
      takeChoice :: [String] -> Either String RPS
      takeChoice = \case
        []  -> Left "Missing Choice."
        [x] -> readRPS x & maybeToRight rpsChoiceError
        _   -> Left rpsChoiceError
      rpsChoiceError :: String
      rpsChoiceError
        =  "You must select one of the following:\n"
        <> "r (Rock)\n"
        <> "p (Paper)\n"
        <> "s (Scissors)\n"
