{-# LANGUAGE DataKinds, FlexibleInstances, LambdaCase, MultiParamTypeClasses, NoImplicitPrelude,
    OverloadedStrings, TupleSections, ViewPatterns #-}
module Magus.Truco.Command where

import Control.Applicative (pure, (<*>))
import Control.Arrow (left)
import Data.Char (isNumber)
import Data.Either (Either(Left, Right))
import Data.Either.Combinators (maybeToRight)
import Data.Eq (Eq)
import Data.Function (($), (&), (.))
import Data.Functor (fmap, (<$>))
import Data.List (drop, take, words, filter)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.String (String)
import Data.Text (unpack, pack)
import Data.Word (Word)
import Discord (
    Channel, Message, Snowflake(Snowflake), User, WebhookId
  , messageText, messageChannel, messageAuthor, userId
  )
import Text.Read (readMaybe)
import Text.Show (Show)

import Arcana.RPS
import Discord.Reflex.Command

import qualified Prelude as P
import qualified Reflex as R

-- | Initiates a new game of Truco
data TrucoCommand = TrucoCommand
  { _trucoCommandChannel :: Snowflake
  , _trucoCommandPlayer1 :: Snowflake
  , _trucoCommandPlayer2 :: Snowflake
  } deriving Show


data TrucoCommandError = MissingTrucoOpponent | TrucoInvalidUsername | IncorrectTrucoArguments

instance DiscordCommand "!truco" (Either (TrucoCommandError, Message) TrucoCommand) where
  parseCommand _ m = left (, m) $ do
    let p1 = userId $ messageAuthor m
    p2 <- takePlayer (drop 1 . words . unpack $ messageText m)
    pure $ TrucoCommand (messageChannel m) p1 p2
    where
      takePlayer :: [String] -> Either TrucoCommandError Snowflake
      takePlayer []  = Left MissingTrucoOpponent
      takePlayer [x] = maybeToRight TrucoInvalidUsername (toSnowflake x)
      takePlayer _   = Left IncorrectTrucoArguments
      toSnowflake :: String -> Maybe Snowflake
      toSnowflake = fmap Snowflake . readMaybe . filter isNumber

-- | A player attempts to make a move
data TrucoPlayCommand = TrucoPlayCommand
  { _trucoPlayCommandChannel :: Snowflake
  , _trucoPlayCommandAuthor  :: User
  , _trucoPlayCommandCard    :: Word
  } deriving (Eq, Show)

data TrucoPlayCommandError = MissingTrucoPlayChoice | TrucoPlayParseError

instance DiscordCommand "!tplay" (Either (TrucoPlayCommandError, Message) TrucoPlayCommand) where
  parseCommand _ m = left (, m) $ do
    let aut = messageAuthor m
    cho <- takeChoice . drop 1 . words . unpack $ messageText m
    pure $ TrucoPlayCommand (messageChannel m) aut cho
    where
      takeChoice :: [String] -> Either TrucoPlayCommandError Word
      takeChoice = \case
        []  -> Left MissingTrucoPlayChoice
        [x] -> readMaybe x & maybeToRight TrucoPlayParseError
        _   -> Left TrucoPlayParseError
