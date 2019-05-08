{-# LANGUAGE DataKinds, FlexibleInstances, LambdaCase, MultiParamTypeClasses, NoImplicitPrelude,
    OverloadedStrings, ScopedTypeVariables, TupleSections, ViewPatterns #-}
module Magus.Command where

import Control.Applicative (pure)
import Control.Arrow (left)
import Control.Monad ((>>=))
import Data.Either (Either(Left, Right))
import Data.Either.Combinators (maybeToRight)
import Data.Eq ((/=))
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.List (drop, take, words, filter)
import Data.Maybe
import Data.Ord ((>), (<=))
import Data.String (String)
import Data.Semigroup ((<>))
import Data.Text (unpack, pack)
import Data.Word
import Discord (Message, messageText)
import Prelude (Integer, fromInteger, toInteger, maxBound, minBound, pred, succ)
import Text.Read (readMaybe)
import Text.Show (show)

import Discord.Reflex.Command

type CommandError = (String, Message)

-- | Throw dice
data DiceCommand = DiceCommand
  { _diceCommandMessage :: Message
  , _diceCommandSize :: Word8
  }

instance DiscordCommand "!dice" (Either CommandError DiceCommand) where
  parseCommand _ m = do
    let ms = listToMaybe . drop 1 . words . unpack $ messageText m
    s <- left (, m) $ fromMaybe (Right 6)
      ( (>>= \x -> if x <= max       then Right x else Left $ "Dice size cannot exceed " <> show max <> ".")
      . (>>= \x -> if x /= succ min then Right x else Left "You want to throw a dice of only one face?")
      . (>>= \x -> if x /= min      then Right x else Left "Your selection would break space-time continuum.")
      . maybeToRight "You should select a positive number, or nothing at all."
      . (readMaybe :: String -> Maybe Integer) <$> ms
      )
    pure $ DiceCommand m (fromInteger s)
    where
      min = 0
      max = toInteger (pred maxBound :: Word8)
