{-# LANGUAGE DataKinds, FlexibleInstances, LambdaCase, MultiParamTypeClasses, NoImplicitPrelude,
    OverloadedStrings, ScopedTypeVariables, TupleSections, TypeApplications, ViewPatterns #-}
module Magus.Command where

import Control.Applicative (pure)
import Control.Arrow (left)
import Control.Monad ((>>=))
import Data.Bool (otherwise)
import Data.Either (Either(Left, Right))
import Data.Either.Combinators (maybeToRight)
import Data.Eq ((==), (/=))
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.List (drop, filter, length, take, words)
import Data.Maybe (Maybe, fromMaybe, listToMaybe)
import Data.Ord ((>), (<=), (<))
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
  parseCommand _ m = left (, m) $ do
    let xs = drop 1 . words . unpack $ messageText m
    xs' <- if 1 < length xs then Left "Only one optional argument accepted (dice faces)" else pure xs
    let ms = listToMaybe xs'
    s <- fromMaybe (Right 6)
      ( (>>= restrictFaces)
      . maybeToRight "Your optional selection should be valid number."
      . (readMaybe :: String -> Maybe Integer) <$> ms
      )
    pure $ DiceCommand m (fromInteger s)
    where
      minB = 0 :: Integer
      maxB = toInteger @Word8 (pred maxBound)
      restrictFaces x
        | x > maxB       = Left $ "Dice size cannot exceed " <> show maxB <> "."
        | x == succ minB = Left "You want to throw a dice of only one face?"
        | x <= minB      = Left $ "Your selection of " <> show x <> " face/s would break space-time continuum."
        | otherwise      = pure x
