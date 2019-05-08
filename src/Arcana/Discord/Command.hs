{-# LANGUAGE DataKinds, FlexibleInstances, LambdaCase, MultiParamTypeClasses, NoImplicitPrelude,
    OverloadedStrings, TupleSections, ViewPatterns #-}
module Arcana.Discord.Command where

import Control.Applicative (pure)
import Data.Function (($))
import Data.Either (Either)
import Data.String (String)
import Data.Word
import Discord (Message)

import Discord.Reflex.Command

type CommandError = (String, Message)

-- | Throw dice
data DiceCommand = DiceCommand
  { _diceCommandMessage :: Message
  , _diceCommandSize :: Word8
  }

instance DiscordCommand "!dice" (Either CommandError DiceCommand) where
  parseCommand _ m = pure $ DiceCommand m 6
