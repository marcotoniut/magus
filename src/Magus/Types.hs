{-# LANGUAGE LambdaCase, NoImplicitPrelude, TemplateHaskell #-}
module Magus.Types where

import Control.Lens (makeLenses)
import Data.Eq (Eq)
import Data.Function ((.))
import Discord (Channel, User, Snowflake, userId)
import Numeric.Natural (Natural)
import Text.Show (Show)

import Arcana.Game
import Arcana.RPS

data Participant = Participant
  { _participantUser    :: User
  , _participantChannel :: Channel
  } deriving (Eq, Show)

participantId :: Participant -> Snowflake
participantId = userId . _participantUser

makeLenses ''Participant
