{-# LANGUAGE LambdaCase, NoImplicitPrelude, TemplateHaskell #-}
module Magus.Types where

import Control.Lens (makeLenses)
import Data.Eq (Eq)
import Discord (Channel, User)
import Numeric.Natural (Natural)
import Text.Show (Show)

import Arcana.Game
import Arcana.RPS

-- Participant
data Player = Player
  { _playerUser    :: User
  , _playerChannel :: Channel
  } deriving (Eq, Show)

makeLenses ''Player
