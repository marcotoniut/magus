{-# LANGUAGE LambdaCase, NoImplicitPrelude, TemplateHaskell #-}
module Arcana.RPS.Discord where

import Control.Lens (makeLenses)
import Data.Eq (Eq)
import Discord (
    Channel, ChannelRequest(CreateMessage), Gateway, RestChan, Snowflake(Snowflake)
  , ThreadIdType, User, restCall
  )
import Numeric.Natural (Natural)
import Text.Show (Show)

import Arcana.Game
import Arcana.RPS

data RPSPlayer = RPSPlayer
  { _rpsPlayerUser :: User
  , _rpsPlayerChannel :: Channel
  } deriving (Eq, Show)

makeLenses ''RPSPlayer


data RPSGame = RPSGame
  { _rpsGameId        :: Natural
  , _rpsGameChannelId :: Snowflake
  , _rpsGamePlayer1   :: RPSPlayer
  , _rpsGamePlayer2   :: RPSPlayer
  , _rpsGamePlay      :: SimultaneousPlay RPS RPS
  } deriving (Eq, Show)

makeLenses ''RPSGame

data RPSGameError u
  = RPSIncorrectPlayer u
  | RPSPayedInPublicRoom u Snowflake
  | RPSSimultaneousPlayError u SimultaneousPlayError
  deriving (Eq, Show)

rpsGameErrorUser :: (RPSGameError u) -> u
rpsGameErrorUser = \case
  RPSIncorrectPlayer u -> u
  RPSPayedInPublicRoom u _ -> u
  RPSSimultaneousPlayError u _ -> u
