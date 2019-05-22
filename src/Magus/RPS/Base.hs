{-# LANGUAGE LambdaCase, NoImplicitPrelude, TemplateHaskell #-}
module Magus.RPS.Base where

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
import Magus.Types

data RPSGame = RPSGame
  { _rpsGameId        :: Natural
  , _rpsGameChannelId :: Snowflake
  , _rpsGamePlayer1   :: Player
  , _rpsGamePlayer2   :: Player
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
