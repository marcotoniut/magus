{-# LANGUAGE LambdaCase, NoImplicitPrelude, TemplateHaskell #-}
module Magus.Truco.Base where

import Control.Lens (makeLenses)
import Data.Eq (Eq)
import Discord (
    Channel, ChannelRequest(CreateMessage), Gateway, RestChan, Snowflake(Snowflake)
  , ThreadIdType, User, restCall
  )
import Numeric.Natural (Natural)
import Text.Show (Show)

import Arcana.Game
-- import Arcana.Truco
import Arcana.Truco.Types

-- |
data TrucoPlayer = TrucoPlayer
  { _trucoPlayerUser :: User
  , _trucoPlayerChannel :: Channel
  } deriving (Eq, Show)

makeLenses ''TrucoPlayer


data TrucoGame = TrucoGame
  { _trucoGameId        :: Natural
  , _trucoGameChannelId :: Snowflake
  , _trucoGamePlayer1   :: TrucoPlayer
  , _trucoGamePlayer2   :: TrucoPlayer
  , _trucoGameJuego     :: TurnPlay Card Card
  } deriving (Eq, Show)

makeLenses ''TrucoGame

-- data TrucoGameError u
--   = RPSIncorrectPlayer u
--   | RPSPayedInPublicRoom u Snowflake
--   | RPSSimultaneousPlayError u SimultaneousPlayError
--   deriving (Eq, Show)

-- trucoGameErrorUser :: (TrucoGameGameError u) -> u
-- trucoGameErrorUser = \case
--   RPSIncorrectPlayer u -> u
--   RPSPayedInPublicRoom u _ -> u
--   RPSSimultaneousPlayError u _ -> u
