{-# LANGUAGE LambdaCase, NoImplicitPrelude, TemplateHaskell, ViewPatterns #-}
module Magus.Truco.Base where

import Control.Applicative (pure)
import Control.Lens (makeLenses)
import Data.Either (Either(Left, Right))
import Data.Either.Combinators
import Data.Eq (Eq, (==))
import Data.Functor ((<$>))
import Data.List.Index
import Data.Word
import Data.Tuple (snd)
import Discord (
    Channel, ChannelRequest(CreateMessage), Gateway, RestChan, Snowflake(Snowflake)
  , ThreadIdType, User, restCall
  )
import Numeric.Natural (Natural)
import Prelude (fromIntegral)
import Text.Show (Show)

import Arcana.Game
-- import Arcana.Truco
import Arcana.Truco.Types
import Magus.Types

data TrucoGame = TrucoGame
  { _trucoGameId        :: Natural
  , _trucoGameChannelId :: Snowflake
  , _trucoGameMazo      :: [Card]
  , _trucoGamePlayer1   :: Player
  , _trucoGameHand1     :: [Card]
  , _trucoGamePlayer2   :: Player
  , _trucoGameHand2     :: [Card]
  , _trucoGameJuego     :: TurnPlay Card Card
  } deriving (Eq, Show)

makeLenses ''TrucoGame

data TrucoGameError u
  = TrucoIncorrectPlayer u
  | TrucoPayedInPublicRoom u Snowflake
  | TrucoPlayError u PlayError
  | TrucoTurnPlayError u TurnPlayError
  deriving (Eq, Show)

trucoGameErrorUser :: (TrucoGameError u) -> u
trucoGameErrorUser = \case
  TrucoIncorrectPlayer u -> u
  TrucoPayedInPublicRoom u _ -> u
  TrucoPlayError u _ -> u
  TrucoTurnPlayError u _ -> u

data PlayError = OutOfBound deriving (Eq, Show)

playCard :: Word -> [Card] -> Either PlayError (Card, [Card])
playCard (fromIntegral -> i) cs = do
  c <- snd <$> maybeToRight OutOfBound (ifind (\k _ -> k == i) cs)
  pure (c, deleteAt i cs)
