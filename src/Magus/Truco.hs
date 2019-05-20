{-# LANGUAGE ApplicativeDo, DataKinds, FlexibleContexts, GADTs, LambdaCase,
    NoImplicitPrelude, OverloadedStrings, RankNTypes, RecursiveDo, ScopedTypeVariables,
    TupleSections, TypeApplications #-}
module Magus.Truco where

import Control.Applicative (liftA2, pure, (<*>))
import Control.Lens (set')
import Control.Monad (join, sequence, unless, void, mapM_, (>>=))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON)
import Data.Bifunctor (bimap)
import Data.Bool (otherwise, (&&))
import Data.Default (def)
import Data.Eq ((==), (/=))
import Data.Either (Either(Left, Right))
import Data.Either.Combinators (rightToMaybe)
import Data.Function (id, ($), (&), (.))
import Data.Functor (fmap, (<$>), (<&>))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust)
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup ((<>))
import Data.Text (Text, pack, unpack)
import Data.Tuple (fst, snd)
import Discord (
    Auth(Auth), Channel, ChannelRequest(CreateMessage, CreateMessageEmbed), Gateway, RestChan
  , Snowflake(Snowflake), ThreadIdType, UserRequest(CreateDM, GetUser)
  , channelId, messageChannel, restCall, userId, userName
  )
import Reflex
import Text.Show (show)

import Arcana.Game
import Arcana.Truco.Types
import Discord.Reflex
import Discord.Reflex.Command
import Magus.Truco.Base
import Magus.Truco.Command

import qualified Discord as D
import qualified Prelude as P

data Jugada = Jugada Player Card

jugar :: Ronda -> Jugada -> Ronda
jugar (Ronda js) j = Ronda (j:js)

-- data Ronda a (n :: Nat) = Ronda (VecList n a)
data Ronda = Ronda [Jugada]

data Juego = Juego [Ronda]


trucoApp :: forall t m.
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  , MonadIO m
  , MonadIO (Performable m)
  , PostBuild t m
  , PerformEvent t m
  , TriggerEvent t m
  ) => (RestChan, Gateway, [ThreadIdType])
    -> m ()
trucoApp dis = do
  e_m <- subscribeToDiscord dis
  let (f_rps, e_ngc) = fanEither $ catchCommand (Proxy @"!truco") e_m
      -- (f_plc, e_plc) = fanEither $ catchCommand (Proxy @"rps-play") e_m

  e_igc <- registerNewGame e_ngc
  (f_ng, e_ng) <- fmap fanEither $ performEvent $ e_igc <&> \(i, c) -> liftIO $ do
    let id1 = _trucoCommandPlayer1 c
        id2 = _trucoCommandPlayer2 c
    ec1 <- restCall dis (CreateDM id1)
    ec2 <- restCall dis (CreateDM id2)
    eu1 <- restCall dis (GetUser id1)
    eu2 <- restCall dis (GetUser id2)
    pure $ (\u1 c1 u2 c2 -> TrucoGame
      { _trucoGameId        = i
      , _trucoGameChannelId = _trucoCommandChannel c
      , _trucoGamePlayer1   = TrucoPlayer u1 c1
      , _trucoGamePlayer2   = TrucoPlayer u2 c2
      , _trucoGameJuego     = consTurnPlay
      }) <$> eu1 <*> ec1 <*> eu2 <*> ec2


  
  P.undefined
  where
    registerNewGame :: P.Num n => Event t a -> m (Event t (n, a))
    registerNewGame e = do
      d_id <- count e
      pure $ attach (current d_id) e
  
