{-# LANGUAGE ApplicativeDo, DataKinds, FlexibleContexts, GADTs, LambdaCase,
    NoImplicitPrelude, OverloadedStrings, RankNTypes, RecursiveDo, ScopedTypeVariables,
    TupleSections, TypeApplications #-}
module Magus.Truco where

import Control.Applicative (liftA2, pure, (<*>))
import Control.Arrow (left)
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
import System.Random
import Text.Show (show)

import Arcana.Game
import Arcana.Truco.Mazo
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
      (f_plc, e_plc) = fanEither $ catchCommand (Proxy @"!tplay") e_m

  e_igc <- registerNewGame e_ngc
  (f_ng, e_ng) <- fmap fanEither $ performEvent $ e_igc <&> \(i, c) -> liftIO $ do
    let id1 = _trucoCommandPlayer1 c
        id2 = _trucoCommandPlayer2 c
    ec1 <- restCall dis (CreateDM id1)
    ec2 <- restCall dis (CreateDM id2)
    eu1 <- restCall dis (GetUser id1)
    eu2 <- restCall dis (GetUser id2)

    r <- newStdGen
    let cs = shuffle r consMazo
        (h1, cs1) = dealPlayer 3 cs
        (h2, cs2) = dealPlayer 3 cs1
    pure $ (\u1 c1 u2 c2 -> TrucoGame
      { _trucoGameId        = i
      , _trucoGameChannelId = _trucoCommandChannel c
      , _trucoGameMazo      = cs2
      , _trucoGamePlayer1   = TrucoPlayer u1 c1
      , _trucoGameHand1     = h1
      , _trucoGamePlayer2   = TrucoPlayer u2 c2
      , _trucoGameHand2     = h2
      , _trucoGameJuego     = consTurnPlay
      }) <$> eu1 <*> ec1 <*> eu2 <*> ec2


  rec
    let
      (f_pla, e_pla) :: (Event t (TrucoGameError Snowflake), Event t (TrucoGame, Maybe TrucoPlayCommand))
        = fanEither
        $ (fmap ((\(g, p) -> do
            g2 <- trucoPlay p g
            if   channelId (_trucoPlayerChannel (_trucoGamePlayer1 g)) /= _trucoPlayCommandChannel p
              && channelId (_trucoPlayerChannel (_trucoGamePlayer2 g)) /= _trucoPlayCommandChannel p
            then Left $ TrucoPayedInPublicRoom (userId (_trucoPlayCommandAuthor p)) (_trucoPlayCommandChannel p)
            else Right (g2, Just p)
            )))
        $ attachWithMaybe (\mgp p -> (, p) . fst <$> mgp) (current d_state)
          e_plc
    d_state :: Dynamic t (Maybe (TrucoGame, Maybe TrucoPlayCommand)) <- holdDyn Nothing . fmap Just $ leftmost
      [ (, Nothing) <$> e_ng
      , e_pla
      ]
  let e_state = mapMaybe id (updated d_state)
  P.undefined
  where
    registerNewGame :: P.Num n => Event t a -> m (Event t (n, a))
    registerNewGame e = do
      d_id <- count e
      pure $ attach (current d_id) e
    trucoPlay :: TrucoPlayCommand -> TrucoGame -> Either (TrucoGameError Snowflake) TrucoGame
    trucoPlay p g
      | uid == (userId . _trucoPlayerUser $ _trucoGamePlayer1 g)
        = do
          (c, cs) <- left (TrucoPlayError uid) $ playCard (_trucoPlayCommandCard p) (_trucoGameHand1 g)
          setPlay $ playFirst c gp
      | uid == (userId . _trucoPlayerUser $ _trucoGamePlayer2 g)
        = do
          (c, cs) <- left (TrucoPlayError uid) $ playCard (_trucoPlayCommandCard p) (_trucoGameHand2 g)
          setPlay $ playSecond c gp
      | otherwise = Left $ TrucoIncorrectPlayer uid
      where
        setPlay = bimap (TrucoTurnPlayError uid) (\p -> set' trucoGameJuego p g)
        uid = userId $ _trucoPlayCommandAuthor p
        pc = _trucoPlayCommandCard p
        gp = _trucoGameJuego g
