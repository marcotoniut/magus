{-# LANGUAGE ApplicativeDo, DataKinds, FlexibleContexts, GADTs, LambdaCase,
    NoImplicitPrelude, OverloadedStrings, RankNTypes, RecursiveDo, ScopedTypeVariables,
    TupleSections, TypeApplications #-}
module Magus where

import Control.Arrow (first)
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
import Discord.Reflex
import Discord.Reflex.Command
import Magus.Command
import Magus.RPS (rpsApp)

import qualified Discord as D
import qualified Prelude as P

import System.Random

attachRandom ::
  ( MonadHold t m
  , MonadFix m
  , Random a
  , RandomGen g
  , Reflex t
  ) => g
    -> Event t (g -> (a, g), b)
    -> m (Event t (a, b))
attachRandom rg e = do
  rec
    b_g <- hold rg (snd . fst <$> e_r)
    let e_r = attachWith (\g (f, x) -> (f g, x)) b_g e
  pure (first fst <$> e_r)


magusApp :: forall t m g.
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  , MonadIO m
  , MonadIO (Performable m)
  , PostBuild t m
  , PerformEvent t m
  , RandomGen g
  , TriggerEvent t m
  ) => (RestChan, Gateway, [ThreadIdType])
    -> g
    -> m ()
magusApp dis rg = do
  e_m <- subscribeToDiscord dis
  let (f_dc, e_dc) = fanEither $ catchCommand (Proxy @"!dice") e_m
  
  e_dr <- attachRandom rg (e_dc <&> \c -> (randomR (0, _diceCommandSize c), c))

  rpsApp dis

  mapM_ (emitToDiscord dis)
    [ e_dr <&> \(r, DiceCommand m s) ->
        CreateMessageEmbed (messageChannel m) "" def
          { D.embedTitle = pure $ "Throws D" <> show s
          , D.embedDescription = pure $ show r
          }
    ]
