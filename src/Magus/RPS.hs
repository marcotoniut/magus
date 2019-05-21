{-# LANGUAGE ApplicativeDo, DataKinds, FlexibleContexts, GADTs, LambdaCase,
    NoImplicitPrelude, OverloadedStrings, RankNTypes, RecursiveDo, ScopedTypeVariables,
    TupleSections, TypeApplications #-}
module Magus.RPS where

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
import Data.Maybe (Maybe(Just, Nothing), isJust, maybe)
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
import Arcana.RPS
import Discord.Reflex
import Discord.Reflex.Command
import Magus.RPS.Base
import Magus.RPS.Command

import qualified Discord as D
import qualified Prelude as P

rpsApp :: forall t m.
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
rpsApp dis = do
  e_m <- subscribeToDiscord dis
  let (f_rps, e_ngc) = fanEither $ catchCommand (Proxy @"!rps") e_m
      (f_plc, e_plc) = fanEither $ catchCommand (Proxy @"!rpsplay") e_m

  e_igc <- registerNewGame e_ngc
  (f_ng, e_ng) <- fmap fanEither $ performEvent $ e_igc <&> \(i, c) -> liftIO $ do
    let id1 = rpsCommandPlayer1 c
        id2 = rpsCommandPlayer2 c
    ec1 <- restCall dis (CreateDM id1)
    ec2 <- restCall dis (CreateDM id2)
    eu1 <- restCall dis (GetUser id1)
    eu2 <- restCall dis (GetUser id2)
    pure $ (\u1 c1 u2 c2 -> RPSGame
      { _rpsGameId        = i
      , _rpsGameChannelId = rpsCommandChannel c
      , _rpsGamePlayer1   = RPSPlayer u1 c1
      , _rpsGamePlayer2   = RPSPlayer u2 c2
      , _rpsGamePlay      = consSimultaneousPlay
      }) <$> eu1 <*> ec1 <*> eu2 <*> ec2

  rec
    let
      (f_pla, e_pla) :: (Event t (RPSGameError Snowflake), Event t (RPSGame, Maybe RPSPlayCommand))
        = fanEither
        $ (fmap ((\(g, p) -> do
            g2 <- rpsPlay p g
            if   channelId (_rpsPlayerChannel (_rpsGamePlayer1 g)) /= rpsPlayCommandChannel p
              && channelId (_rpsPlayerChannel (_rpsGamePlayer2 g)) /= rpsPlayCommandChannel p
            then Left $ RPSPayedInPublicRoom (userId (rpsPlayCommandAuthor p)) (rpsPlayCommandChannel p)
            else Right (g2, Just p)
            )))
        $ attachWithMaybe (\mgp p -> (, p) . fst <$> mgp) (current d_state)
          e_plc
    d_state :: Dynamic t (Maybe (RPSGame, Maybe RPSPlayCommand)) <- holdDyn Nothing . fmap Just $ leftmost
      [ (, Nothing) <$> e_ng
      , e_pla
      ]
  let e_state = mapMaybe id (updated d_state)

  mapM_ (emitToDiscord dis)
    [ e_ng <&> \g ->
        CreateMessageEmbed (_rpsGameChannelId g) "" rpsEmbed
          { D.embedDescription = pure . unpack
            $ renderRPSGameStarted g
          }
    , e_ng <&> \g ->
        CreateMessageEmbed (channelId . _rpsPlayerChannel $ _rpsGamePlayer1 g) "" rpsEmbed
          { D.embedDescription = pure . unpack
            $ renderRPSPlayerExplanation
          }
    , e_ng <&> \g ->
        CreateMessageEmbed (channelId . _rpsPlayerChannel $ _rpsGamePlayer2 g) "" rpsEmbed
          { D.embedDescription = pure . unpack
            $ renderRPSPlayerExplanation
          }
    , ffilter (isJust . matchPlay . _rpsGamePlay) (fst <$> e_state) <&> \g ->
        CreateMessageEmbed (_rpsGameChannelId g) "" rpsEmbed
          { D.embedDescription = pure . unpack $ renderRPSGameFinished g
          }
    , mapMaybe sequence e_state <&> \(g, p) -> do
        let u1 = _rpsGamePlayer1 g
            u2 = _rpsGamePlayer2 g
        if userId (rpsPlayCommandAuthor p) == userId (_rpsPlayerUser u1)
        then CreateMessageEmbed (_rpsGameChannelId g) "" $ rpsEmbed
          { D.embedDescription = pure $ userName (_rpsPlayerUser u1) <> " made a move."
          }
        else CreateMessageEmbed (_rpsGameChannelId g) "" $ rpsEmbed
          { D.embedDescription = pure
            $ userName (_rpsPlayerUser u2) <> " made a move."
          }
    -- , f_dmc <&> \e -> CreateMessage (Snowflake c) $ pack (show e)
    , f_pla <&> \case
        RPSPayedInPublicRoom _ c -> CreateMessageEmbed c "" rpsEmbed
          { D.embedDescription = pure $ "You shouldn't announce your play on a public room! Tell me on DM instead."
          }
        e -> CreateMessageEmbed (rpsGameErrorUser e) "" rpsEmbed
          { D.embedDescription = pure $ show e
          }
    , f_rps <&> \(e, m) -> CreateMessageEmbed (messageChannel m) "" rpsEmbed
          { D.embedDescription = pure e
          }
    , f_plc <&> \(e, m) -> CreateMessageEmbed (messageChannel m) "" rpsEmbed
          { D.embedDescription = pure e
          }
    ]
  where
    errorMessage (e, m) = CreateMessage (messageChannel m) (pack e)
    -- playFailed = CreateMessage (Snowflake c) . pack . show
    -- startCommandFailed (m, _) = CreateMessage (Snowflake c) (pack m)
    -- playCommandFailed (m, _) = CreateMessage (Snowflake c) (pack m)
    registerNewGame :: P.Num n => Event t a -> m (Event t (n, a))
    registerNewGame e = do
      d_id <- count e
      pure $ attach (current d_id) e
    rpsPlay :: RPSPlayCommand -> RPSGame -> Either (RPSGameError Snowflake) RPSGame
    rpsPlay p g
      | uid == (userId . _rpsPlayerUser $ _rpsGamePlayer1 g) = setPlay $ playA pc gp
      | uid == (userId . _rpsPlayerUser $ _rpsGamePlayer2 g) = setPlay $ playB pc gp
      | otherwise = Left $ RPSIncorrectPlayer uid
      where
        setPlay = bimap (RPSSimultaneousPlayError uid) (\p -> set' rpsGamePlay p g)
        uid = userId $ rpsPlayCommandAuthor p
        pc = rpsPlayCommandChoice p
        gp = _rpsGamePlay g

rpsEmbed :: D.Embed
rpsEmbed = def
  { D.embedTitle = pure "Rock Paper Scissors Game"
  } 

renderRPSPlayerExplanation :: Text
renderRPSPlayerExplanation 
  =  "MATCH STARTED!\n"
  <> "Respond with " <> commandName (Proxy @"!rpsplay") <> " followed by an option:\n"
  <> "`r` (Rock)\n"
  <> "`p` (Paper)\n"
  <> "`s` (Scissors)\n"

renderRPSGameStarted :: RPSGame -> Text
renderRPSGameStarted g = pack
  $  "BEGIN!\n"
  <> (userName . _rpsPlayerUser $ _rpsGamePlayer1 g)
  <> " vs "
  <> (userName . _rpsPlayerUser $ _rpsGamePlayer2 g) <> "\n"

renderRPSGameFinished :: RPSGame -> Text
renderRPSGameFinished g = pack
  $  "(" <> (userName . _rpsPlayerUser $ _rpsGamePlayer1 g) <> ") "
  <> maybe "[ X ]" show (fst $ _rpsGamePlay g)
  <> " vs "
  <> maybe "[ X ]" show (snd $ _rpsGamePlay g)
  <> " (" <> (userName . _rpsPlayerUser $ _rpsGamePlayer2 g) <> ")\n"
  <> maybe "MATCH UNRESOLVED" renderResult (matchPlay (_rpsGamePlay g)) <> "\n"
  where
    renderResult = \case
      Win  -> (userName . _rpsPlayerUser $ _rpsGamePlayer1 g) <> " WINS!"
      Lose -> (userName . _rpsPlayerUser $ _rpsGamePlayer2 g) <> " WINS!"
      Tie  -> "GAME WAS A TIE"
