{-# LANGUAGE ApplicativeDo, DataKinds, FlexibleContexts, GADTs, LambdaCase,
    NoImplicitPrelude, OverloadedStrings, RankNTypes, RecursiveDo, ScopedTypeVariables,
    TupleSections, TypeApplications, ViewPatterns #-}
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
import Data.List.Index (indexed)
import Data.Maybe (Maybe(Just, Nothing), isJust, maybe)
import Data.Monoid (mconcat)
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup ((<>))
import Data.Text (Text, pack, unpack)
import Data.Tuple (fst, snd)
import Discord (
    Auth(Auth), Channel, ChannelRequest(CreateMessage, CreateMessageEmbed), Gateway, RestChan
  , Snowflake(Snowflake), ThreadIdType, UserRequest(CreateDM, GetUser)
  , channelId, messageChannel, restCall, userId, userName
  )
import Prelude (succ)
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
import Magus.Truco.Render
import Magus.Types

import qualified Discord as D
import qualified Prelude as P

data Jugada = Jugada Participant Card

jugar :: Ronda -> Jugada -> Ronda
jugar (Ronda js) j = Ronda (j:js)

-- data Ronda a (n :: Nat) = Ronda (VecList n a)
data Ronda = Ronda [Jugada]

data Juego = Juego [Ronda]

trucoApp :: forall t m.
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  , MonadIO m
  , MonadIO (Performable m)
  , PostBuild t m
  , PerformEvent t m
  , TriggerEvent t m
  ) => (RestChan, Gateway, [ThreadIdType])
    -> Event t D.Event
    -> m ()
trucoApp dis e_m = do
  let (f_ngc, e_ngc) = fanEither $ catchCommand (Proxy @"!truco") e_m
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
      , _trucoGamePlayer1   = Participant u1 c1
      , _trucoGameHand1     = h1
      , _trucoGamePlayer2   = Participant u2 c2
      , _trucoGameHand2     = h2
      , _trucoGameJuego     = consTurnPlay
      }) <$> eu1 <*> ec1 <*> eu2 <*> ec2

  rec
    let
      (f_pla, e_pla) :: (Event t (TrucoGameError Snowflake), Event t (TrucoGame, Maybe TrucoPlayCommand))
        = fanEither
        $ (fmap ((\(g, p) -> do
            g2 <- trucoPlay p g
            if   channelId (_participantChannel (_trucoGamePlayer1 g)) /= _trucoPlayCommandChannel p
              && channelId (_participantChannel (_trucoGamePlayer2 g)) /= _trucoPlayCommandChannel p
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
  mapM_ (emitToDiscord dis)
    [ e_ng <&> \g ->
        CreateMessageEmbed (_trucoGameChannelId g) "" trucoEmbed
          { D.embedDescription = pure . unpack
            $ renderTrucoGameStarted g
          }
    , e_ng <&> \g ->
        CreateMessageEmbed (channelId . _participantChannel $ _trucoGamePlayer1 g) "" trucoEmbed
          { D.embedDescription = pure . unpack
            $ renderTrucoPlayerHand (_trucoGameHand1 g)
          }
    , e_ng <&> \g ->
        CreateMessageEmbed (channelId . _participantChannel $ _trucoGamePlayer2 g) "" trucoEmbed
          { D.embedDescription = pure . unpack
            $ renderTrucoPlayerHand (_trucoGameHand2 g)
          }
    -- , ffilter (isJust . matchPlay . _rpsGamePlay) (fst <$> e_state) <&> \g ->
    --     CreateMessageEmbed (_rpsGameChannelId g) "" rpsEmbed
    --       { D.embedDescription = pure . unpack $ renderRPSGameFinished g
    --       }
    -- , mapMaybe sequence e_state <&> \(g, p) -> do
    --     let u1 = _rpsGamePlayer1 g
    --         u2 = _rpsGamePlayer2 g
    --     if userId (rpsPlayCommandAuthor p) == userId (_participantUser u1)
    --     then CreateMessageEmbed (_rpsGameChannelId g) "" $ rpsEmbed
    --       { D.embedDescription = pure $ userName (_participantUser u1) <> " made a move."
    --       }
    --     else CreateMessageEmbed (_rpsGameChannelId g) "" $ rpsEmbed
    --       { D.embedDescription = pure
    --         $ userName (_participantUser u2) <> " made a move."
    --       }
    -- -- , f_dmc <&> \e -> CreateMessage (Snowflake c) $ pack (show e)
    , f_pla <&> \case
        TrucoPayedInPublicRoom _ c -> CreateMessageEmbed c "" trucoEmbed
          { D.embedDescription = pure "You shouldn't announce your play on a public room! Tell me on DM instead."
          }
        e -> CreateMessageEmbed (trucoGameErrorUser e) "" trucoEmbed
          { D.embedDescription = pure $ show e
          }
    , f_ngc <&> \(e, m) -> CreateMessageEmbed (messageChannel m) "" trucoEmbed
          { D.embedDescription = pure $ e & \case
            MissingTrucoOpponent    -> "Opponent option is missing."
            TrucoInvalidUsername    -> "Opponent options isn't properly formatted. Should select a username."
            IncorrectTrucoArguments -> "Incorrect number of arguments."
          }
    , f_plc <&> \(e, m) -> CreateMessageEmbed (messageChannel m) "" trucoEmbed
          { D.embedDescription = pure $ e & \case
            MissingTrucoPlayChoice -> "Select one the cards in your hand."
            TrucoPlayParseError    -> "Truco play command wasn't properly formatted."
          }
    ]
  where
    registerNewGame :: P.Num n => Event t a -> m (Event t (n, a))
    registerNewGame e = do
      d_id <- count e
      pure $ attach (current d_id) e
    trucoPlay :: TrucoPlayCommand -> TrucoGame -> Either (TrucoGameError Snowflake) TrucoGame
    trucoPlay p g
      | uid == (userId . _participantUser $ _trucoGamePlayer1 g)
        = do
          (c, cs) <- left (TrucoPlayError uid) $ playCard (_trucoPlayCommandCard p) (_trucoGameHand1 g)
          setPlay $ playFirst c gp
      | uid == (userId . _participantUser $ _trucoGamePlayer2 g)
        = do
          (c, cs) <- left (TrucoPlayError uid) $ playCard (_trucoPlayCommandCard p) (_trucoGameHand2 g)
          setPlay $ playSecond c gp
      | otherwise = Left $ TrucoIncorrectPlayer uid
      where
        setPlay = bimap (TrucoTurnPlayError uid) (\p -> set' trucoGameJuego p g)
        uid = userId $ _trucoPlayCommandAuthor p
        pc = _trucoPlayCommandCard p
        gp = _trucoGameJuego g


trucoEmbed :: D.Embed
trucoEmbed = def
  { D.embedTitle = pure "Truco Game"
  } 

renderTrucoPlayerHand :: [Card] -> Text
renderTrucoPlayerHand (indexed -> cs)
  =  "YOUR HAND!\n"
  <> "Respond with " <> commandName (Proxy @"!tplay") <> " followed by an option:\n"
  <> pack (mconcat (ffor cs (\(i, c) -> show (succ i) <> ") " <> showCard c <> "\n")))

renderTrucoGameStarted :: TrucoGame -> Text
renderTrucoGameStarted g = pack
  $  (userName . _participantUser . _trucoGamePlayer1) g
  <> " vs "
  <> (userName . _participantUser . _trucoGamePlayer2) g <> "\n"
  <> "A DM was sent with your dealt Hand\n"

renderTrucoGameFinished :: TrucoGame -> Text
renderTrucoGameFinished g = pack
  $  "(" <> (userName . _participantUser . _trucoGamePlayer1) g <> ") "
  <> maybe "[ X ]" show (justFirstPlay $ _trucoGameJuego g)
  <> " vs "
  <> maybe "[ X ]" show (justSecondPlay $ _trucoGameJuego g)
  <> " (" <> (userName . _participantUser . _trucoGamePlayer2) g <> ")\n"
  -- <> fromMaybe "MATCH UNRESOLVED" (renderResult <$> matchPlay (_trucoGamePlay g)) <> "\n"
  -- where
  --   renderResult = \case
  --     Win  -> (userName . _participantUser $ _trucoGamePlayer1 g) <> " WINS!"
  --     Lose -> (userName . _participantUser $ _trucoGamePlayer2 g) <> " WINS!"
  --     Tie  -> "GAME WAS A TIE"
        
