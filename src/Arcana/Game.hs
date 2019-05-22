{-# LANGUAGE DeriveAnyClass, LambdaCase, NoImplicitPrelude #-}
module Arcana.Game where

-- import Data.Bifunctor (Bifunctor)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq)
import Data.Maybe (Maybe(Just, Nothing))
import Text.Show (Show)

-- | Data structure for a two player game that resolves in two moves in any order
type SimultaneousPlay a b = (Maybe a, Maybe b)
data SimultaneousPlayError = AlreadyPlayedA | AlreadyPlayedB | SimultaneousPlayFinished
  deriving (Eq, Show)

consSimultaneousPlay :: SimultaneousPlay a b
consSimultaneousPlay = (Nothing, Nothing)

playA :: a -> SimultaneousPlay a b -> Either SimultaneousPlayError (SimultaneousPlay a b)
playA x = \case
  (Just _, Just _) -> Left SimultaneousPlayFinished
  (Nothing, my)    -> Right (Just x, my)
  _                -> Left AlreadyPlayedA

playB :: b -> SimultaneousPlay a b -> Either SimultaneousPlayError (SimultaneousPlay a b)
playB y = \case
  (Just _, Just _) -> Left SimultaneousPlayFinished
  (mx, Nothing)    -> Right (mx, Just y)
  _                -> Left AlreadyPlayedB


-- | Data structure that encodes a two-turn game sequence
data TurnPlay a b = PlayedNeither | PlayedFirst a | PlayedBoth a b
  deriving (Eq, Show)
data TurnPlayError = IsFirstTurn | IsSecondTurn | TurnPlayFinished
  deriving (Eq, Show)

consTurnPlay :: TurnPlay a b
consTurnPlay = PlayedNeither

playFirst :: a -> TurnPlay a b -> Either TurnPlayError (TurnPlay a b)
playFirst x = \case
  PlayedNeither   -> Right (PlayedFirst x)
  PlayedFirst _   -> Left IsSecondTurn
  PlayedBoth  _ _ -> Left TurnPlayFinished

playSecond :: b -> TurnPlay a b -> Either TurnPlayError (TurnPlay a b)
playSecond y = \case
  PlayedNeither   -> Left IsFirstTurn
  PlayedFirst x   -> Right (PlayedBoth x y)
  PlayedBoth  _ _ -> Left TurnPlayFinished

justFirstPlay :: TurnPlay a b -> Maybe a
justFirstPlay = \case
  PlayedNeither   -> Nothing
  PlayedFirst x   -> Just x
  PlayedBoth  x _ -> Just x

justSecondPlay :: TurnPlay a b -> Maybe b
justSecondPlay = \case
  PlayedBoth  _ y -> Just y
  _               -> Nothing

justFinished :: TurnPlay a b -> Maybe (a, b)
justFinished = \case
  PlayedBoth  x y -> Just (x, y)
  _               -> Nothing


-- data RoundPlay a (s :: Nat) (n :: Nat) where
--   PlayedNeither | PlayedFirst a | PlayedBoth a b
--   deriving (Eq, Show)

