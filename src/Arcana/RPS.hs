{-# LANGUAGE NoImplicitPrelude #-}
module Arcana.RPS where

import Control.Applicative (liftA2)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Maybe (Maybe)
import Data.Tuple (uncurry)
import Text.Show (Show)

import Arcana.Game

data RPS = Rock | Paper | Scissors deriving (Show, Eq)

data Match = Win | Lose | Tie deriving (Show, Eq)

match :: RPS -> RPS -> Match
match Rock     Rock     = Tie
match Rock     Paper    = Lose
match Rock     Scissors = Win
match Paper    Rock     = Win
match Paper    Paper    = Tie
match Paper    Scissors = Lose
match Scissors Rock     = Lose
match Scissors Paper    = Win
match Scissors Scissors = Tie

matchPlay :: SimultaneousPlay RPS RPS -> Maybe Match
matchPlay = uncurry (liftA2 match)
