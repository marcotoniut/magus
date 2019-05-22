-- Encoding of the game logic https://github.com/eugeniasimich/truco
{-# LANGUAGE TemplateHaskell #-}
module Arcana.Truco.Types where

import        Control.Monad.State
import        Control.Lens

data Valor = Tres | Dos | Ancho | Rey | Caballo | Sota | Siete | Seis | Cinco | Cuatro
  deriving (Bounded, Enum, Eq, Ord, Show)

data Palo = Espada | Basto | Oro | Copa
  deriving (Bounded, Enum, Eq, Show)

data Card = Card
  { _cardValor :: Valor
  , _cardPalo  :: Palo
  } deriving (Eq, Show)
-- instance Show
-- instance Show Card where
--   show (Card n s) = show n ++ show s

instance Ord Card where
  compare x y = if _cardValor x == _cardValor y && _cardPalo x == _cardPalo y then EQ else cmp' x y
    where cmp' (Card Ancho Espada)  _ = GT
          cmp' _ (Card Ancho Espada)  = LT
          cmp' (Card Ancho Basto)   _ = GT
          cmp' _ (Card Ancho Basto)   = LT
          cmp' (Card Siete Espada)  _ = GT
          cmp' _ (Card Siete Espada)  = LT
          cmp' (Card Siete Oro)     _ = GT
          cmp' _ (Card Siete Oro)     = LT
          cmp' (Card v _) (Card w _)  = compare v w

type Name = String

-- data Player = Player
--   { _playerHand :: [Card]
--   , _playerTeam :: TeamId
--   }

data TrucoPlayer = TrucoPlayer
  { _name     :: Name     --player name
  , _cards    :: [Card]   --cards in hand
  , _down     :: [Card]   --cards on table
  , _team     :: TeamId
  -- , _color    :: Color
  } deriving (Eq, Show)

type PlayerId = Int

data TeamId   = Nos | Ellos deriving (Enum, Eq, Show)

data Action   = Rejected   --For the envido, challenged rejected, points won by team TeamId.
              | Won Int PlayerId    --For the envido, challenge won by playerId, which claimed Int points.

data Truco = Truco | Retruco | ValeCuatro deriving (Enum, Eq, Show) -- Bounded

type TrucoChallenge = (Either NotPlayed Truco, Maybe TeamId)  --truco challenged by teamId 

data NotPlayed = AlMazo Truco | NotPlayed
  deriving (Eq, Show)


data Envido = Envido | EnvidoEnvido | RealEnvido | FaltaEnvido deriving (Enum, Eq, Show) -- Bounded
type EnvidoChallenge = Maybe (Envido, Action, TeamId) -- who won or challenged (a rejected) envido
  
type Round    = Int

data Hand     = Hand
  { _truco              :: TrucoChallenge         --truco challenge already? 
  , _envido             :: EnvidoChallenge        --envido challenge already?
  , _round              :: Round                  --1st, 2nd, 3rd round?
  , _roundCards         :: [(PlayerId, Card)]     --Cards in play
  , _handStarter        :: PlayerId               --player who start the hand
  , _roundStarter       :: PlayerId               --player who starts the round
  , _formerRounds       :: [(Round, Maybe TeamId)] }     --which team won each round
  
data Game     = Game
  { _players  :: [TrucoPlayer]          --Players
  , _hand     :: Hand              --current hand
  , _points   :: (Int, Int) 
  } -- Points by team

makeLenses    ''Game
makeLenses    ''TrucoPlayer
makeLenses    ''Hand

data Option = E Envido | T Truco | C Card deriving Show

type Handler = [(Option, IOS ())]

data Answer = CE Envido | CT Truco | Y | N deriving Show

type Score = Int
-- newtype Score = Score Int

data EnvidoPointsOption = Cantar | SonBuenas | Mesa deriving (Show, Eq)



type IOS a = StateT Game IO a

