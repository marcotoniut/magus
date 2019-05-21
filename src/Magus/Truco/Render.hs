{-# LANGUAGE LambdaCase, NoImplicitPrelude #-}
module Magus.Truco.Render where

import Data.Semigroup ((<>))
import Data.String (String)

import Arcana.Truco.Types

showCard :: Card -> String
showCard c = showValor (_cardValor c) <> showPalo (_cardPalo c)

showValor :: Valor -> String
showValor = \case
  Tres      -> " 3"
  Dos       -> " 2"
  Ancho     -> " 1"
  Rey       -> "12"
  Caballo   -> "11"
  Sota      -> "10"
  Siete     -> " 7"
  Seis      -> " 6"
  Cinco     -> " 5"
  Cuatro    -> " 4"

-- 3️⃣ 2️⃣ 1️⃣ 🔟 7️⃣ 6️⃣ 5️⃣ 4️⃣ 8️⃣ 9️⃣

showPalo :: Palo -> String
showPalo = \case
  Espada    -> "E"
  Basto     -> "B"
  Oro       -> "O"
  Copa      -> "C"

--  🗡 🍗 ☼ 🍷