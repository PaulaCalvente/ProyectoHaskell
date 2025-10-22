module Mecanicas.Movement where

import Graphics.Gloss hiding (Point, Vector) --Se omite Point y Vector para evitar conflictos con nuestros módulos
import Data.Explosion
import Data.Mundo
import Data.Proyectil
import Data.Robot
import Data.Torreta
import Data.DatosComunes
import Mecanicas.Proyectil

generarRecorrido :: Id -> [Position]
generarRecorrido id = take 11 $ zip xs ys
  where
    xs = [ fromIntegral ((id * i * 1111) `mod` 500) - 250 | i <- [1..] ]
    ys = [ fromIntegral ((id * i * 713) `mod` 500) - 250 | i <- [1..] ]