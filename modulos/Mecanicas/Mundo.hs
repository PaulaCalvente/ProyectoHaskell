module Mecanicas.Mundo where

import Data.Explosion
import Data.Mundo
import Data.Proyectil
import Data.Robot
import Data.Torreta
import Data.DatosComunes

actualizarWorld :: World -> [Robot] -> [Projectile] -> World
actualizarWorld w robotsActivos proyectilesActivos =
  w { robots = robotsActivos, projectiles = proyectilesActivos }