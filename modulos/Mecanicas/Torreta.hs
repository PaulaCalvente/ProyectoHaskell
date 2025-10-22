module Mecanicas.Torreta where

import Data.Explosion
import Data.Mundo
import Data.Proyectil
import Data.Robot
import Data.Torreta
import Data.DatosComunes

import Utils

nuevoCooldown :: Turret -> Float -> Float
nuevoCooldown t dt = max 0 (cooldown t - dt)

actualizarCooldown :: Robot -> Float -> Robot
actualizarCooldown r t1 = r { turret = (turret r) { cooldown = t1 } }

actualizarTorreta :: Robot -> Robot -> Robot
actualizarTorreta r objetivo =
  let ang = calcularAngulo r objetivo
      vec = calcularVector ang
  in r { turret = (turret r) { angleT = ang, vectorT = vec } }