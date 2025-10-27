module Mecanicas.Explosiones where

import Data.Explosion
import Data.Mundo
import Data.Proyectil
import Data.Robot
import Data.Torreta
import Data.DatosComunes

import Mecanicas.Robot
import Mecanicas.Collision

import Utils

-- Actualiza la lista de explosiones activas (resta tiempo y mantiene las que siguen vivas)
actualizarExplosiones :: Float -> [Explosion] -> [Explosion] -> [Explosion]
actualizarExplosiones dt existentes nuevas =
  [ Explosion pos size (tiempoTotal - dt) src
  | Explosion pos size tiempoTotal src <- existentes ++ nuevas
  , tiempoTotal - dt > 0
  ]

-- Detecta impactos proyectil-robot
detectarImpactos :: [Robot] -> [Projectile] -> [(Id, Id, Damage, Position)]
detectarImpactos rs ps =
  [ (idR r, idP p, damage (commonP p), position (commonP p))
  | r <- rs, healthR r > 0
  , p <- ps
  , idR r /= idP p
  , checkCollision (position (commonP p)) projectileRadius (robotBox r)
  ]

-- ✅ Solo resta vida, no marca haveExploded aquí
aplicarDaño :: [Robot] -> [(Id, Id, Damage, Position)] -> [Robot]
aplicarDaño rs impactos =
  [ if any (\(idr, _, _, _) -> idr == idR r) impactos
      then let totalDaño = sum [ d | (idr, _, d, _) <- impactos, idr == idR r ]
           in r { healthR = max 0 (healthR r - totalDaño) }
      else r
  | r <- rs
  ]

-- Genera explosiones de impacto normales (no de muerte)
generarExplosiones :: [(Id, Id, Damage, Position)] -> [Explosion]
generarExplosiones impactos =
  [ Explosion pos (30, 0) 0.6 (RobotHitByProjectile rid pid dmg pos)
  | (rid, pid, dmg, pos) <- impactos
  ]
