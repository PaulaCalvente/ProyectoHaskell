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

actualizarExplosiones :: Float -> [Explosion] -> [Explosion] -> [Explosion]
actualizarExplosiones dt existentes nuevas =
  [ Explosion pos size (ttl - dt) src
  | Explosion pos size ttl src <- existentes ++ nuevas
  , ttl - dt > 0
  ]

detectarImpactos :: [Robot] -> [Projectile] -> [(Id, Id, Damage, Position)]
detectarImpactos rs ps =
  [ (idR r, idP p, damage (commonP p), position (commonP p))
  | r <- rs, healthR r > 0
  , p <- ps
  , idR r /= idP p
  , checkCollision (position (commonP p)) projectileRadius (ninoBox r)
  ]

aplicarDaño :: [Robot] -> [(Id, Id, Damage, Position)] -> [Robot]
aplicarDaño rs impactos =
  [ if any (\(idr, _, _, _) -> idr == idR r) impactos
      then let totalDaño = sum [ d | (idr, _, d, _) <- impactos, idr == idR r ]
           in r { healthR = max 0 (healthR r - totalDaño)
                , haveExploded = haveExploded r || healthR r - totalDaño <= 0
                }
      else r
  | r <- rs
  ]

generarExplosiones :: [(Id, Id, Damage, Position)] -> [Explosion]
generarExplosiones impactos =
  [ Explosion pos (30, 0) 0.6 (RobotHitByProjectile rid pid dmg pos)
  | (rid, pid, dmg, pos) <- impactos
  ]