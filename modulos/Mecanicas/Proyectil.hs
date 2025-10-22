module Mecanicas.Proyectil where

import Data.Explosion
import Data.Mundo
import Data.Proyectil
import Data.Robot
import Data.Torreta
import Data.DatosComunes

generarDisparo :: Float -> Robot -> Robot -> (Projectile, Robot)
generarDisparo dt r objetivo =
  let (xM, yM) = position (commonR r)
      (xT, yT) = position (commonR objetivo)
      angRad = atan2 (yT - yM) (xT - xM)
      speed = 250
      vx = cos angRad * speed
      vy = sin angRad * speed
      cooldown' = case idR r of
                    1 -> 0.6
                    2 -> 1.6
                    3 -> 1.2
                    4 -> 0.9
                    _ -> 1.0
      baseProj = projectileT (turret r)
      p = Projectile
            { idP = idR r
            , commonP = (commonP baseProj) { position = (xM, yM + 28), velocity = (vx, vy) }
            , rangeP = 1000
            }
      r' = r { turret = (turret r) { cooldown = cooldown' } }
  in (p, r')

moverProyectiles :: [Projectile] -> Float -> [Projectile]
moverProyectiles ps dt =
  [ p { commonP = (commonP p)
          { position = (x + vx * dt, y + vy * dt) } }
  | p <- ps
  , let (x, y) = position (commonP p)
        (vx, vy) = velocity (commonP p)
  , x > -ancho/2 && x < ancho/2 && y > -alto/2 && y < alto/2
  ]

filtrarProyectilesRestantes :: [Projectile] -> [(Id, Id, Damage, Position)] -> [Projectile]
filtrarProyectilesRestantes ps impactos =
  [ p | p <- ps, not (any (\(_, pid, _, _) -> pid == idP p) impactos) ]
