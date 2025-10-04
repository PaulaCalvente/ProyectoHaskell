-- Todo lo relacionado con HAUS3 (SAT y colisiones)

module Collision where

import Types
import Utils

-- SAT: Separating Axis Theorem
checkCollision :: [Point] -> [Point] -> Bool
checkCollision rect1 rect2 = all (\axis -> superposicionPorEje rect1 rect2 axis) ejes
  where
    ejes = getAxes rect1 ++ getAxes rect2

    getAxes :: [Point] -> [Vector]
    getAxes pts = [perp (sub p2 p1) | (p1, p2) <- zip pts (tail pts ++ [head pts])]

    superposicionPorEje :: [Point] -> [Point] -> Vector -> Bool
    superposicionPorEje p1 p2 eje =
        not (max1 < min2 || max2 < min1)
        where
            (min1, max1) = projectPolygon p1 eje
            (min2, max2) = projectPolygon p2 eje

    projectPolygon :: [Point] -> Vector -> (Float, Float)
    projectPolygon pts eje =
        (minimum projections, maximum projections)
        where   
            projections = map (`dot` eje) pts

-- Funciones auxiliares para vectores
sub :: Point -> Point -> Vector
sub (x1,y1) (x2,y2) = (x1-x2, y1-y2)

dot :: Vector -> Vector -> Float
dot (x1,y1) (x2,y2) = x1*x2 + y1*y2

perp :: Vector -> Vector
perp (x,y) = (-y,x)


-- Colisiones Robot-Proyectil
detectedRobotProjectileCollisions :: [Robot] -> [Projectile] -> ([RobotHit], Int)
detectedRobotProjectileCollisions robots proyectiles = (hits, total)
  where
    hits =
      [ RobotHitByProjectile
          { idRobot      = idR r  
          , idProjectile = idP p
          , damageHit    = damageP p
          , hitAt        = positionP p
          }
      | r <- robots
      , p <- proyectiles
      , checkCollision (vertexRobot r) (vertexProjectile p)
      ]
    total = length hits

-- Colisiones Robot-Robot
detectRobotRobotCollisions :: [Robot] -> ([RobotHit], Int)
detectRobotRobotCollisions robots = (hits, total)
  where
    hits =
      [ RobotCollidedWithRobot
          { idRobot1    = idR r1
          , idRobot2    = idR r2
          , damageHit1  = damageR r1
          , damageHit2  = damageR r2
          , hitAt       = positionR r1
          }
      | r1 <- robots
      , r2 <- robots
      , idR r1 < idR r2
      , checkCollision (vertexRobot r1) (vertexRobot r2)
      ]
    total = length hits

-- Función principal de chequeo
checkCollisions :: World -> Int
checkCollisions world = totalRP + totalRR
  where
    rs = robots world
    ps = projectiles world
    totalRP = length [ () | r <- rs, p <- ps
                          , checkCollision (vertexRobot r) (vertexProjectile p) ]
    totalRR = length [ () | r1 <- rs, r2 <- rs
                          , idR r1 < idR r2
                          , checkCollision (vertexRobot r1) (vertexRobot r2) ]
