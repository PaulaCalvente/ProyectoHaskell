module Collision where
import Data.Explosion
import Data.Mundo
import Data.Proyectil
import Data.Robot
import Data.Torreta

import Utils
import Movement

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
    projections = (`dot` eje) <$> pts

detectedRobotProjectileCollisions :: [Robot] -> [Projectile] -> ([RobotHit], [Explosion], Int)
detectedRobotProjectileCollisions robots proyectiles = (hits, explosions, length hits)
  where
    hits =
      [ RobotHitByProjectile
          { idRobot      = idR r
          , idProjectile = idP p
          , damageHit    = damageP p
          , hitAt        = positionP p
          }
      | (r, p) <- (,) <$> robots <*> proyectiles -- Combina en la tupla todos los robots y proyectiles
      , checkCollision (pointsR r) (pointsP p)
      ]

    explosions =
      [ Explosion
          { positionE = positionP p
          , sizeE     = (50, 50)  
          , durationE = 1.0
          , source    = RobotHitByProjectile
                        { idRobot      = idR r
                        , idProjectile = idP p
                        , damageHit    = damageP p
                        , hitAt        = positionP p
                        }
          }
      | (r, p) <- (,) <$> robots <*> proyectiles
      , checkCollision (pointsR r) (pointsP p)
      ]

detectRobotRobotCollisions :: [Robot] -> ([RobotHit], [Explosion], Int)
detectRobotRobotCollisions robots = (hits, explosions, length hits)
  where
    hits =
      [ RobotCollidedWithRobot
          { idRobot1    = idR r1
          , idRobot2    = idR r2
          , damageHit1  = damageR r1
          , damageHit2  = damageR r2
          , hitAt       = positionR r1  
          }
      | (r1, r2) <- (,) <$> robots <*> robots
      , idR r1 < idR r2
      , checkCollision (pointsR r1) (pointsR r2)
      ]

    explosions =
      [ Explosion
          { positionE = positionR r1
          , sizeE     = (60, 60)  
          , durationE = 1.0
          , source    = RobotCollidedWithRobot
                        { idRobot1    = idR r1
                        , idRobot2    = idR r2
                        , damageHit1  = damageR r1
                        , damageHit2  = damageR r2
                        , hitAt       = positionR r1
                        }
          }
      | (r1, r2) <- (,) <$> robots <*> robots
      , idR r1 < idR r2
      , checkCollision (pointsR r1) (pointsR r2)
      ]

checkCollisions :: World -> Int
checkCollisions world = totalRP + totalRR
  where
    rs = robots world
    ps = projectiles world
    totalRP = length [ () | (r, p) <- (,) <$> rs <*> ps
                          , checkCollision (pointsR r) (pointsP p) ]
    totalRR = length [ () | (r1, r2) <- (,) <$> rs <*> rs
                          , idR r1 < idR r2
                          , checkCollision (pointsR r1) (pointsR r2) ]