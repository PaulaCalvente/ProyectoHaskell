module Mecanicas.Collision where
import Data.Explosion
import Data.Mundo
import Data.Proyectil
import Data.Robot
import Data.Torreta
import Data.DatosComunes
import Data.List (find)

import Utils
import Mecanicas.Movement
import Mecanicas.Robot (robotBox)  

{--
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
          , hitPosition        = positionP p
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
                        , hitPosition        = positionP p
                        }
          }
      | (r, p) <- (,) <$> robots <*> proyectiles
      , checkCollision (pointsR r) (pointsP p)
      ]
--}
detectRobotRobotCollisions :: [Robot] -> ([RobotHit], [Explosion], Int)
detectRobotRobotCollisions robots = (hits, explosions, length hits)
  where
    distanciaCorta r1 r2 =
      distanceBetween (positionR r1) (positionR r2) < 45 

    boxesCollide ((minx1,miny1),(maxx1,maxy1)) ((minx2,miny2),(maxx2,maxy2)) =
      not (maxx1 < minx2 || minx1 > maxx2 || maxy1 < miny2 || miny1 > maxy2)

    hits =
      [ RobotCollidedWithRobot
          { idRobot1    = idR r1
          , idRobot2    = idR r2
          , damageHit1  = damageR r1
          , damageHit2  = damageR r2
          , hitPosition = positionR r1
          }
      | (r1, r2) <- (,) <$> robots <*> robots
      , idR r1 < idR r2
      , boxesCollide (robotBox r1) (robotBox r2) || distanciaCorta r1 r2
      ]

    explosions =
      [ Explosion
          { positionE = positionR r1
          , sizeE     = (60, 60)
          , durationE = 0.6
          , source    = RobotCollidedWithRobot
                        { idRobot1    = idR r1
                        , idRobot2    = idR r2
                        , damageHit1  = damageR r1
                        , damageHit2  = damageR r2
                        , hitPosition = positionR r1
                        }
          , idA       = idR r1
          , idB       = idR r2
          }
      | (r1, r2) <- (,) <$> robots <*> robots
      , idR r1 < idR r2
      , boxesCollide (robotBox r1) (robotBox r2) || distanciaCorta r1 r2
      ]
{--
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
--}
checkCollision :: (Float,Float) -> Float -> ((Float,Float),(Float,Float)) -> Bool
checkCollision (cx,cy) r ((minx,miny),(maxx,maxy)) =
  dx*dx + dy*dy <= r*r
  where clx = max minx (min cx maxx)
        cly = max miny (min cy maxy)
        dx  = cx - clx
        dy  = cy - cly

-- normaliza una pareja a orden (min,max) para evitar duplicados (1,2) == (2,1)
normalizePair :: CollisionPair -> CollisionPair
normalizePair (a,b) = if a <= b then (a,b) else (b,a)

-- resta dt a todos los TTL y filtra expirados
tickRecentCollisions :: Float -> RecentCollisions -> RecentCollisions
tickRecentCollisions dt = filter (\(_, ttl) -> ttl > 0) . map (\(p, ttl) -> (p, ttl - dt))

-- comprueba si la pareja ya está en recentCollisions
pairAlreadyRecent :: CollisionPair -> RecentCollisions -> Bool
pairAlreadyRecent p recent = any ((== normalizePair p) . fst) recent

-- detecta pares de robots que colisionan (devuelve pares normalizados)
detectCollisionPairs :: [Robot] -> [CollisionPair]
detectCollisionPairs robots =
  [ normalizePair (idR r1, idR r2)
  | (r1, r2) <- (,) <$> robots <*> robots
  , idR r1 < idR r2
  , let box1 = robotBox r1
        box2 = robotBox r2
  , boxesCollide box1 box2 || distanceBetween (positionR r1) (positionR r2) < 45
  ]
  where
    boxesCollide ((minx1,miny1),(maxx1,maxy1)) ((minx2,miny2),(maxx2,maxy2)) =
      not (maxx1 < minx2 || minx1 > maxx2 || maxy1 < miny2 || miny1 > maxy2)

-- con los pares actuales y recentCollisions, devuelve:
--   (nuevosPairs, nuevosRecent) : nuevosPairs son los pares que NO estaban en recent; nuevosRecent es recent actualizado con las nuevas entradas (ttlNew)
detectNewPairs :: Float -> [Robot] -> RecentCollisions -> ([CollisionPair], RecentCollisions)
detectNewPairs ttlNew robots recent =
  let currentPairs = detectCollisionPairs robots
      isNew p = not (pairAlreadyRecent p recent)
      newPairs = filter isNew currentPairs
      newEntries = [(p, ttlNew) | p <- newPairs]
      updatedRecent = recent ++ newEntries
  in (newPairs, updatedRecent)


-- crea Explosion para una pareja (buscamos ambos robots para posición/tamaño/damage si quieres)
explosionFromPair :: [Robot] -> (Id, Id) -> Explosion
explosionFromPair robots (a,b) =
  let Just r1 = find (\r -> idR r == a) robots
      Just r2 = find (\r -> idR r == b) robots
      pos = positionR r1
      src = RobotCollidedWithRobot
              { idRobot1 = a
              , idRobot2 = b
              , damageHit1 = damageR r1
              , damageHit2 = damageR r2
              , hitPosition = pos
              }
  in Explosion pos (60,60) 0.9 src a b