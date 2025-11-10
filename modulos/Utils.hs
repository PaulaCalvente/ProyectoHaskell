module Utils where

import Graphics.Gloss hiding (Point, Vector)
import Data.Explosion
import Data.Mundo
import Data.Proyectil
import Data.Torreta
import Data.DatosComunes
import Data.Robot
import qualified Data.Map as M
import Data.Char (toLower)
import Data.List (isInfixOf)

-- Funciones geométricas
distanceBetween :: Position -> Position -> Distance
distanceBetween (x1, y1) (x2, y2) =
  sqrt . sum $ fmap (^2) [x2 - x1, y2 - y1]

angleToTarget :: Position -> Position -> Angle
angleToTarget (x1, y1) (x2, y2) =
  atan2 (y2 - y1) (x2 - x1) 

deg2rad :: Angle -> Angle
deg2rad a = a * pi / 180

rad2deg :: Angle -> Angle
rad2deg a = a * 180 / pi

subVec :: Vector -> Vector -> Vector
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

getVertices :: (Point, Point, Point, Point, Angle) -> [Point]
getVertices (p1, p2, p3, p4, a) =
  fmap rot [p1, p2, p3, p4]
  where
    rad = deg2rad a
    rot (x, y) = (x * cos rad - y * sin rad, x * sin rad + y * cos rad)

dot :: Point -> Point -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

sub :: Point -> Point -> Point
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

perp :: Vector -> Vector
perp (x, y) = (-y, x)

isInBounds :: Point -> Size -> Bool
isInBounds (x, y) (width, height) =
  x >= 0 && x <= width && y >= 0 && y <= height

mul :: Point -> Point -> Point
mul (w, h) (sw, sh) = (w * sw, h * sh)

updatePosition :: Float -> Position -> Velocity -> Position
updatePosition dt (px, py) (vx, vy) =
  (px + vx * dt, py + vy * dt)

calcularAngulo :: Robot -> Robot -> Float
calcularAngulo r objetivo =
  rad2deg (angleToTarget (positionR r) (positionR objetivo))

calcularVector :: Float -> (Float, Float)
calcularVector angDegree =
  (cos angDegree, sin angDegree)

generarRecorrido :: Id -> [Position]
generarRecorrido id = take 11 $ zip xs ys
  where
    xs = [ fromIntegral ((id * i * 1111) `mod` 500) - 250 | i <- [1..] ]
    ys = [ fromIntegral ((id * i * 713) `mod` 500) - 250 | i <- [1..] ]

perfilBaseRobot :: Id -> Robot
perfilBaseRobot id =
  case id of
    1 -> Robot
          { idR = id
          , commonR = CommonData id 0 (0,0) (0,0) (40,50) (generarRecorrido id)
          , healthR = 70
          , maxHealthR = 70
          , radarRange = 200
          , turret = Turret id (1,0) 0
                      (Projectile id (CommonData id 8 (0,0) (250,0) (projectileRadius*2, projectileRadius*2) []) 1000)
                      1.2
          , haveExploded = False
          , shooting = False
          , memory = M.empty
          }
    2 -> Robot
          { idR = id
          , commonR = CommonData id 0 (0,0) (0,0) (40,50) (generarRecorrido id)
          , healthR = 120
          , maxHealthR = 120
          , radarRange = 120
          , turret = Turret id (-1,0) 180
                      (Projectile id (CommonData id 18 (0,0) (-180,0) (projectileRadius*2, projectileRadius*2) []) 1000)
                      3.2
          , haveExploded = False
          , shooting = False
          , memory = M.empty
          }
    3 -> Robot
          { idR = id
          , commonR = CommonData id 0 (0,0) (0,0) (40,50) (generarRecorrido id)
          , healthR = 85
          , maxHealthR = 85
          , radarRange = 160
          , turret = Turret id (-1,0) 180
                      (Projectile id (CommonData id 6 (0,0) (200,0) (projectileRadius*2, projectileRadius*2) []) 1000)
                      2.4
          , haveExploded = False
          , shooting = False
          , memory = M.empty
          }
    4 -> Robot
          { idR = id
          , commonR = CommonData id 0 (0,0) (0,0) (40,50) (generarRecorrido id)
          , healthR = 85
          , maxHealthR = 85
          , radarRange = 120
          , turret = Turret id (-1,0) 180
                      (Projectile id (CommonData id 10 (0,0) (-220,0) (projectileRadius*2, projectileRadius*2) []) 1000)
                      2.0
          , haveExploded = False
          , shooting = False
          , memory = M.empty
          }
    _ -> perfilBaseRobot ((id - 1) `mod` 4 + 1)  -- fallback cíclico

nombreABaseId :: String -> Maybe Id
nombreABaseId s
  | "robot1" `isInfixOf` map toLower s = Just 1
  | "robot2" `isInfixOf` map toLower s = Just 2
  | "robot3" `isInfixOf` map toLower s = Just 3
  | "robot4" `isInfixOf` map toLower s = Just 4
  | otherwise = Nothing