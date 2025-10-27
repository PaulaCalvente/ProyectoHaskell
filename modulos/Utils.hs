module Utils where

import Graphics.Gloss hiding (Point, Vector) --Se omite Point y Vector para evitar conflictos con nuestros módulos
import Data.Explosion
import Data.Mundo
import Data.Proyectil
import Data.Torreta
import Data.DatosComunes
import Data.Robot

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
