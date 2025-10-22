module Utils where

import Graphics.Gloss hiding (Point, Vector) --Se omite Point y Vector para evitar conflictos con nuestros módulos
import Data.Explosion
import Data.Mundo
import Data.Proyectil
import Data.Robot
import Data.Torreta
import Data.DatosComunes


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




circleAABB :: (Float,Float) -> Float -> ((Float,Float),(Float,Float)) -> Bool
circleAABB (cx,cy) r ((minx,miny),(maxx,maxy)) =
  dx*dx + dy*dy <= r*r
  where clx = max minx (min cx maxx)
        cly = max miny (min cy maxy)
        dx  = cx - clx
        dy  = cy - cly

ninoBox :: Robot -> ((Float,Float),(Float,Float))
ninoBox r = 
  let (x, y) = position (commonR r)
  in ((x-20,y-25),(x+20,y+25))

