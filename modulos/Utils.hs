module Utils where

-- Tipos básicos de geometría
type Point = (Float, Float)
type Vector = (Int, Int)
type Angle = Float
type Distance = Float
type Position = (Float, Float)
type Size = (Float, Float)

-- Funciones geométricas
distanceBetween :: Position -> Position -> Distance
distanceBetween (x1, y1) (x2, y2) =
  sqrt ((x2 - x1)^2 + (y2 - y1)^2)

angleToTarget :: Position -> Position -> Angle
angleToTarget (x1, y1) (x2, y2) =
  atan2 (y2 - y1) (x2 - x1) * 180 / pi

deg2rad :: Angle -> Angle
deg2rad a = a * pi / 180

rad2deg :: Angle -> Angle
rad2deg a = a * 180 / pi

subVec :: Vector -> Vector -> Vector
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

getVertices :: (Point, Point, Point, Point, Angle) -> [Point]
getVertices (p1, p2, p3, p4, a) =
  map (\(x, y) -> let rad = deg2rad a
                  in (x * cos rad - y * sin rad, x * sin rad + y * cos rad))
      [p1, p2, p3, p4]

dot :: Point -> Point -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

sub :: Point -> Point -> Point
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

perp :: Vector -> Vector
perp (x, y) = (-y, x)

isInBounds :: Point -> Size -> Bool
isInBounds (x, y) (width, height) =
  x >= 0 && x <= width && y >= 0 && y <= height
