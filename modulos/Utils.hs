module Utils where

import Graphics.Gloss hiding (Point, Vector)
import Types


-- Funciones geométricas
distanceBetween :: Position -> Position -> Distance
distanceBetween (x1, y1) (x2, y2) =
  sqrt . sum $ fmap (^2) [x2 - x1, y2 - y1]

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




-----------------------------------------------------------------------------

-- FUNCIONES NECESARIAS PARA HAUS5

-----------------------------------------------------------------------------

ancho, alto :: Float
ancho = 600
alto  = 600

velNino, velChicleVel :: Float
velNino = 150
velChicleVel = 300

chicleRadius :: Float
chicleRadius = 10

circleAABB :: (Float,Float) -> Float -> ((Float,Float),(Float,Float)) -> Bool
circleAABB (cx,cy) r ((minx,miny),(maxx,maxy)) =
  let clx = max minx (min cx maxx)
      cly = max miny (min cy maxy)
      dx  = cx - clx
      dy  = cy - cly
  in dx*dx + dy*dy <= r*r

ninoBox :: Robot -> ((Float,Float),(Float,Float))
ninoBox r = 
  let (x, y) = position (commonR r)
  in ((x-20,y-25),(x+20,y+25))


dibujarProfe :: (Float, Float) -> Picture
dibujarProfe (x,y) = Translate x y $ Pictures
  [ Color (makeColorI 255 220 180 255) $ Translate 0 40 $ rectangleSolid 40 40
  , Translate 0 55 $ Color (makeColorI 80 50 10 255) $ rectangleSolid 42 10
  , Color (makeColorI 40 70 160 255) $ rectangleSolid 50 60
  , Translate 0 (-50) $ Color (makeColorI 30 40 80 255) $ rectangleSolid 40 25
  , Translate (-10) 45 $ Color black $ circleSolid 3
  , Translate (10) 45 $ Color black $ circleSolid 3
  , Translate 0 32 $ Color red $ rectangleSolid 10 3
  ]


dibujarNino :: Robot -> Picture
dibujarNino r = 
  let (x, y) = position (commonR r)
      c = case idR r of
            1 -> orange
            2 -> blue
            3 -> red
            4 -> green
            _ -> white
  in Translate x y $ Pictures
     [ Color c $ rectangleSolid 40 50
     , Translate 0 35 $ Color (makeColorI 255 220 180 255) $ rectangleSolid 30 30
     , Translate 0 50 $ Color (makeColorI 90 60 20 255) $ rectangleSolid 32 8
     , Translate (-8) 40 $ Color black $ circleSolid 2.5
     , Translate (8) 40 $ Color black $ circleSolid 2.5
     , Translate 0 28 $ Color red $ rectangleSolid 8 2
     , Translate 0 (-45) $ Color black $ rectangleSolid 30 10
     , Translate 18 28 $ Color (makeColorI 180 60 180 230) $ circleSolid 6
     ]

dibujarChicle :: Projectile -> Picture
dibujarChicle p = 
  let (x, y) = position (commonP p)
      c = makeColorI 180 60 180 230
      r = 8 + 2 * sin (x / 30)
  in Translate x y $ Color c $ circleSolid r

dibujarExplosion :: Explosion -> Picture
dibujarExplosion (Explosion (x,y) _ ttl _) =
  let a = max 0 (min 1 (ttl / 0.6))
  in Translate x y $
       Pictures
         [ Color (withAlpha a (makeColorI 255 120 200 255)) $ thickCircle (30*0.6) (30*0.25)
         , Color (withAlpha (a*0.8) (makeColorI 255 200 255 255)) $ circleSolid (30*0.3)
         ]

dibujarBoton :: Picture
dibujarBoton = Pictures
  [ Translate (-85) (-140) $ Scale 0.2 0.2 $ Color black $ Text "Iniciar Juego"
  ]

dentroBoton :: (Float, Float) -> Bool
dentroBoton (mx, my) =
  mx >= -115 && mx <= 85 && my >= -185 && my <= -95

-- ================================
-- Estado del juego
-- ================================

data ModoJuego = Inicio | Jugando deriving (Show, Eq)
