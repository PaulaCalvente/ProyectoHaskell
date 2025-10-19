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
  dx*dx + dy*dy <= r*r
  where clx = max minx (min cx maxx)
        cly = max miny (min cy maxy)
        dx  = cx - clx
        dy  = cy - cly

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
      ang = angleT (turret r)
      rad = deg2rad ang
      len = 25
      grosor = 4
      mouthPos = (10, 28)
      (mx, my) = mouthPos
      strawEnd = (mx + cos rad * len, my + sin rad * len)
      strawColor = makeColorI 200 200 200 255
  in Translate x y $ Pictures
       [ Color c $ rectangleSolid 40 50
       , Translate 0 35 $ Color (makeColorI 255 220 180 255) $ rectangleSolid 30 30
       , Translate 0 50 $ Color (makeColorI 90 60 20 255) $ rectangleSolid 32 8
       , Translate (-8) 40 $ Color black $ circleSolid 2.5
       , Translate (8) 40 $ Color black $ circleSolid 2.5
       , Translate 0 28 $ Color red $ rectangleSolid 8 2
       , Translate 0 (-45) $ Color black $ rectangleSolid 30 10
       , Translate 18 28 $ Color (makeColorI 180 60 180 230) $ circleSolid 6
       , Translate mx my $
           Color strawColor $
             Rotate (-ang) $
               Translate (len/2) 0 $ rectangleSolid len grosor
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

-- 💥 Burbuja que crece y se desvanece antes de desaparecer
dibujarBurbujaMuerte :: BurbujaMuerte -> Picture
dibujarBurbujaMuerte (BurbujaMuerte (x,y) ttl) =
  let baseRadio = 30
      -- efecto de “crecer” cuando queda menos de 1 segundo
      grow = if ttl < 1 then 1 + (1 - ttl) * 0.5 else 1
      alpha = if ttl < 1 then ttl else 1  -- se desvanece suavemente
      cBorde   = makeColor 1 0.4 1 (0.6 * alpha)
      cRelleno = makeColor 1 0.7 1 (0.3 * alpha)
  in Translate x y $
       Pictures
         [ Color cRelleno $ circleSolid (baseRadio * grow)
         , Color cBorde   $ thickCircle (baseRadio * 0.8 * grow) (5 * grow)
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

data Modo = Inicio | Jugando | Victoria Int deriving (Eq, Show)

------------------------------------------------------------
-- BARRAS DE VIDA 
------------------------------------------------------------

-- Colores coherentes para cada jugador
colorJugador :: Robot -> Color
colorJugador r = case idR r of
  1 -> orange
  2 -> blue
  3 -> red
  4 -> green
  _ -> white

------------------------------------------------------------
-- PANEL IZQUIERDO: HUD (pegado totalmente al borde)
------------------------------------------------------------
dibujarHUD :: [Robot] -> Picture
dibujarHUD rs =
  let num = length rs
      panelW = 200
      panelH = fromIntegral num * 45 + 40
      -- 💡 Ahora completamente pegado al borde izquierdo
      panelX = -ancho / 2 + panelW / 2 - 10
      panelY = alto / 2 - panelH / 2 - 20

      fondo = Color (makeColor 0 0 0 0.4) $
                 Translate panelX panelY $
                   rectangleSolid panelW panelH

      barras = Pictures
        [ dibujarBarraVidaVerticalAt panelX panelY r i
        | (i, r) <- zip [0..] rs ]
  in Pictures [fondo, barras]


-- Barra con su etiqueta encima (misma organización, solo desplazada)
dibujarBarraVidaVerticalAt :: Float -> Float -> Robot -> Int -> Picture
dibujarBarraVidaVerticalAt panelX panelY r idx =
  let vida        = healthR r
      anchoTotal  = 120
      altoBarra   = 14
      anchoVida   = max 0 (min 1 (vida / 100)) * anchoTotal
      colorN      = colorJugador r
      -- posiciones ajustadas para quedar dentro del panel pegado al borde
      baseY       = (panelY + 60) - fromIntegral idx * 45
      baseX       = panelX - 85
  in Pictures
       [ Translate baseX (baseY + 10) $
           Scale 0.15 0.15 $
             Color white $
               Text ("Alumno " ++ show (idR r))
       , Translate (panelX - 15) baseY $
           Pictures
             [ Color white $ rectangleWire (anchoTotal + 4) (altoBarra + 4)
             , Color (greyN 0.3) $ rectangleSolid anchoTotal altoBarra
             , Translate (-(anchoTotal - anchoVida)/2) 0 $
                 Color colorN $ rectangleSolid anchoVida altoBarra
             ]
       ]
