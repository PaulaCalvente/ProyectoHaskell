module Config.Dibujar where

import Graphics.Gloss hiding (Vector, Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector, Point)

import Data.Explosion
import Data.Mundo
import Data.Proyectil
import Data.Robot
import Data.Torreta
import Data.DatosComunes
import Data.Maybe (fromMaybe)

import Mecanicas.Robot

import Utils

dibujar :: MundoGloss -> Picture
dibujar m = case modo m of
  Inicio  -> Pictures [ imagenInicio m, dibujarBoton ]
  Jugando ->
    let w = worldState m
    in Pictures
      [ fondoJuego m
      , dibujarRobotP m (0, 240)
      , Pictures (map (dibujarRobot m) [r | r <- robots w, healthR r > 0])
      , Pictures (map dibujarProjectile (projectiles w))
      , Pictures (map dibujarExplosion (explosiones m))
      , dibujarHUD (robots w)
      , dibujarPutInfo m
      ]

  Victoria rid ->
    Pictures
      [ imagenVictoria m
      , Translate (-240) 135 $
          Scale 0.27 0.27 $
          Color black $
          Text ("Alumno " ++ show rid ++ " es el ganador")
      ]
  Derrota ->
    Pictures [imagenDerrota m]

dibujarPutInfo :: MundoGloss -> Picture
dibujarPutInfo m =
  let w = worldState m
      vivos = countActiveRobots (robots w)
      proyectilesActivos = length (projectiles w)
      exps = length (explosiones m)
      infoLines = [ "INFORMACION", "Alumnos vivos: " ++ show vivos, "Chicles activos: " ++ show proyectilesActivos, "Explosiones: " ++ show exps ]
      fondo = Color (makeColor 0 0 0 0.6) $ Translate 330 235 $ rectangleSolid 250 120
      linePictures = [ Translate 230 (260 - fromIntegral i * 25) $ Scale 0.15 0.15 $ Color white $ Text line | (i, line) <- zip [0..] infoLines ]
  in Pictures (fondo : linePictures)

dibujarRobotP :: MundoGloss -> (Float, Float) -> Picture
dibujarRobotP m (x, y) =
  Translate x y $
    Scale 0.3 0.3 $
      fromMaybe Blank (imagenProfe m)

dibujarRobot :: MundoGloss -> Robot -> Picture
dibujarRobot m r =
  let (x, y) = position (commonR r)
      robotEscalado   = Scale 0.3 0.3 (imagenRobot1 m) 
      torretaEscalada = Scale 0.15 0.15 (imagenTorreta m)
      ang = angleT (turret r)
  in Translate x y $ Pictures
       [ robotEscalado
       , Rotate (-ang) torretaEscalada
       ]

dibujarProjectile :: Projectile -> Picture
dibujarProjectile p = 
  let (x, y) = position (commonP p)
      c = makeColorI 180 60 180 230
      r = 8 + 2 * sin (x / 30)
  in Translate x y $ Color c $ circleSolid r

dibujarExplosion :: Explosion -> Picture
dibujarExplosion (Explosion (x,y) _ tiempoTotal _) =
  let a = max 0 (min 1 (tiempoTotal / 0.6))
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

------------------------------------------------------------
-- PANEL IZQUIERDO: HUD
------------------------------------------------------------
dibujarHUD :: [Robot] -> Picture
dibujarHUD rs =
  let num = length rs
      panelWide = 200
      panelHeight = fromIntegral num * 45 + 40
      panelWideRel = -ancho / 2 + panelWide / 2 - 10
      panelHeightRel = alto / 2 - panelHeight / 2 - 20
      fondo = Color (makeColor 0 0 0 0.6) $
                 Translate (panelWideRel - 130) (panelHeightRel + 10) $
                   rectangleSolid panelWide panelHeight
      barras = Pictures
        [ dibujarBarraVida panelWideRel panelHeightRel r i
        | (i, r) <- zip [0..] rs ]
  in Pictures [fondo, barras]


dibujarBarraVida :: Float -> Float -> Robot -> Int -> Picture
dibujarBarraVida panelWideRel panelHeightRel r idx =
  let vida        = healthR r
      anchoTotal  = 120
      altoBarra   = 14
      anchoVida   = max 0 (min 1 (vida / maxHealthR r)) * anchoTotal
      baseY       = (panelHeightRel + 60) - fromIntegral idx * 45
      colorTexto  = if vida <= 0
                    then (makeColorI 255 130 130 255)
                    else white
      nombreTxt   = if vida <= 0
                    then "Muerto"
                    else "Alumno " ++ show (idR r)
      vidaTxt = show (round vida)
      vidaX = panelWideRel - 75
      in Pictures
       [ Translate (panelWideRel - 220) (baseY + 30) $
          Scale 0.15 0.15 $ Color colorTexto $ Text nombreTxt -- Info de cada niño
       , Translate (panelWideRel - 150) (baseY + 15) $ -- La vida de cada niño
           Pictures
             [ Color white $ rectangleWire (anchoTotal + 4) (altoBarra + 4)
             , Color (greyN 0.3) $ rectangleSolid anchoTotal altoBarra
             , Translate (-(anchoTotal - anchoVida)/2) 0 $
                 Color red $ rectangleSolid anchoVida altoBarra
             ]
       , Translate vidaX (baseY + 5) $
           Scale 0.15 0.15 $ Color white $ Text vidaTxt -- Vida de cada niño
       ]
