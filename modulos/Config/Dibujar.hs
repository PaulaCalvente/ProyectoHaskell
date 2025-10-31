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
      , Pictures (map (dibujarProjectile m) (projectiles w))
      , Pictures (map (dibujarExplosion m) (explosiones m))
      , dibujarEscritorios m
      , dibujarComida m
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

dibujarEscritorios :: MundoGloss -> Picture
dibujarEscritorios m = 
  case imagenEscritorio m of
    Nothing -> Blank
    Just escritorioPic ->
      let
        escala = 0.2
        escritorioEscalado = Scale escala escala escritorioPic

        -- Coordenadas de los 4 escritorios
        posiciones =
          [ (-250, 0)   -- arriba izquierda
          , (250, 0)    -- arriba derecha
          , (-250, -200)  -- abajo izquierda
          , (250, -200)   -- abajo derecha
          ]
      in Pictures [Translate x y escritorioEscalado | (x, y) <- posiciones]

dibujarComida :: MundoGloss -> Picture
dibujarComida m =
  let maybeZumoPic     = imagenZumo m
      maybePlatanoPic  = imagenPlatano m
      maybeSandwichPic = imagenSandwich m
      escala = 0.15

      posicionesZumo     = [posZumo1 m,     posZumo2 m]
      posicionesPlatano  = [posPlatano1 m,  posPlatano2 m]
      posicionesSandwich = [posSandwich1 m, posSandwich2 m]

      crearImagen :: Maybe Picture -> [(Float, Float)] -> [Picture]
      crearImagen Nothing _ = []
      crearImagen (Just img) ps =
        [ Translate x y (Scale escala escala img) | (x, y) <- ps ]

      zumos      = crearImagen maybeZumoPic posicionesZumo
      platanos   = crearImagen maybePlatanoPic posicionesPlatano
      sandwiches = crearImagen maybeSandwichPic posicionesSandwich
  in
      Pictures (zumos ++ platanos ++ sandwiches)




dibujarRobot :: MundoGloss -> Robot -> Picture
dibujarRobot m r =
  let
    (x, y) = position (commonR r)
    ang = angleT (turret r)

    -- Selecciona la imagen del robot según su id
    maybeRobotPic = seleccionarImagenRobot m (idR r)
    maybeTorretaPic = imagenTorreta m
    
    torretaOffsetX = 5
    torretaOffsetY = 7
    torretaLength = 150 * 0.3 

    -- Dibuja la torreta (o una por defecto)
    torretaComponent =
      case maybeTorretaPic of
        Nothing ->
          Color blue (rectangleSolid 30 15)
        Just torretaPic ->
          let
            torretaEscalada = Scale 0.15 0.15 torretaPic
            pivoteTrasladado = Translate (-torretaLength / 2) 0 torretaEscalada 
            torretaRotada = Rotate (-ang) pivoteTrasladado
          in
            Translate torretaOffsetX torretaOffsetY torretaRotada

    -- Dibuja el cuerpo del robot (o un círculo por defecto)
    cuerpoComponent =
      case maybeRobotPic of
        Nothing -> Color red (Circle 10)
        Just robotPic -> Scale 0.3 0.3 robotPic

    robotPicture = Pictures [cuerpoComponent, torretaComponent]
  in
    Translate x y robotPicture

-- Selecciona la imagen del robot según su id (1..4)
seleccionarImagenRobot :: MundoGloss -> Int -> Maybe Picture
seleccionarImagenRobot m n =
  case n of
    1 -> imagenRobot1 m
    2 -> imagenRobot2 m
    3 -> imagenRobot3 m
    4 -> imagenRobot4 m
    _ -> imagenRobot1 m  -- Por defecto, usa la 1 si el id no coincide

dibujarProjectile :: MundoGloss -> Projectile -> Picture
dibujarProjectile m p =
  let
    (x, y) = position (commonP p)
    r = 8 + 2 * sin (x / 30)
    maybeProyectilPic = imagenProyectil m
  in
    case maybeProyectilPic of
      Nothing ->  -- Fallback: dibuja un círculo si la imagen no cargó
        Translate x y $ Color (makeColorI 180 60 180 230) $ circleSolid r
      Just proyectilPic ->
        let
          escala = 0.08
          proyectilEscalado = Scale escala escala proyectilPic
        in Translate x y proyectilEscalado

dibujarExplosion :: MundoGloss -> Explosion -> Picture
dibujarExplosion m (Explosion (x, y) _ ttl src) =
  let
    esMuerte = case src of
      RobotHitByProjectile { damageHit = dmg } -> dmg == 0
      _ -> False

    imgBase
      | esMuerte  = imagenExplosionMuerte m
      | ttl > 0.4 = imagenExplosion1 m
      | ttl > 0.2 = imagenExplosion2 m
      | ttl > 0   = imagenExplosion3 m
      | otherwise = Nothing

    pic = case imgBase of
      Just img -> Scale 0.25 0.25 img
      Nothing  -> Blank
  in Translate x y pic

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
