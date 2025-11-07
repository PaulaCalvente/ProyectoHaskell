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

------------------------------------------------------------
-- DIBUJADO PRINCIPAL
------------------------------------------------------------
------------------------------------
limitesEscritorios :: [((Float, Float), (Float, Float))]
limitesEscritorios =
  [ ((-300, -200), (-40,  40))
  , ((-300, -200), (-240, -160))
  , ((200,  300),  (-40,   40))
  , ((200,  300),  (-240, -160))
  ]

-- Verifica si una posición está dentro de algún escritorio usando límites absolutos
estaDentroDeEscritorio :: (Float, Float) -> Bool
estaDentroDeEscritorio (x, y) =
  any (\((minX, maxX), (minY, maxY)) ->
        x >= minX && x <= maxX &&
        y >= minY && y <= maxY)
      limitesEscritorios


dibujar :: MundoGloss -> Picture
dibujar m = case modo m of
  Inicio  -> Pictures [ imagenInicio m, dibujarBoton ]

  Jugando ->
    let w = worldState m
    in Pictures
      [ fondoJuego m
      , dibujarRobotP m (posicionProfesor m)     -- profesor (usa normal o enfadado)
      , dibujarCuentaAtrasProfesor m             -- contador visible
      , Pictures (map (dibujarRobot m) [r | r <- robots w, healthR r > 0])
      , Pictures (map (dibujarProjectile m) (projectiles w))
      , Pictures (map (dibujarExplosion m) (explosiones m))
      , dibujarComida m
      , dibujarHUD (robots w)
      , dibujarPutInfo m
      , dibujarEscritorios m
      ]

  Victoria rid ->
    Pictures
      [ imagenVictoria m
      , Translate (-240) 135 $
          Scale 0.27 0.27 $
          Color black $
          Text ("Alumno " ++ show rid ++ " es el ganador")
      , dibujarBotonReiniciar
      ]

  Derrota -> Pictures [imagenDerrota m, dibujarBotonReiniciar]

------------------------------------------------------------
-- INFORMACIÓN SUPERIOR DERECHA
------------------------------------------------------------
dibujarPutInfo :: MundoGloss -> Picture
dibujarPutInfo m =
  let w = worldState m
      vivos = countActiveRobots (robots w)
      proyectilesActivos = length (projectiles w)
      exps = length (explosiones m)
      infoLines =
        [ "INFORMACION"
        , "Alumnos vivos: " ++ show vivos
        , "Chicles activos: " ++ show proyectilesActivos
        , "Explosiones: " ++ show exps
        ]
      fondo = Color (makeColor 0 0 0 0.6) $
                Translate 330 235 $ rectangleSolid 250 120
      linePictures =
        [ Translate 230 (260 - fromIntegral i * 25)
            $ Scale 0.15 0.15
            $ Color white
            $ Text line
        | (i, line) <- zip [0..] infoLines ]
  in Pictures (fondo : linePictures)

------------------------------------------------------------
-- DIBUJAR PROFESOR (usa imagen enfadada si está activo)
------------------------------------------------------------
dibujarRobotP :: MundoGloss -> (Float, Float) -> Picture
dibujarRobotP m (x, y) =
  let imgProfe = if profesorActivo m
                   then imagenProfeEnfadado m   -- nueva imagen si está enfadado
                   else imagenProfe m
  in Translate x (y + 10) $ Scale 0.3 0.3 $ fromMaybe Blank imgProfe


------------------------------------------------------------
-- CONTADOR SOBRE EL PROFESOR
------------------------------------------------------------
-- Contador sobre el profesor
dibujarCuentaAtrasProfesor :: MundoGloss -> Picture
dibujarCuentaAtrasProfesor m
  | not (profesorActivo m) = Blank
  | otherwise =
      let (x, y) = posicionProfesor m
          t = max 0 (tiempoExplosionProfesor m)
          n = ceiling t :: Int
          progreso = 1 - (t / 3)
          pulsacion = 1 + 0.05 * sin (t * 10)
          energia = 0.4 + 0.6 * (sin (t * 6) ** 2)

          -- Colores dinámicos
          colorPrincipal
            | t <= 1 = makeColor 1 0.2 0.2 0.9   -- rojo intenso al final
            | t <= 2 = makeColor 1 0.6 0.1 0.9   -- naranja
            | otherwise = makeColor 0.1 0.9 1 0.9 -- azul eléctrico
          colorBrillo = makeColor 1 1 1 0.3

          radioBase = 40 * pulsacion
          grosor = 8

          -- Anillo externo que late con energía
          anilloExterno =
            Color colorPrincipal $ ThickCircle (radioBase * 0.7) grosor

          -- Círculo interior translúcido con brillo
          nucleo =
            Color (makeColor 1 1 1 (0.2 + 0.2 * sin (t * 5))) $
              circleSolid (radioBase * 0.45)

          -- Halo difuso que se expande
          halo =
            Color (makeColor 0.3 0.9 1 (0.2 * energia)) $
              ThickCircle (radioBase * 0.9) (radioBase * 0.4)

          -- Número digital centrado y brillante
          textoPrincipal =
            let texto = show n
                escala = 0.45
                offset = fromIntegral (length texto) * (-15)
            in Pictures
                 [ Translate (offset - 3) (-15)
                     $ Scale escala escala
                     $ Color colorPrincipal
                     $ Text texto
                 , Translate (offset - 3) (-15)
                     $ Scale escala escala
                     $ Color colorBrillo
                     $ Text texto
                 ]
      in Translate x (y + 105)
           $ Pictures [halo, nucleo, anilloExterno, textoPrincipal]



------------------------------------------------------------
-- ESCRITORIOS
------------------------------------------------------------
dibujarEscritorios :: MundoGloss -> Picture
dibujarEscritorios m =
  case imagenEscritorio m of
    Nothing -> Blank
    Just escritorioPic ->
      let escala = 0.2
          escritorioEscalado = Scale escala escala escritorioPic
          posiciones =
            [ (-250, 0)
            , (250, 0)
            , (-250, -200)
            , (250, -200)
            ]
      in Pictures [Translate x y escritorioEscalado | (x, y) <- posiciones]

------------------------------------------------------------
-- COMIDA
------------------------------------------------------------
dibujarComida :: MundoGloss -> Picture
dibujarComida m =
  let maybeZumoPic     = imagenZumo m
      maybePlatanoPic  = imagenPlatano m
      maybeSandwichPic = imagenSandwich m
      escala = 0.15

      crearImagen :: Bool -> Maybe Picture -> (Float, Float) -> [Picture]
      crearImagen activo (Just img) pos
        | activo    = [Translate (fst pos) (snd pos) (Scale escala escala img)]
        | otherwise = []
      crearImagen _ Nothing _ = []

      zumos      = crearImagen (zumo1Activo m) maybeZumoPic (posZumo1 m)
                 ++ crearImagen (zumo2Activo m) maybeZumoPic (posZumo2 m)
      platanos   = crearImagen (platano1Activo m) maybePlatanoPic (posPlatano1 m)
                 ++ crearImagen (platano2Activo m) maybePlatanoPic (posPlatano2 m)
      sandwiches = crearImagen (sandwich1Activo m) maybeSandwichPic (posSandwich1 m)
                 ++ crearImagen (sandwich2Activo m) maybeSandwichPic (posSandwich2 m)
  in Pictures (zumos ++ platanos ++ sandwiches)

------------------------------------------------------------
-- ROBOTS Y TORRETAS
------------------------------------------------------------
dibujarRobot :: MundoGloss -> Robot -> Picture
dibujarRobot m r =
  let (x, y) = position (commonR r)
      ang = angleT (turret r)
      maybeRobotPic = seleccionarImagenRobot m (idR r)
      maybeTorretaPic = imagenTorreta m

      torretaOffsetX = 5
      torretaOffsetY = 7
      torretaLength = 150 * 0.3

      torretaComponent =
        case maybeTorretaPic of
          Nothing -> Color blue (rectangleSolid 30 15)
          Just torretaPic ->
            let torretaEscalada = Scale 0.15 0.15 torretaPic
                pivoteTrasladado = Translate (torretaLength / 2) 0 torretaEscalada
                torretaRotada = Rotate (-ang) pivoteTrasladado
            in Translate torretaOffsetX torretaOffsetY torretaRotada

      cuerpoComponent =
        case maybeRobotPic of
          Nothing -> Color red (Circle 10)
          Just robotPic -> Scale 0.3 0.3 robotPic

      robotPicture = Pictures [cuerpoComponent, torretaComponent]
  in Translate x y robotPicture

seleccionarImagenRobot :: MundoGloss -> Int -> Maybe Picture
seleccionarImagenRobot m n =
  case n of
    1 -> imagenRobot1 m
    2 -> imagenRobot2 m
    3 -> imagenRobot3 m
    4 -> imagenRobot4 m
    _ -> imagenRobot1 m

------------------------------------------------------------
-- PROYECTILES
------------------------------------------------------------
dibujarProjectile :: MundoGloss -> Projectile -> Picture
dibujarProjectile m p =
  let (x, y) = position (commonP p)
      r = 8 + 2 * sin (x / 30)
      maybeProyectilPic = imagenProyectil m
  in case maybeProyectilPic of
       Nothing ->
         Translate x y $ Color (makeColorI 180 60 180 230) $ circleSolid r
       Just proyectilPic ->
         let escala = 0.08
         in Translate x y (Scale escala escala proyectilPic)

------------------------------------------------------------
-- EXPLOSIONES (incluye profesor explosivo)
------------------------------------------------------------
dibujarExplosion :: MundoGloss -> Explosion -> Picture
dibujarExplosion m (Explosion (x, y) _ ttl src _ _) =
  let
    esMuerte = case src of
      RobotHitByProjectile { damageHit = dmg } -> dmg == 0
      _ -> False

    esObstaculoComida = case src of
      RobotHitByProjectile { idProjectile = pid } -> pid == -1
      _ -> False

    esExplosionProfesor = case src of
      RobotHitByProjectile { idProjectile = pid } -> pid == -99
      _ -> False

    esExplosionRobot = case src of
      RobotCollidedWithRobot { idRobot1 = pid } -> pid == -100
      _ -> False

    imgBase
      | esMuerte            = imagenExplosionMuerte m
      | esObstaculoComida   = imagenExplosionComida m
      | esExplosionProfesor = imagenExplosionProfesor m 
      | esExplosionRobot   = imagenExplosionRobot m
      | ttl > 0.4           = imagenExplosion1 m
      | ttl > 0.2           = imagenExplosion2 m
      | ttl > 0             = imagenExplosion3 m
      | otherwise           = Nothing

    pic = case imgBase of
      Just img -> Scale 0.25 0.25 img
      Nothing  -> Blank
  in Translate x y pic

------------------------------------------------------------
-- BOTÓN DE INICIO
------------------------------------------------------------
dibujarBoton :: Picture
dibujarBoton = Pictures
  [ Translate (-85) (-140) $ Scale 0.2 0.2 $ Color black $ Text "Iniciar Juego"
  ]

dentroBoton :: (Float, Float) -> Bool
dentroBoton (mx, my) = mx >= -115 && mx <= 85 && my >= -185 && my <= -95


------------------------------------------------------------
-- BOTÓN DE REINICIO
------------------------------------------------------------
dibujarBotonReiniciar :: Picture
dibujarBotonReiniciar =
  let ancho = 260
      alto = 70
      posY = -180   -- debajo del caramelo
      radio = 25
      fondo =
        Pictures
          [ Color (makeColor 0 0 0 0.3) $
              Translate 5 (posY - 5) $
                rectangleSolid ancho alto
          , Color (makeColorI 255 105 180 220) $
              Translate 0 posY $
                rectangleSolid ancho alto
          , -- esquinas redondeadas simuladas
            Color (makeColorI 255 105 180 220) $
              Pictures
                [ Translate (ancho/2 - radio) (posY + alto/2 - radio) $ circleSolid radio
                , Translate (-ancho/2 + radio) (posY + alto/2 - radio) $ circleSolid radio
                , Translate (ancho/2 - radio) (posY - alto/2 + radio) $ circleSolid radio
                , Translate (-ancho/2 + radio) (posY - alto/2 + radio) $ circleSolid radio
                ]
          ]
      texto =
        Color (makeColorI 255 255 255 230) $
          Translate (-85) (posY - 15) $
            Scale 0.22 0.22 $
              Text "Reiniciar juego"
  in Pictures [fondo, texto]

------------------------------------------------------------
-- DETECCIÓN DE CLIC EN EL BOTÓN DE REINICIO
------------------------------------------------------------
dentroBotonReiniciar :: (Float, Float) -> Bool
dentroBotonReiniciar (mx, my) =
  mx >= -130 && mx <= 130 && my >= (-215) && my <= (-145)


------------------------------------------------------------
-- HUD IZQUIERDO
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
      nombreTxt   = if vida <= 0 then "Muerto" else "Alumno " ++ show (idR r)
      vidaTxt = show (round vida)
      vidaX = panelWideRel - 75
  in Pictures
       [ Translate (panelWideRel - 220) (baseY + 30) $
           Scale 0.15 0.15 $ Color colorTexto $ Text nombreTxt
       , Translate (panelWideRel - 150) (baseY + 15) $
           Pictures
             [ Color white $ rectangleWire (anchoTotal + 4) (altoBarra + 4)
             , Color (greyN 0.3) $ rectangleSolid anchoTotal altoBarra
             , Translate (-(anchoTotal - anchoVida)/2) 0 $
                 Color red $ rectangleSolid anchoVida altoBarra
             ]
       , Translate vidaX (baseY + 5) $
           Scale 0.15 0.15 $ Color white $ Text vidaTxt
       ]
