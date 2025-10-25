module Config.World where

import Graphics.Gloss hiding (Vector, Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector, Point)

import Data.Explosion
import Data.Mundo
import Data.Proyectil
import Data.Robot
import Data.Torreta
import Data.DatosComunes

import Config.Dibujar

import Utils
import Mecanicas.Movement
import Mecanicas.Robot
import Mecanicas.Mundo
import Mecanicas.Explosiones
import Mecanicas.Proyectil

-- 1. CAMBIO EN LA FIRMA: El quinto argumento (robot1) ahora es Maybe Picture.
estadoInicial :: Picture -> Picture -> Picture -> Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> MundoGloss
estadoInicial inicio fondo victoria derrota robot1 robot2 robot3 robot4 torreta profe proyectil explosion1 explosion2 explosion3= MundoGloss
  { worldState = World
      { robots =
          [ -- Alumno 1: Speedster
            Robot
              { idR = 1
              , commonR = CommonData 1 0 (-150, -100) (0, 0) (40, 50) (generarRecorrido 1)
              , healthR = 50
              , maxHealthR = 50
              , radarRange = 120
              , turret = Turret 1 (1, 0) 0 
                  (Projectile 1 (CommonData 1 8 (0,0) (250, 0) (projectileRadius*2, projectileRadius*2) []) 1000)
                  1.2  -- cooldown
              , haveExploded = False
              , shooting = False
              }
          , -- Alumno 2: Tank
            Robot
              { idR = 2
              , commonR = CommonData 2 0 (150, -100) (0, 0) (40, 50) (generarRecorrido 2)
              , healthR = 140
              , maxHealthR = 140
              , radarRange = 200
              , turret = Turret 2 (-1, 0) 180 
                  (Projectile 2 (CommonData 2 18 (0,0) (-180, 0) (projectileRadius*2, projectileRadius*2) []) 1000)
                  3.2  -- cooldown
              , haveExploded = False
              , shooting = False
              }
          , -- Alumno 3: Soporte
            Robot
              { idR = 3
              , commonR = CommonData 3 0 (-150, 50) (0, 0) (40, 50) (generarRecorrido 3)
              , healthR = 85
              , maxHealthR = 85
              , radarRange = 160
              , turret = Turret 3 (1, 0) 0 
                  (Projectile 3 (CommonData 3 6 (0,0) (200, 0) (projectileRadius*2, projectileRadius*2) []) 1000)
                  2.4  -- cooldown
              , haveExploded = False
              , shooting = False
              }
          , -- Alumno 4: All-rounder
            Robot
              { idR = 4
              , commonR = CommonData 4 0 (150, 50) (0, 0) (40, 50) (generarRecorrido 4)
              , healthR = 85
              , maxHealthR = 85
              , radarRange = 120
              , turret = Turret 4 (-1, 0) 180 
                  (Projectile 4 (CommonData 4 10 (0,0) (-220, 0) (projectileRadius*2, projectileRadius*2) []) 1000)
                  2.0  -- cooldown
              , haveExploded = False
              , shooting = False
              }
          ]
      , projectiles = []
      , turrets = []
      , robotHits = []
      }
  , modo = Inicio
  , imagenInicio = inicio
  , fondoJuego = fondo
  , imagenVictoria = victoria
  , imagenDerrota = derrota
  , imagenRobot1 = robot1 
  , imagenRobot2 = robot2
  , imagenRobot3 = robot3
  , imagenRobot4 = robot4
  , imagenTorreta = torreta
  , imagenProfe = profe
  , imagenProyectil = proyectil
  , imagenExplosion1 = explosion1
  , imagenExplosion2 = explosion2
  , imagenExplosion3 = explosion3
  , explosiones = []
  , numRobotsSeleccionados = 4
  , personalidades = []
  }
  
manejarEvento :: Event -> MundoGloss -> MundoGloss
manejarEvento (EventKey (MouseButton LeftButton) Down _ pos) m
  -- Desde la pantalla inicial → ir a Selección
  | modo m == Inicio, dentroBoton pos =
      m { modo = Seleccion }

  -- Desde Selección → comenzar la partida
  | modo m == Seleccion, dentroBoton pos =
      let nuevosRobots = generarRobotsSeleccionados (numRobotsSeleccionados m) (personalidades m)
          nuevoWorld   = (worldState m) { robots = nuevosRobots
                                        , projectiles = []
                                        , robotHits = []
                                        , turrets = []
                                        }
      in m { modo = Jugando, worldState = nuevoWorld }

  -- 🟢 Desde Victoria o Derrota → volver al menú (reiniciar)
  | modo m `elem` [Victoria 0, Derrota], dentroBotonReiniciar pos =
      estadoInicial (imagenInicio m) (fondoJuego m) (imagenVictoria m) (imagenDerrota m)
                    (imagenRobot1 m) (imagenRobot2 m) (imagenRobot3 m) (imagenRobot4 m)
                    (imagenTorreta m) (imagenProfe m) (imagenProyectil m)
                    (imagenExplosion1 m) (imagenExplosion2 m) (imagenExplosion3 m)

  | otherwise = m

manejarEvento _ m = m



actualizar :: Float -> MundoGloss -> MundoGloss
actualizar dt m
  | modo m == Inicio = m
  | otherwise =
      case robotsVivos rsDanyados of
        [ultimo] -> m { worldState = w', explosiones = expsAct, modo = Victoria (idR ultimo) }
        []       -> m { worldState = w', explosiones = expsAct, modo = Derrota }
        _        -> m { worldState = w', explosiones = expsAct }
  where
    w = worldState m
    (rs4, nuevosProj) = actualizarRobots dt w
    ps0 = projectiles w ++ nuevosProj
    psMov = moverProyectiles ps0 dt
    impactosDetectados = detectarImpactos rs4 psMov
    rsDanyados = aplicarDaño rs4 impactosDetectados
    nuevasExplosiones = generarExplosiones impactosDetectados
    psRestantes = filtrarProyectilesRestantes psMov impactosDetectados
    expsAct = actualizarExplosiones dt (explosiones m) nuevasExplosiones
    w' = actualizarWorld w rsDanyados psRestantes

-- Genera los robots según el número y personalidades elegidas
generarRobotsSeleccionados :: Int -> [String] -> [Robot]
generarRobotsSeleccionados n pers =
  take n $ zipWith crearRobot [1..] (pers ++ repeat "Default")

-- Crea un robot con características según su "personalidad"
crearRobot :: Int -> String -> Robot
crearRobot i tipo =
  case tipo of
    "Speedster" ->
      Robot { idR = i
            , commonR = CommonData i 0 (-150 + 100 * fromIntegral i, -100)
                                         (0, 0) (40, 50) (generarRecorrido i)
            , healthR = 50
            , maxHealthR = 50
            , radarRange = 120
            , turret = Turret i (1, 0) 0
                (Projectile i (CommonData i 8 (0,0) (250, 0)
                (projectileRadius*2, projectileRadius*2) []) 1000)
                1.2
            , haveExploded = False
            , shooting = False
            }

    "Tank" ->
      Robot { idR = i
            , commonR = CommonData i 0 (-150 + 100 * fromIntegral i, -100)
                                         (0, 0) (40, 50) (generarRecorrido i)
            , healthR = 140
            , maxHealthR = 140
            , radarRange = 200
            , turret = Turret i (1, 0) 0
                (Projectile i (CommonData i 18 (0,0) (200, 0)
                (projectileRadius*2, projectileRadius*2) []) 1000)
                3.2
            , haveExploded = False
            , shooting = False
            }

    "Support" ->
      Robot { idR = i
            , commonR = CommonData i 0 (-150 + 100 * fromIntegral i, 50)
                                         (0, 0) (40, 50) (generarRecorrido i)
            , healthR = 85
            , maxHealthR = 85
            , radarRange = 160
            , turret = Turret i (1, 0) 0
                (Projectile i (CommonData i 6 (0,0) (200, 0)
                (projectileRadius*2, projectileRadius*2) []) 1000)
                2.4
            , haveExploded = False
            , shooting = False
            }

    "AllRounder" ->
      Robot { idR = i
            , commonR = CommonData i 0 (-150 + 100 * fromIntegral i, 50)
                                         (0, 0) (40, 50) (generarRecorrido i)
            , healthR = 85
            , maxHealthR = 85
            , radarRange = 120
            , turret = Turret i (1, 0) 0
                (Projectile i (CommonData i 10 (0,0) (220, 0)
                (projectileRadius*2, projectileRadius*2) []) 1000)
                2.0
            , haveExploded = False
            , shooting = False
            }

    -- Default (por si el jugador no elige tipo)
    _ ->
      Robot { idR = i
            , commonR = CommonData i 0 (-150 + 100 * fromIntegral i, -50)
                                         (0, 0) (40, 50) (generarRecorrido i)
            , healthR = 90
            , maxHealthR = 90
            , radarRange = 130
            , turret = Turret i (1, 0) 0
                (Projectile i (CommonData i 10 (0,0) (220, 0)
                (projectileRadius*2, projectileRadius*2) []) 1000)
                2.0
            , haveExploded = False
            , shooting = False
            }

-- Detecta clics dentro del botón de reinicio (en pantallas de Victoria y Derrota)
dentroBotonReiniciar :: Point -> Bool
dentroBotonReiniciar (x, y) =
  x > -150 && x < 150 && y > -250 && y < -170
