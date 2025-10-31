module Config.World where

import Graphics.Gloss hiding (Vector, Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector, Point)

import Data.Explosion
import Data.Mundo
import Data.Proyectil
import Data.Robot
import Data.Torreta
import Data.DatosComunes
import Mecanicas.Collision

import Config.Dibujar

import Utils
import Mecanicas.Movement
import Mecanicas.Robot
import Mecanicas.Mundo
import Mecanicas.Explosiones
import Mecanicas.Proyectil

import qualified Data.Map as M
-- Mundo inicial con posiciones aleatorias
estadoInicial :: Picture -> Picture -> Picture -> Picture
               -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture
               -> Maybe Picture -> Maybe Picture -> Maybe Picture
               -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture
               -> Maybe Picture -> Maybe Picture -> Maybe Picture 
               -> (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float)
               -> (Float, Float) -> (Float, Float)  -- posSandwich1 y 2
               -> (Float, Float) -> (Float, Float)  -- posZumo1 y 2
               -> (Float, Float) -> (Float, Float)  -- posPlatano1 y 2
               -> MundoGloss
estadoInicial inicio fondo victoria derrota
               robot1 robot2 robot3 robot4
               torreta profe proyectil
               explosion1 explosion2 explosion3 explosionMuerte escritorio sandwich zumo platano
               pos1 pos2 pos3 pos4
               posSandwich1 posSandwich2
               posZumo1 posZumo2
               posPlatano1 posPlatano2 = MundoGloss
               
  { worldState = World
      { robots =
          [ Robot
              { idR = 1
              , commonR = CommonData 1 0 pos1 (0, 0) (40, 50) (generarRecorrido 1)
              , healthR = 50
              , maxHealthR = 50
              , radarRange = 120
              , turret = Turret 1 (1, 0) 0 
                          (Projectile 1 (CommonData 1 8 (0,0) (250, 0) (projectileRadius*2, projectileRadius*2) []) 1000)
                          1.2
              , haveExploded = False
              , shooting = False
              , memory = M.empty
              }
          , Robot
              { idR = 2
              , commonR = CommonData 2 0 pos2 (0, 0) (40, 50) (generarRecorrido 2)
              , healthR = 140
              , maxHealthR = 140
              , radarRange = 200
              , turret = Turret 2 (-1, 0) 180 
                          (Projectile 2 (CommonData 2 18 (0,0) (-180, 0) (projectileRadius*2, projectileRadius*2) []) 1000)
                          3.2
              , haveExploded = False
              , shooting = False
              , memory = M.empty
              }
          , Robot
              { idR = 3
              , commonR = CommonData 3 0 pos3 (0, 0) (40, 50) (generarRecorrido 3)
              , healthR = 85
              , maxHealthR = 85
              , radarRange = 160
              , turret = Turret 3 (-1, 0) 180 
                          (Projectile 3 (CommonData 3 6 (0,0) (200, 0) (projectileRadius*2, projectileRadius*2) []) 1000)
                          2.4
              , haveExploded = False
              , shooting = False
              , memory = M.empty
              }
          , Robot
              { idR = 4
              , commonR = CommonData 4 0 pos4 (0, 0) (40, 50) (generarRecorrido 4)
              , healthR = 85
              , maxHealthR = 85
              , radarRange = 120
              , turret = Turret 4 (-1, 0) 180 
                          (Projectile 4 (CommonData 4 10 (0,0) (-220, 0) (projectileRadius*2, projectileRadius*2) []) 1000)
                          2.0
              , haveExploded = False
              , shooting = False
              , memory = M.empty
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
  , imagenExplosionMuerte = explosionMuerte   
  , imagenEscritorio = escritorio
  , imagenSandwich = sandwich
  , imagenZumo     = zumo
  , imagenPlatano  = platano
  , posSandwich1 = posSandwich1
  , posSandwich2 = posSandwich2
  , posZumo1     = posZumo1
  , posZumo2     = posZumo2
  , posPlatano1  = posPlatano1
  , posPlatano2  = posPlatano2
  , explosiones = []
  }

-- Manejo de eventos
manejarEvento :: Event -> MundoGloss -> MundoGloss
manejarEvento (EventKey (MouseButton LeftButton) Down _ pos) m
  | modo m == Inicio, dentroBoton pos = m { modo = Jugando }
  | otherwise = m
manejarEvento _ m = m


-- Actualización principal del mundo
actualizar :: Float -> MundoGloss -> MundoGloss
actualizar dt m
  | modo m == Inicio = m
  | otherwise =
      case robotsVivos rsFinal of
        [ultimo] -> m { worldState = w', explosiones = expsAct, modo = Victoria (idR ultimo) }
        []       -> m { worldState = w', explosiones = expsAct, modo = Derrota }
        _        -> m { worldState = w', explosiones = expsAct }
  where
    w = worldState m

    -- Movimiento y disparos
    (rs4, nuevosProj) = actualizarRobots dt w
    ps0  = projectiles w ++ nuevosProj
    psMov = moverProyectiles ps0 dt

    -- 🔴 Usar SOLO vivos para colisiones e impactos
    rsVivos = filter isRobotAlive rs4

    -- Impactos de proyectiles contra vivos
    impactosDetectados    = detectarImpactos rsVivos psMov
    rsDanyados            = aplicarDaño rsVivos impactosDetectados
    nuevasExplosionesProj = generarExplosiones impactosDetectados
    psRestantes           = filtrarProyectilesRestantes psMov impactosDetectados

    -- Colisiones robot-robot SOLO entre vivos
    (_hitsRR, explosionesRR, _nRR) = detectRobotRobotCollisions rsDanyados

    -- Explosiones por muerte (solo si aún no han explotado)
    explosionesMuerte =
      [ Explosion (positionR r) (70,70) 2.5
          (RobotHitByProjectile (idR r) 0 0 (positionR r))
      | r <- rsDanyados
      , not (isRobotAlive r)
      , not (haveExploded r)
      ]

    -- Actualizamos los robots: marcamos los muertos como explotados
    rsFinal =
      [ if not (isRobotAlive r)
          then r { haveExploded = True }
          else r
      | r <- rsDanyados
      ]

    -- Combinamos todas las explosiones
    nuevasExplosionesTot = nuevasExplosionesProj ++ explosionesRR ++ explosionesMuerte

    -- Actualizamos la lista de explosiones activas
    expsAct = actualizarExplosiones dt (explosiones m) nuevasExplosionesTot

    -- Estado final del mundo
    w' = actualizarWorld w rsFinal psRestantes


