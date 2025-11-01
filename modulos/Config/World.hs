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
              -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture  
              -> (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float)
              -> (Float, Float) -> (Float, Float)
              -> (Float, Float) -> (Float, Float)
              -> (Float, Float) -> (Float, Float)
              -> MundoGloss
estadoInicial inicio fondo victoria derrota
               robot1 robot2 robot3 robot4
               torreta profe proyectil
               explosion1 explosion2 explosion3 explosionMuerte escritorio sandwich zumo platano explosionComida
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
  , imagenExplosionComida = explosionComida
  , sandwich1Activo = True
  , sandwich2Activo = True
  , zumo1Activo     = True
  , zumo2Activo     = True
  , platano1Activo  = True
  , platano2Activo  = True
  , explosiones = []
  }

-- Manejo de eventos
manejarEvento :: Event -> MundoGloss -> MundoGloss
manejarEvento (EventKey (MouseButton LeftButton) Down _ pos) m
  | modo m == Inicio, dentroBoton pos = m { modo = Jugando }
  | otherwise = m
manejarEvento _ m = m


-- Actualización principal del mundo

-- Actualización principal del mundo
actualizar :: Float -> MundoGloss -> MundoGloss
actualizar dt m
  | modo m == Inicio = m
  | otherwise =
      case [r | r <- rsFinal, isRobotAlive r] of
        [ultimo] -> m4 { modo = Victoria (idR ultimo) }
        []       -> m4 { modo = Derrota }
        _        -> m4
  where
    w = worldState m

    -- Movimiento y disparos
    (rs0, nuevosProj) = actualizarRobots dt w
    ps0  = projectiles w ++ nuevosProj
    psMov = moverProyectiles ps0 dt

    ------------------------------------------------------------
    -- COLISIONES CON OBJETOS DE COMIDA
    ------------------------------------------------------------

    distanciaA :: Robot -> (Float, Float) -> Bool
    distanciaA r pos =
      isRobotAlive r && distanceBetween (positionR r) pos <= 20

    aplicarDanoComida :: (Float, Float) -> [Robot] -> [Robot]
    aplicarDanoComida pos =
      map (\r ->
        if distanciaA r pos && isRobotAlive r
          then r { healthR = max 0 (healthR r - 10) }  -- resta 10, puede morir si baja a 0
          else r)


    -- === ZUMO 1 ===
    colisionZumo1 = any (`distanciaA` posZumo1 m) rs0
    rs1 = if colisionZumo1 then aplicarDanoComida (posZumo1 m) rs0 else rs0
    expZumo1 = if colisionZumo1
               then [Explosion (posZumo1 m) (30,30) 0.6 (RobotHitByProjectile (-1) (-1) 10 (posZumo1 m))]
               else []

    -- === ZUMO 2 ===
    colisionZumo2 = any (`distanciaA` posZumo2 m) rs1
    rs2 = if colisionZumo2 then aplicarDanoComida (posZumo2 m) rs1 else rs1
    expZumo2 = if colisionZumo2
               then [Explosion (posZumo2 m) (30,30) 0.6 (RobotHitByProjectile (-1) (-1) 10 (posZumo2 m))]
               else []

    -- === PLÁTANO 1 ===
    colisionPlatano1 = any (`distanciaA` posPlatano1 m) rs2
    rs3 = if colisionPlatano1 then aplicarDanoComida (posPlatano1 m) rs2 else rs2
    expPlatano1 = if colisionPlatano1
                  then [Explosion (posPlatano1 m) (30,30) 0.6 (RobotHitByProjectile (-1) (-1) 10 (posPlatano1 m))]
                  else []

    -- === PLÁTANO 2 ===
    colisionPlatano2 = any (`distanciaA` posPlatano2 m) rs3
    rs4 = if colisionPlatano2 then aplicarDanoComida (posPlatano2 m) rs3 else rs3
    expPlatano2 = if colisionPlatano2
                  then [Explosion (posPlatano2 m) (30,30) 0.6 (RobotHitByProjectile (-1) (-1) 10 (posPlatano2 m))]
                  else []

    -- === SÁNDWICH 1 ===
    colisionSandwich1 = any (`distanciaA` posSandwich1 m) rs4
    rs5 = if colisionSandwich1 then aplicarDanoComida (posSandwich1 m) rs4 else rs4
    expSandwich1 = if colisionSandwich1
                   then [Explosion (posSandwich1 m) (30,30) 0.6 (RobotHitByProjectile (-1) (-1) 10 (posSandwich1 m))]
                   else []

    -- === SÁNDWICH 2 ===
    colisionSandwich2 = any (`distanciaA` posSandwich2 m) rs5
    rs6 = if colisionSandwich2 then aplicarDanoComida (posSandwich2 m) rs5 else rs5
    expSandwich2 = if colisionSandwich2
                   then [Explosion (posSandwich2 m) (30,30) 0.6 (RobotHitByProjectile (-1) (-1) 10 (posSandwich2 m))]
                   else []

    -- Todas las explosiones de comida juntas
    explosionesObst = expZumo1 ++ expZumo2 ++ expPlatano1 ++ expPlatano2 ++ expSandwich1 ++ expSandwich2

    -- Actualizar estado de objetos (cada uno independiente)
    m1 = m
      { zumo1Activo     = if colisionZumo1 then False else zumo1Activo m
      , zumo2Activo     = if colisionZumo2 then False else zumo2Activo m
      , platano1Activo  = if colisionPlatano1 then False else platano1Activo m
      , platano2Activo  = if colisionPlatano2 then False else platano2Activo m
      , sandwich1Activo = if colisionSandwich1 then False else sandwich1Activo m
      , sandwich2Activo = if colisionSandwich2 then False else sandwich2Activo m
      }


    ------------------------------------------------------------
    -- RESTO DE LÓGICA GENERAL DEL JUEGO
    ------------------------------------------------------------

    rsVivos = rs6  -- ya no filtramos para evitar eliminar por error

    -- Impactos de proyectiles
    impactosDetectados    = detectarImpactos rsVivos psMov
    rsDanyados            = aplicarDaño rsVivos impactosDetectados
    nuevasExplosionesProj = generarExplosiones impactosDetectados
    psRestantes           = filtrarProyectilesRestantes psMov impactosDetectados

    -- Colisiones robot-robot
    (_hitsRR, explosionesRR, _nRR) = detectRobotRobotCollisions rsDanyados

    -- Explosiones por muerte (solo si aún no han explotado)
    explosionesMuerte =
      [ Explosion (positionR r) (70,70) 2.5
          (RobotHitByProjectile (idR r) 0 0 (positionR r))
      | r <- rsDanyados
      , healthR r <= 0
      , not (haveExploded r)
      ]

    -- Actualizamos los robots: marcamos los que murieron ahora
    rsFinal =
      [ if healthR r <= 0
          then r { haveExploded = True, healthR = 0 }
          else r
      | r <- rsDanyados
      ]

    -- Combinamos todas las explosiones
    nuevasExplosionesTot = nuevasExplosionesProj ++ explosionesRR ++ explosionesMuerte ++ explosionesObst

    -- Actualizamos la lista de explosiones activas
    expsAct = actualizarExplosiones dt (explosiones m1) nuevasExplosionesTot

    -- Estado final del mundo
    w' = actualizarWorld w rsFinal psRestantes
    m2 = m1 { worldState = w', explosiones = expsAct }
    m3 = m2
    m4 = m3
