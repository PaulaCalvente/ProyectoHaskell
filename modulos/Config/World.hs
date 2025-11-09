module Config.World where

import Graphics.Gloss hiding (Vector, Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector, Point)
import Test.QuickCheck (Gen, generate, choose)
import System.IO.Unsafe (unsafePerformIO)

import Data.Explosion
import Data.Mundo
import Data.Proyectil
import Data.Robot
import Data.Torreta
import Data.DatosComunes
import Mecanicas.Collision

import Config.Dibujar
import Config.Generacion

import Utils
import Mecanicas.Movement
import Mecanicas.Robot
import Mecanicas.Mundo
import Mecanicas.Explosiones
import Mecanicas.Proyectil
import Estadisticas (escribirEstadisticas)

import qualified Data.Map as M
import Data.List (partition)
import Data.Maybe (fromMaybe)

------------------------------------------------------------
-- ESTADO INICIAL
------------------------------------------------------------

estadoInicial :: Picture -> Picture -> Picture -> Picture -> Picture
              -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture
              -> Maybe Picture -> Maybe Picture -> Maybe Picture
              -> Maybe Picture -> Maybe Picture -> Maybe Picture
              -> Maybe Picture -> Maybe Picture
              -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture
              -> Maybe Picture
              -> (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float)
              -> (Float, Float) -> (Float, Float)
              -> (Float, Float) -> (Float, Float)
              -> (Float, Float) -> (Float, Float)
              -> MundoGloss
estadoInicial inicio fondo victoria derrota imagenCarga
               robot1 robot2 robot3 robot4
               torreta profe profeEnfadado proyectil
               explosion1 explosion2 explosion3 explosionMuerte escritorio sandwich zumo platano explosionComida explosionProfesor
               explosionRobots
               pos1 pos2 pos3 pos4
               posSandwich1 posSandwich2
               posZumo1 posZumo2
               posPlatano1 posPlatano2 =
  MundoGloss
    { worldState = World
        { robots =
            [ Robot
                { idR = 1
                , commonR = CommonData 1 0 pos1 (0, 0) (40, 50) (generarRecorrido 1)
                , healthR = 70
                , maxHealthR = 70
                , radarRange = 200
                , turret = Turret 1 (1, 0) 0
                            (Projectile 1 (CommonData 1 8 (0,0) (250, 0)
                            (projectileRadius*2, projectileRadius*2) []) 1000)
                            1.2
                , haveExploded = False
                , shooting = False
                , memory = M.empty
                }
            , Robot
                { idR = 2
                , commonR = CommonData 2 0 pos2 (0, 0) (40, 50) (generarRecorrido 2)
                , healthR = 120
                , maxHealthR = 120
                , radarRange = 120
                , turret = Turret 2 (-1, 0) 180
                            (Projectile 2 (CommonData 2 18 (0,0) (-180, 0)
                            (projectileRadius*2, projectileRadius*2) []) 1000)
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
                            (Projectile 3 (CommonData 3 6 (0,0) (200, 0)
                            (projectileRadius*2, projectileRadius*2) []) 1000)
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
                            (Projectile 4 (CommonData 4 10 (0,0) (-220, 0)
                            (projectileRadius*2, projectileRadius*2) []) 1000)
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
    , imagenCarga = imagenCarga
    , imagenRobot1 = robot1
    , imagenRobot2 = robot2
    , imagenRobot3 = robot3
    , imagenRobot4 = robot4
    , imagenTorreta = torreta
    , imagenProfe = profe
    , imagenProfeEnfadado = profeEnfadado 
    , imagenProyectil = proyectil
    , imagenExplosion1 = explosion1
    , imagenExplosion2 = explosion2
    , imagenExplosion3 = explosion3
    , imagenExplosionMuerte = explosionMuerte
    , imagenEscritorio = escritorio
    , imagenSandwich = sandwich
    , imagenZumo = zumo
    , imagenPlatano = platano
    , imagenExplosionComida = explosionComida
    , imagenExplosionProfesor = explosionProfesor
    , imagenExplosionRobot = explosionRobots
    , torneosRestantes = 1
    , posSandwich1 = posSandwich1
    , posSandwich2 = posSandwich2
    , posZumo1 = posZumo1
    , posZumo2 = posZumo2
    , posPlatano1 = posPlatano1
    , posPlatano2 = posPlatano2
    , sandwich1Activo = True
    , sandwich2Activo = True
    , zumo1Activo = True
    , zumo2Activo = True
    , platano1Activo = True
    , platano2Activo = True
    , profesorActivo = False
    , tiempoExplosionProfesor = 0
    , posicionProfesor = (0, 180)
    , cooldownProfesor = 0
    , explosiones = []
    , recentCollisions = []
    , tiempoTranscurrido = 0
    , historialImpactos = []
    , muertesRegistradas = []
    , todosLosResultados = []
    }

------------------------------------------------------------
-- INPUT / EVENTOS
------------------------------------------------------------

manejarEvento :: Event -> MundoGloss -> MundoGloss
manejarEvento (EventKey (MouseButton LeftButton) Down _ pos) m
  | modo m == Inicio, dentroBoton pos = m { modo = Cargando, tiempoTranscurrido = 0 }
  | esFinJuego (modo m), dentroBotonReiniciar pos = reiniciarMundo m
  | otherwise = m
manejarEvento _ m = m

esFinJuego :: Modo -> Bool
esFinJuego (Victoria _) = True
esFinJuego Derrota      = True
esFinJuego _            = False

reiniciarMundo :: MundoGloss -> MundoGloss
reiniciarMundo m =
  estadoInicial (imagenInicio m)
                (fondoJuego m)
                (imagenVictoria m)
                (imagenDerrota m)
                (imagenCarga m)
                (imagenRobot1 m)
                (imagenRobot2 m)
                (imagenRobot3 m)
                (imagenRobot4 m)
                (imagenTorreta m)
                (imagenProfe m)
                (imagenProfeEnfadado m)
                (imagenProyectil m)
                (imagenExplosion1 m)
                (imagenExplosion2 m)
                (imagenExplosion3 m)
                (imagenExplosionMuerte m)
                (imagenEscritorio m)
                (imagenSandwich m)
                (imagenZumo m)
                (imagenPlatano m)
                (imagenExplosionComida m)
                (imagenExplosionProfesor m)
                (imagenExplosionRobot m)
                (-20,156) (-50,321) (155,83) (202,65)
                (-54,-59) (233,-98)
                (-235,255) (173,-300)
                (-312,-200) (55,-133)

------------------------------------------------------------
-- RESUMEN DEL TORNEO
------------------------------------------------------------

generarResultadoTorneo :: MundoGloss -> ResultadoTorneo
generarResultadoTorneo m = 
   let
    w = worldState m
    todosBots = robots w
    idsBots = map idR todosBots
    duracion = tiempoTranscurrido m
    impactosMap = historialImpactos m
    impactosCompletos = [ (id, fromMaybe 0 (lookup id impactosMap)) | id <- idsBots ]
    muertesMap = muertesRegistradas m
    porcentajesVida = 
      [ (id, let tMuerte = fromMaybe duracion (lookup id muertesMap)
                 pct = if duracion > 0 then (tMuerte / duracion) * 100 else 100
             in pct)
      | id <- idsBots
      ]
    ganador = case modo m of
      Victoria i -> Just i
      Derrota -> Nothing
      _ -> Nothing
  in ResultadoTorneo
       { numImpactosPorBot = impactosCompletos
       , porcentajeVidaPorBot = porcentajesVida
       , ganadorTorneo = ganador
       , duracionTorneo = duracion
       }

-- Cuenta victorias por bot a partir de los resultados
victoriasPorBot :: [ResultadoTorneo] -> M.Map Id Int
victoriasPorBot =
  foldr (\res mp -> case ganadorTorneo res of
                      Just i  -> M.insertWith (+) i 1 mp
                      Nothing -> mp)
        M.empty

-- Devuelve la lista de bots con el máximo nº de victorias
campeonesEmpatados :: [ResultadoTorneo] -> [Id]
campeonesEmpatados resultados =
  case M.toList (victoriasPorBot resultados) of
    [] -> []
    xs -> let maxV = maximum (map snd xs)
          in [ i | (i,v) <- xs, v == maxV ]

------------------------------------------------------------
-- UPDATE PRINCIPAL
------------------------------------------------------------

actualizar :: Float -> MundoGloss -> MundoGloss
actualizar dt m
  -- NUEVO: Modo de carga (3 segundos mostrando la imagen)
  | modo m == Cargando =
      let t = tiempoTranscurrido m + dt
      in if t >= 3  -- Duración de la pantalla de carga (3 segundos)
           then m { modo = Jugando, tiempoTranscurrido = 0 }
           else m { tiempoTranscurrido = t }

  | modo m == Inicio = m
  | esFinJuego (modo m) = m  -- Ya terminó: no hacer nada
  | otherwise =
      let
        w = worldState m
        (rs0, nuevosProj) = actualizarRobots dt w
        ps0 = projectiles w ++ nuevosProj
        psMov = moverProyectiles ps0 dt

        distanciaA r pos = isRobotAlive r && distanceBetween (positionR r) pos <= 50
        aplicarDanoComida pos = map (\r ->
          if distanciaA r pos
            then let vida = healthR r in if vida <= 10 then r { healthR = 0 } else r { healthR = vida - 10 }
            else r)
        procesarComida activo pos rs =
          if activo && any (`distanciaA` pos) rs
             then (True, aplicarDanoComida pos rs,
                  [Explosion pos (30,30) 0.6 (RobotHitByProjectile (-1) (-1) 10 pos) (-1) (-1)],
                  (10000,10000))
            else (False, rs, [], pos)

        (colZ1, rs1, expZ1, posZ1) = procesarComida (zumo1Activo m) (posZumo1 m) rs0
        (colZ2, rs2, expZ2, posZ2) = procesarComida (zumo2Activo m) (posZumo2 m) rs1
        (colP1, rs3, expP1, posP1) = procesarComida (platano1Activo m) (posPlatano1 m) rs2
        (colP2, rs4, expP2, posP2) = procesarComida (platano2Activo m) (posPlatano2 m) rs3
        (colS1, rs5, expS1, posS1) = procesarComida (sandwich1Activo m) (posSandwich1 m) rs4
        (colS2, rs6, expS2, posS2) = procesarComida (sandwich2Activo m) (posSandwich2 m) rs5

        m1 = m
          { zumo1Activo = if colZ1 then False else zumo1Activo m
          , zumo2Activo = if colZ2 then False else zumo2Activo m
          , platano1Activo = if colP1 then False else platano1Activo m
          , platano2Activo = if colP2 then False else platano2Activo m
          , sandwich1Activo = if colS1 then False else sandwich1Activo m
          , sandwich2Activo = if colS2 then False else sandwich2Activo m
          , posZumo1 = posZ1, posZumo2 = posZ2
          , posPlatano1 = posP1, posPlatano2 = posP2
          , posSandwich1 = posS1, posSandwich2 = posS2
          }

        explosionesObst = expZ1 ++ expZ2 ++ expP1 ++ expP2 ++ expS1 ++ expS2

        profesorPos = posicionProfesor m
        hayContactoProfe = any (\r -> isRobotAlive r && distanceBetween (positionR r) profesorPos < 55) rs6
        cdActual = max 0 (cooldownProfesor m - dt)

        (profActivo2, tProfe2, rs7, expProfe, nuevoCooldown) =
          if profesorActivo m
            then let t' = tiempoExplosionProfesor m - dt in
              if t' <= 0
                then let radio = 100
                         rsGolpeados = [ if distanceBetween (positionR r) profesorPos < radio
                                         then r { healthR = max 0 (healthR r - 40) }
                                         else r | r <- rs6 ]
                         ex = [Explosion profesorPos (120,120) 1.2 (RobotHitByProjectile (-99) (-99) 40 profesorPos)]
                     in (False, 0, rsGolpeados, ex, 5.0)
                else (True, t', rs6, [], cdActual)
            else if hayContactoProfe && cdActual <= 0
                   then (True, 3.0, rs6, [], cdActual)
                   else (False, 0, rs6, [], cdActual)

        tiempoActualizado = tiempoTranscurrido m + dt

        rsVivos = filter isRobotAlive rs7
        impactosDetectados = detectarImpactos rsVivos psMov

        nuevosImpactos = [ (idProjectile, 1) | (_, idProjectile, _, _) <- impactosDetectados ]
        historialActualizado = 
          foldl (\acc (idBot, cuenta) ->
            let (antes, despues) = partition ((== idBot) . fst) acc
            in case antes of
                 [] -> (idBot, cuenta) : acc
                 _  -> (idBot, sum (map snd antes) + cuenta) : despues
          ) (historialImpactos m) nuevosImpactos

        rsDanyadosTemp = aplicarDaño rsVivos impactosDetectados
        nuevasExplosionesProj = generarExplosiones impactosDetectados
        psRestantes = filtrarProyectilesRestantes psMov impactosDetectados

        recent0 = recentCollisions m
        recentTicked = tickRecentCollisions dt recent0
        ttlForCollision = 2.5
        (newPairs, recentUpdated) = detectNewPairs ttlForCollision rsVivos recentTicked
        explFromPairs = map (explosionFromPair rsVivos) newPairs

        botsQueMurieron = [ r | r <- rsDanyadosTemp, healthR r <= 0, not (haveExploded r) ]
        nuevasMuertes = [ (idR r, tiempoActualizado) | r <- botsQueMurieron ]
        muertesActualizadas = muertesRegistradas m ++ nuevasMuertes

        explosionesMuerte = [ Explosion (positionR r) (70,70) 2.5
                                (RobotHitByProjectile (idR r) 0 0 (positionR r)) (idR r) 0
                            | r <- botsQueMurieron ]

        rsFinal = [ case lookup (idR r) [(idR r', r') | r' <- rsDanyadosTemp] of
                      Just rNuevo -> if healthR rNuevo <= 0
                                      then rNuevo { haveExploded = True, healthR = 0 }
                                      else rNuevo
                      Nothing -> r
                  | r <- rs7 ]

        nuevasExplosionesTot = nuevasExplosionesProj ++ explFromPairs ++ explosionesMuerte ++ explosionesObst ++ [ f (-1) (-1) | f <- expProfe ]
        expsAct = actualizarExplosiones dt (explosiones m1) nuevasExplosionesTot

        w' = (actualizarWorld w { robots = rsVivos } rsVivos psRestantes) { robots = rsFinal }

        m2 = m1
          { worldState = w'
          , explosiones = expsAct
          , profesorActivo = profActivo2
          , tiempoExplosionProfesor = tProfe2
          , cooldownProfesor = nuevoCooldown
          , recentCollisions = recentUpdated
          , tiempoTranscurrido = tiempoActualizado
          , historialImpactos = historialActualizado
          , muertesRegistradas = muertesActualizadas
          }

        vivos = [r | r <- rsFinal, isRobotAlive r]
        termina = length vivos <= 1

      in if termina
           then
             let mFinal = case vivos of
                            [r] -> m2 { modo = Victoria (idR r) }
                            _   -> m2 { modo = Derrota }
                 resultado = generarResultadoTorneo mFinal
                 mConRes = mFinal { todosLosResultados = todosLosResultados m2 ++ [resultado] }
             in reiniciarAutomatico mConRes
           else m2


------------------------------------------------------------
-- REINICIO AUTOMÁTICO + CAMPEÓN GLOBAL
------------------------------------------------------------

reiniciarMundoIO :: MundoGloss -> IO MundoGloss
reiniciarMundoIO m = do
  [pos1, pos2, pos3, pos4,
   posSandwich1, posSandwich2,
   posZumo1, posZumo2,
   posPlatano1, posPlatano2] <- generate (generarPosiciones 10)
  pure $
    estadoInicial (imagenInicio m)
                  (fondoJuego m)
                  (imagenVictoria m)
                  (imagenDerrota m)
                  (imagenCarga m)
                  (imagenRobot1 m)
                  (imagenRobot2 m)
                  (imagenRobot3 m)
                  (imagenRobot4 m)
                  (imagenTorreta m)
                  (imagenProfe m)
                  (imagenProfeEnfadado m)
                  (imagenProyectil m)
                  (imagenExplosion1 m)
                  (imagenExplosion2 m)
                  (imagenExplosion3 m)
                  (imagenExplosionMuerte m)
                  (imagenEscritorio m)
                  (imagenSandwich m)
                  (imagenZumo m)
                  (imagenPlatano m)
                  (imagenExplosionComida m)
                  (imagenExplosionProfesor m)
                  (imagenExplosionRobot m)
                  pos1 pos2 pos3 pos4
                  posSandwich1 posSandwich2
                  posZumo1 posZumo2
                  posPlatano1 posPlatano2

--  Devuelve el Id del campeón global (si lo hay).
--   Regla: mayor nº de victorias; si hay empate, el Id más bajo.
campeonGlobalSimple :: [ResultadoTorneo] -> Maybe Id
campeonGlobalSimple resultados =
  case M.toList vittorie of
    [] -> Nothing
    xs ->
      let maxV = maximum (map snd xs)
      in Just (minimum [ i | (i,v) <- xs, v == maxV ])
  where
    vittorie =
      foldr (\res mp -> case ganadorTorneo res of
                          Just i  -> M.insertWith (+) i 1 mp
                          Nothing -> mp)
            M.empty resultados

reiniciarAutomatico :: MundoGloss -> MundoGloss
reiniciarAutomatico m
  | torneosRestantes m > 1 =
      let nuevo = unsafePerformIO (reiniciarMundoIO m)
      in nuevo
           { modo               = Cargando
           , torneosRestantes   = torneosRestantes m - 1
           , tiempoTranscurrido = 0
           , historialImpactos  = []
           , muertesRegistradas = []
           , todosLosResultados = todosLosResultados m
           }
  | otherwise =
      unsafePerformIO $ do
        let res        = todosLosResultados m
            finalistas = campeonesEmpatados res
        case finalistas of
          []  -> escribirEstadisticas res >> pure (m { modo = Derrota })
          [g] -> escribirEstadisticas res >> pure (m { modo = Victoria g })
          _   -> iniciarPlayoffIO finalistas m

-- Playoff entre empatados (mismo motor + 1 partida extra)
iniciarPlayoffIO :: [Id] -> MundoGloss -> IO MundoGloss
iniciarPlayoffIO finalistas m = do
  [p1,p2,p3,p4, s1,s2, z1,z2, pl1,pl2] <- generate (generarPosiciones 10)

  let base =
        estadoInicial (imagenInicio m) (fondoJuego m) (imagenVictoria m) (imagenDerrota m) (imagenCarga m)
                      (imagenRobot1 m) (imagenRobot2 m) (imagenRobot3 m) (imagenRobot4 m)
                      (imagenTorreta m) (imagenProfe m) (imagenProfeEnfadado m) (imagenProyectil m)
                      (imagenExplosion1 m) (imagenExplosion2 m) (imagenExplosion3 m) (imagenExplosionMuerte m)
                      (imagenEscritorio m) (imagenSandwich m) (imagenZumo m) (imagenPlatano m)
                      (imagenExplosionComida m) (imagenExplosionProfesor m) (imagenExplosionRobot m)
                      p1 p2 p3 p4 s1 s2 z1 z2 pl1 pl2

      rsPlayoff = [ r | r <- robots (worldState base), idR r `elem` finalistas ]
      wPlayoff  = (worldState base) { robots = rsPlayoff }

  pure base
    { worldState         = wPlayoff
    , modo               = Jugando
    , torneosRestantes   = 1
    , tiempoTranscurrido = 0
    , historialImpactos  = []
    , muertesRegistradas = []
    , todosLosResultados = todosLosResultados m
    }
