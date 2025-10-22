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
import Movement (positionR, isRobotAlive, detectedAgent)

generarPuntosPatrulla :: Id -> [Position]
generarPuntosPatrulla id = take 11 $ zip xs ys
  where
    xs = [ fromIntegral ((id * i * 611) `mod` 500) - 250 | i <- [1..] ]
    ys = [ fromIntegral ((id * i * 456) `mod` 500) - 250 | i <- [1..] ]

estadoInicial :: Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> MundoGloss
estadoInicial inicio fondo victoria derrota robot1 torreta= MundoGloss
  { worldState = World
      { robots =
          [ -- Alumno 1: Speedster
            Robot
              { idR = 1
              , commonR = CommonData 1 0 (-150, -100) (0, 0) (40, 50) (generarPuntosPatrulla 1)
              , healthR = 70
              , maxHealthR = 70
              , radarRange = 120
              , turret = Turret 1 (1, 0) 0 
                  (Projectile 1 (CommonData 1 8 (0,0) (250, 0) (chicleRadius*2, chicleRadius*2) []) 1000)
                  0.6  -- cooldown
              , haveExploded = False
              }
          , -- Alumno 2: Tank
            Robot
              { idR = 2
              , commonR = CommonData 2 0 (150, -100) (0, 0) (40, 50) (generarPuntosPatrulla 2)
              , healthR = 180
              , maxHealthR = 180
              , radarRange = 200
              , turret = Turret 2 (-1, 0) 180 
                  (Projectile 2 (CommonData 2 18 (0,0) (-180, 0) (chicleRadius*2, chicleRadius*2) []) 1000)
                  1.6  -- cooldown
              , haveExploded = False
              }
          , -- Alumno 3: Soporte
            Robot
              { idR = 3
              , commonR = CommonData 3 0 (-150, 50) (0, 0) (40, 50) (generarPuntosPatrulla 3)
              , healthR = 110
              , maxHealthR = 110
              , radarRange = 160
              , turret = Turret 3 (1, 0) 0 
                  (Projectile 3 (CommonData 3 6 (0,0) (200, 0) (chicleRadius*2, chicleRadius*2) []) 1000)
                  1.2  -- cooldown
              , haveExploded = False
              }
          , -- Alumno 4: All-rounder
            Robot
              { idR = 4
              , commonR = CommonData 4 0 (150, 50) (0, 0) (40, 50) (generarPuntosPatrulla 4)
              , healthR = 110
              , maxHealthR = 110
              , radarRange = 120
              , turret = Turret 4 (-1, 0) 180 
                  (Projectile 4 (CommonData 4 10 (0,0) (-220, 0) (chicleRadius*2, chicleRadius*2) []) 1000)
                  0.9  -- cooldown
              , haveExploded = False
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
  -- , imagenRobot2 = robot2
  -- , imagenRobot3 = robot3
  -- , imagenRobot4 = robot4
  , imagenTorreta = torreta
  , explosiones = []
  }

proyectilBase :: Id -> Projectile
proyectilBase i = Projectile
  { idP = i
  , commonP = CommonData i 10 (0, 0) (0, 0) (chicleRadius*2, chicleRadius*2) []
  , rangeP = 1000
  }

manejarEvento :: Event -> MundoGloss -> MundoGloss
manejarEvento (EventKey (MouseButton LeftButton) Down _ pos) m
  | modo m == Inicio, dentroBoton pos = m { modo = Jugando }
  | otherwise = m
manejarEvento _ m = m

velocidadPorRol :: Id -> Float
velocidadPorRol 1 = 150
velocidadPorRol 2 = 20
velocidadPorRol 3 = 50
velocidadPorRol 4 = 60
velocidadPorRol _ = 50

comportamientoNino :: Float -> Robot -> Robot
comportamientoNino dt r
  | estaMuerto r = r
  | null pts     = r
  | haLlegado pos objetivo = avanzarPatrulla r
  | otherwise    = moverHaciaObjetivo dt r
  where
    pts = points (commonR r)
    pos = position (commonR r)
    objetivo = head pts

estaMuerto :: Robot -> Bool
estaMuerto r = healthR r <= 0

haLlegado :: (Float, Float) -> (Float, Float) -> Bool
haLlegado pos objetivo = distanceBetween pos objetivo < 20

avanzarPatrulla :: Robot -> Robot
avanzarPatrulla r =
  let pts = points (commonR r)
      nuevosPts = tail pts ++ [head pts]
  in r { commonR = (commonR r) { points = nuevosPts } }

moverHaciaObjetivo :: Float -> Robot -> Robot
moverHaciaObjetivo dt r =
  let pos = position (commonR r)
      vel = velocidadPorRol (idR r)
      objetivo = head (points (commonR r))
      ang = angleToTarget pos objetivo
      dx = cos ang * vel * dt
      dy = sin ang * vel * dt
      nuevaPos = clampDentro (fst pos + dx, snd pos + dy)
  in r { commonR = (commonR r) { position = nuevaPos } }

robotQuiereDisparar :: World -> Robot -> Bool
robotQuiereDisparar world me =
  any (detectedAgent me) [r | r <- robots world, idR r /= idR me, isRobotAlive r]

pasoShooting :: Float -> World -> ([Robot], [Projectile])
pasoShooting dt world = foldr (procesarRobot dt world) ([], []) (robots world)

procesarRobot :: Float -> World -> Robot -> ([Robot], [Projectile]) -> ([Robot], [Projectile])
procesarRobot dt world r (accR, accP)
  | estaMuerto r      = (r : accR, accP)
  | not puedeDisparar = (rCD : accR, accP)
  | otherwise         = (r' : accR, p : accP)
  where
    rCD = actualizarCooldown r (nuevoCooldown (turret r) dt)
    enemigosVivos = enemigos rCD world
    puedeDisparar = cooldown (turret rCD) <= 0
                 && not (null enemigosVivos)
                 && robotQuiereDisparar world rCD
    objetivo = enemigoMasCercano rCD enemigosVivos
    (p, r') = generarDisparo dt rCD objetivo

nuevoCooldown :: Turret -> Float -> Float
nuevoCooldown t dt = max 0 (cooldown t - dt)

actualizarCooldown :: Robot -> Float -> Robot
actualizarCooldown r t1 = r { turret = (turret r) { cooldown = t1 } }

enemigos :: Robot -> World -> [Robot]
enemigos r w = [ r' | r' <- robots w, idR r' /= idR r, isRobotAlive r' ]

enemigoMasCercano :: Robot -> [Robot] -> Robot
enemigoMasCercano me (r:rs) = fst $ foldl buscar (r, distanceBetween (position (commonR me)) (position (commonR r))) rs
  where
    buscar (minR, minD) r' =
      let d = distanceBetween (position (commonR me)) (position (commonR r'))
      in if d < minD then (r', d) else (minR, minD)

generarDisparo :: Float -> Robot -> Robot -> (Projectile, Robot)
generarDisparo dt r objetivo =
  let (xM, yM) = position (commonR r)
      (xT, yT) = position (commonR objetivo)
      angRad = atan2 (yT - yM) (xT - xM)
      speed = 250
      vx = cos angRad * speed
      vy = sin angRad * speed
      cooldown' = case idR r of
                    1 -> 0.6
                    2 -> 1.6
                    3 -> 1.2
                    4 -> 0.9
                    _ -> 1.0
      baseProj = projectileT (turret r)
      p = Projectile
            { idP = idR r
            , commonP = (commonP baseProj) { position = (xM, yM + 28), velocity = (vx, vy) }
            , rangeP = 1000
            }
      r' = r { turret = (turret r) { cooldown = cooldown' } }
  in (p, r')

apuntarTorreta :: World -> Robot -> Robot
apuntarTorreta world r
  | healthR r <= 0 = r
  | otherwise =
      let enemigosVivos = [e | e <- robots world, idR e /= idR r, isRobotAlive e]
          detectados = filter (detectedAgent r) enemigosVivos
      in if null detectados
           then r
           else
             let objetivo = head detectados
                 ang = angleToTarget (positionR r) (positionR objetivo)
                 angDegree = rad2deg ang
                 vec = (cos angDegree, sin angDegree)
             in r { turret = (turret r) { angleT = angDegree, vectorT = vec } }

curarSoporte :: Float -> Robot -> Robot
curarSoporte dt r
  | idR r /= 3 = r
  | not (isRobotAlive r) = r
  | nuevoTiempo >= 10 =
      r { healthR = vidaNueva
        , turret = (turret r) { cooldown = 0 }
        }
  | otherwise =
      r { turret = (turret r) { cooldown = nuevoTiempo }
        }
  where
    tiempoActual = cooldown (turret r)
    nuevoTiempo  = tiempoActual + dt
    vidaNueva    = min 110 (healthR r + 2)

clampDentro :: (Float,Float) -> (Float,Float)
clampDentro (x,y) =
  let halfW = ancho/2
      halfH = alto/2
      x' = max (-halfW + 20) (min (halfW - 20) x)
      y' = max (-halfH + 25) (min (halfH - 25) y)
  in (x', y')

dispararTodos :: World -> World
dispararTodos w = w { projectiles = nuevos ++ projectiles w }
  where
    nuevos =
      [ Projectile
          { idP = idR r
          , commonP = (commonP (projectileT (turret r)))
              { position = (x + 10 + offset, y + 28)
              , velocity = (vx, 0)
              }
              , rangeP = 1000
          }
      | (i, r) <- zip [0..] (robots w)
      , healthR r > 0
      , let (x, y) = position (commonR r)
            vx     = if even i then velChicleVel else -velChicleVel
            offset = if even i then 20 else -20
      ]

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

actualizarExplosiones :: Float -> [Explosion] -> [Explosion] -> [Explosion]
actualizarExplosiones dt existentes nuevas =
  [ Explosion pos size (ttl - dt) src
  | Explosion pos size ttl src <- existentes ++ nuevas
  , ttl - dt > 0
  ]

actualizarWorld :: World -> [Robot] -> [Projectile] -> World
actualizarWorld w robotsActivos proyectilesActivos =
  w { robots = robotsActivos, projectiles = proyectilesActivos }

robotsVivos :: [Robot] -> [Robot]
robotsVivos = filter ((> 0) . healthR)

actualizarRobots :: Float -> World -> ([Robot], [Projectile])
actualizarRobots dt w =
  let rs0 = robots w
      rs1 = map (comportamientoNino dt) rs0
      rs2 = map (apuntarTorreta w) rs1
      (rs3, nuevosProj) = pasoShooting dt w { robots = rs2 }
      rs4 = map (curarSoporte dt) rs3
  in (rs4, nuevosProj)

moverProyectiles :: [Projectile] -> Float -> [Projectile]
moverProyectiles ps dt =
  [ p { commonP = (commonP p)
          { position = (x + vx * dt, y + vy * dt) } }
  | p <- ps
  , let (x, y) = position (commonP p)
        (vx, vy) = velocity (commonP p)
  , x > -ancho/2 && x < ancho/2 && y > -alto/2 && y < alto/2
  ]

detectarImpactos :: [Robot] -> [Projectile] -> [(Id, Id, Damage, Position)]
detectarImpactos rs ps =
  [ (idR r, idP p, damage (commonP p), position (commonP p))
  | r <- rs, healthR r > 0
  , p <- ps
  , idR r /= idP p
  , circleAABB (position (commonP p)) chicleRadius (ninoBox r)
  ]

aplicarDaño :: [Robot] -> [(Id, Id, Damage, Position)] -> [Robot]
aplicarDaño rs impactos =
  [ if any (\(idr, _, _, _) -> idr == idR r) impactos
      then let totalDaño = sum [ d | (idr, _, d, _) <- impactos, idr == idR r ]
           in r { healthR = max 0 (healthR r - totalDaño)
                , haveExploded = haveExploded r || healthR r - totalDaño <= 0
                }
      else r
  | r <- rs
  ]

generarExplosiones :: [(Id, Id, Damage, Position)] -> [Explosion]
generarExplosiones impactos =
  [ Explosion pos (30, 0) 0.6 (RobotHitByProjectile rid pid dmg pos)
  | (rid, pid, dmg, pos) <- impactos
  ]

filtrarProyectilesRestantes :: [Projectile] -> [(Id, Id, Damage, Position)] -> [Projectile]
filtrarProyectilesRestantes ps impactos =
  [ p | p <- ps, not (any (\(_, pid, _, _) -> pid == idP p) impactos) ]
