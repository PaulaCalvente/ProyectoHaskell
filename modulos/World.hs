module World where

import Graphics.Gloss hiding (Vector, Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector, Point)
import Types
import Utils
import Movement (positionR, isRobotAlive, detectedAgent)

-- Estado que usa Gloss
data MundoGloss = MundoGloss
  { worldState     :: World
  , modo           :: Modo
  , imagenInicio   :: Picture
  , fondoJuego     :: Picture
  , imagenVictoria :: Picture
  , explosiones    :: [Explosion]
  , burbujas       :: [BurbujaMuerte]
  }

-- Genera 4 puntos de patrulla únicos por robot (determinista)
generarPuntosPatrulla :: Id -> [Position]
generarPuntosPatrulla id = take 11 $ zip xs ys
  where
    xs = [ fromIntegral ((id * i * 611) `mod` 500) - 250 | i <- [1..] ]
    ys = [ fromIntegral ((id * i * 456) `mod` 500) - 250 | i <- [1..] ]

-- ================================
-- Estado inicial
-- ================================

estadoInicial :: Picture -> Picture -> Picture -> MundoGloss
estadoInicial inicio fondo victoria = MundoGloss
  { worldState = World
      { robots =
          [ -- Alumno 1: Speedster
            Robot
              { idR = 1
              , commonR = CommonData 1 0 (-150, -100) (0, 0) (40, 50) (generarPuntosPatrulla 1)
              , healthR = 70
              , radarRange = 120
              , turret = Turret 1 (1, 0) 0 
                  (Projectile 1 (CommonData 1 8 (0,0) (250, 0) (chicleRadius*2, chicleRadius*2) []) 1000)
                  0    -- turretAction
                  0.6  -- shoot
              , haveExploded = False
              }
          , -- Alumno 2: Tank
            Robot
              { idR = 2
              , commonR = CommonData 2 0 (150, -100) (0, 0) (40, 50) (generarPuntosPatrulla 2)
              , healthR = 180
              , radarRange = 80
              , turret = Turret 2 (-1, 0) 180 
                  (Projectile 2 (CommonData 2 18 (0,0) (-180, 0) (chicleRadius*2, chicleRadius*2) []) 1000)
                  0    -- turretAction
                  1.6  -- shoot
              , haveExploded = False
              }
          , -- Alumno 3: Soporte
            Robot
              { idR = 3
              , commonR = CommonData 3 0 (-150, 50) (0, 0) (40, 50) (generarPuntosPatrulla 3)
              , healthR = 110
              , radarRange = 160
              , turret = Turret 3 (1, 0) 0 
                  (Projectile 3 (CommonData 3 6 (0,0) (200, 0) (chicleRadius*2, chicleRadius*2) []) 1000)
                  0    -- turretAction
                  1.2  -- shoot
              , haveExploded = False
              }
          , -- Alumno 4: All-rounder
            Robot
              { idR = 4
              , commonR = CommonData 4 0 (150, 50) (0, 0) (40, 50) (generarPuntosPatrulla 4)
              , healthR = 110
              , radarRange = 120
              , turret = Turret 4 (-1, 0) 180 
                  (Projectile 4 (CommonData 4 10 (0,0) (-220, 0) (chicleRadius*2, chicleRadius*2) []) 1000)
                  0    -- turretAction
                  0.9  -- shoot
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
  , explosiones = []
  , burbujas = []
  }

proyectilBase :: Id -> Projectile
proyectilBase i = Projectile
  { idP = i
  , commonP = CommonData i 10 (0, 0) (0, 0) (chicleRadius*2, chicleRadius*2) []
  , rangeP = 1000
  }

-- ================================
-- Dibujo
-- ================================
dibujar :: MundoGloss -> Picture
dibujar m = case modo m of
  Inicio  -> Pictures [ imagenInicio m, dibujarBoton ]
  Jugando ->
    let w = worldState m
    in Pictures
      [ fondoJuego m
      , dibujarProfe (0, 160)
      , Pictures (map dibujarNino [r | r <- robots w, healthR r > 0])
      , Pictures (map dibujarBurbujaMuerte (burbujas m))
      , Pictures (map dibujarChicle (projectiles w))
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

-- ================================
-- PUTINFO
-- ================================
dibujarPutInfo :: MundoGloss -> Picture
dibujarPutInfo m =
  let w = worldState m
      vivos = length [ r | r <- robots w, healthR r > 0 ]
      proyectilesActivos = length (projectiles w)
      exps = length (explosiones m)
      infoLines = [ "INFORMACION", "Alumnos vivos: " ++ show vivos, "Chicles activos: " ++ show proyectilesActivos, "Explosiones: " ++ show exps ]
      fondo = Color (makeColor 0 0 0 0.4) $ Translate 220 235 $ rectangleSolid 250 100
      linePictures = [ Translate 130 (260 - fromIntegral i * 25) $ Scale 0.15 0.15 $ Color white $ Text line | (i, line) <- zip [0..] infoLines ]
  in Pictures (fondo : linePictures)

-- ================================
-- Eventos
-- ================================
manejarEvento :: Event -> MundoGloss -> MundoGloss
manejarEvento (EventKey (MouseButton LeftButton) Down _ pos) m
  | modo m == Inicio, dentroBoton pos = m { modo = Jugando }
  | otherwise = m
-- Ya no hay tecla de espacio ni movimiento
manejarEvento _ m = m


-- ================================
-- Patrulla con puntos aleatorios
-- ================================
velocidadPorRol :: Id -> Float
velocidadPorRol 1 = 220
velocidadPorRol 2 = 60
velocidadPorRol 3 = 100
velocidadPorRol 4 = 120
velocidadPorRol _ = 100

-- Mueve el robot hacia el primer punto de su lista de patrulla
comportamientoNino :: Float -> Robot -> Robot
comportamientoNino dt r
  | healthR r <= 0 = r
  | otherwise =
      let pts = points (commonR r)
          vel = velocidadPorRol (idR r)
          posActual = position (commonR r)
      in if null pts
           then r
           else
             let objetivo = head pts
                 dist = distanceBetween posActual objetivo
             in if dist < 20
                  then
                    let nuevosPts = tail pts ++ [head pts]
                    in r { commonR = (commonR r) { points = nuevosPts } }
                  else
                    let ang = angleToTarget posActual objetivo
                        rad = deg2rad ang
                        dx = cos rad * vel * dt
                        dy = sin rad * vel * dt
                        nuevaPos = clampDentro (fst posActual + dx, snd posActual + dy)
                    in r { commonR = (commonR r) { position = nuevaPos } }

-- ================================
-- Disparo condicional
-- ================================
robotQuiereDisparar :: World -> Robot -> Bool
robotQuiereDisparar world me =
  any (detectedAgent me) [r | r <- robots world, idR r /= idR me, isRobotAlive r]

pasoShooting :: Float -> World -> ([Robot], [Projectile])
pasoShooting dt world = loop (robots world) [] []
  where
    loop [] accR accP = (reverse accR, reverse accP)
    loop (r:xs) accR accP =
      let t0  = shoot (turret r)
          t1  = max 0 (t0 - dt) -- Cuanto cooldown queda 
          rCD = r { turret = (turret r) { shoot = t1 } } -- Actualiza el cooldown y el turret
      in
        if healthR rCD <= 0 || not (robotQuiereDisparar world rCD)
          then loop xs (rCD:accR) accP
          else if t1 > 0
            then loop xs (rCD:accR) accP
            else
              let (x, y)   = position (commonR rCD)
                  (baseVx, _) = velocity (commonP (projectileT (turret rCD)))
                  vx = if baseVx >= 0 then abs baseVx else -abs baseVx
                  offX = if vx >= 0 then 20 else -20
                  p = Projectile
                        { idP     = idR rCD
                        , commonP = (commonP (projectileT (turret rCD)))
                                      {position = (x + offX, y + 28)
                                      , velocity = (vx, 0)
                                      }
                        , rangeP  = 1000
                        }
                  cooldown = case idR rCD of
                    1 -> 0.6
                    2 -> 1.6
                    3 -> 1.2
                    4 -> 0.9
                    _ -> 1.0
                  r' = rCD { turret = (turret rCD) { shoot = cooldown } }
              in loop xs (r':accR) (p:accP)

-- ================================
-- Apuntar torreta al enemigo más cercano
-- ================================
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
                 rad = deg2rad ang
                 vec = (cos rad, sin rad)
             in r { turret = (turret r) { angleT = ang, vectorT = vec } }

-- ================================
-- Curación automática para el Soporte (Alumno 3)
-- ================================
curarSoporte :: Float -> Robot -> Robot
curarSoporte dt r
  | idR r /= 3 = r
  | not (isRobotAlive r) = r
  | nuevoTiempo >= 10 =
      r { healthR = vidaNueva
        , turret = (turret r) { turretAction = 0 }
        }
  | otherwise =
      r { turret = (turret r) { turretAction = nuevoTiempo }
        }
  where
    tiempoActual = turretAction (turret r)
    nuevoTiempo  = tiempoActual + dt
    vidaNueva    = min 110 (healthR r + 2)

-- ================================
-- Actualización
-- ================================
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
      let w = worldState m

          -- Movimiento con patrulla
          rs0 = robots w
          rs1 = map (comportamientoNino dt) rs0

          -- Apuntar torretas
          rs2 = map (apuntarTorreta w) rs1

          -- Disparo condicional
          (rs3, nuevosProj) = pasoShooting dt w{robots = rs2}

          -- Curación para el Soporte
          rs4 = map (curarSoporte dt) rs3

          ps0 = projectiles w ++ nuevosProj

          -- Mover proyectiles
          psMovidos =
            [ p { commonP = (commonP p) { position = (x + vx * dt, y + vy * dt) } }
            | p <- ps0
            , let (x, y) = position (commonP p)
                  (vx, vy) = velocity (commonP p)
            , x > -ancho/2 && x < ancho/2 && y > -alto/2 && y < alto/2
            ]

          -- Colisiones
          impactos =
            [ (idR r, idP p, damage (commonP p), position (commonP p))
            | r <- rs4
            , healthR r > 0                   
            , p <- psMovidos
            , idR r /= idP p
            , circleAABB (position (commonP p)) chicleRadius (ninoBox r)
            ]

          -- Daño
          rsDanyados =
            [ if any (\(idr, _, _, _) -> idr == idR r) impactos
              then let totalDaño = sum [ d | (idr, _, d, _) <- impactos, idr == idR r ]
                  in r { healthR = max 0 (healthR r - totalDaño)
          , haveExploded = if healthR r - totalDaño <= 0 then True else haveExploded r
          }

              else r
            | r <- rs4
            ]

          -- Explosiones y burbujas
          nuevasExplosiones =
            [ Explosion pos (30, 0) 0.6 (RobotHitByProjectile rid pid dmg pos)
            | (rid, pid, dmg, pos) <- impactos
            ]

          -- Crear burbujas solo UNA vez por jugador muerto
          nuevasBurbujas =
            [ BurbujaMuerte (position (commonR r)) 3.5 (idR r)
            | r <- rsDanyados
            , healthR r <= 0
            , notElem (idR r) [ rid | BurbujaMuerte _ _ rid <- burbujas m ]
            , not (haveExploded r)  -- evita que se repita si ya explotó
            ]

          -- Actualizar las burbujas (reducir TTL y eliminarlas al pasar 0s)
          burbAct =
            [ BurbujaMuerte pos ttl' rid
            | BurbujaMuerte pos ttl rid <- burbujas m ++ nuevasBurbujas
            , let ttl' = ttl - dt
            , ttl' > 0
            ]


          psRestantes =
            [ p | p <- psMovidos, not (any (\(_, pid, _, _) -> pid == idP p) impactos) ]

          expsAct = [ Explosion pos size (ttl - dt) src | Explosion pos size ttl src <- explosiones m ++ nuevasExplosiones, ttl - dt > 0 ]
          -- Actualizar las burbujas de chicle (reducir su tiempo de vida y eliminarlas al pasar 0s)

          w' = w { robots = rsDanyados, projectiles = psRestantes }
          vivos = [ r | r <- rsDanyados, healthR r > 0 ]

      in case vivos of
          [ultimo] -> m { worldState = w', explosiones = expsAct, modo = Victoria (idR ultimo), burbujas = burbAct }
          []       -> m { worldState = w', explosiones = expsAct, modo = Victoria 0, burbujas = burbAct }
          _        -> m { worldState = w', explosiones = expsAct, burbujas = burbAct }