module World where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Types
import Utils

-- Estado que usa Gloss
data MundoGloss = MundoGloss
  { worldState     :: World
  , modo           :: Modo
  , imagenInicio   :: Picture
  , fondoJuego     :: Picture
  , imagenVictoria :: Picture
  , explosiones    :: [Explosion]
  , burbujas       :: [BurbujaMuerte]   -- 💥 nuevo campo
  }

-- ================================
-- Estado inicial
-- ================================

estadoInicial :: Picture -> Picture -> Picture -> MundoGloss
estadoInicial inicio fondo victoria = MundoGloss
  { worldState = World
      { robots =
          [ Robot
              { idR = 1
              , commonR = CommonData 1 0 (-150, -100) (0, 0) (40, 50) []
              , healthR = 100
              , radarRange = 0
              , turret = Turret 1 (1, 0) 0 (proyectilBase 1) 0 0
              , haveExploded = False
              , damageR = 20
              }
          , Robot
              { idR = 2
              , commonR = CommonData 2 0 (150, -100) (0, 0) (40, 50) []
              , healthR = 100
              , radarRange = 0
              , turret = Turret 2 (-1, 0) 180 (proyectilBase 2) 0 0
              , haveExploded = False
              , damageR = 10
              }
          , Robot
              { idR = 3
              , commonR = CommonData 3 0 (-150, 50) (0, 0) (40, 50) []
              , healthR = 100
              , radarRange = 0
              , turret = Turret 3 (1, 0) 0 (proyectilBase 3) 0 0
              , haveExploded = False
              , damageR = 20
              }
          , Robot
              { idR = 4
              , commonR = CommonData 4 0 (150, 50) (0, 0) (40, 50) []
              , healthR = 100
              , radarRange = 0
              , turret = Turret 4 (-1, 0) 180 (proyectilBase 4) 0 0
              , haveExploded = False
              , damageR = 10
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
  , burbujas = []    -- 💥 añadido
  }

proyectilBase :: Id -> Projectile
proyectilBase i = Projectile
  { idP = i
  , commonP = CommonData i 0 (0, 0) (0, 0) (chicleRadius*2, chicleRadius*2) []
  , damageP = 10
  , rangeP = 1000
  }

-- ================================
-- Dibujo
-- ================================
dibujar :: MundoGloss -> Picture
dibujar m = case modo m of
  -- Pantalla de inicio
  Inicio  -> Pictures
    [ imagenInicio m
    , dibujarBoton
    ]  -- No mostramos info aquí para no tapar el título

  -- Pantalla de juego
  Jugando ->
    let w = worldState m
    in Pictures
      [ fondoJuego m
      , dibujarProfe (0, 160)

        -- 🧍‍♂️ Niños vivos
      , Pictures (map dibujarNino [r | r <- robots w, healthR r > 0])
      
        -- 🫧 Burbujas de niños eliminados (con animación)
      , Pictures (map dibujarBurbujaMuerte (burbujas m))


      , Pictures (map dibujarChicle (projectiles w))
      , Pictures (map dibujarExplosion (explosiones m))

      , dibujarHUD (robots w)   -- Panel de salud (izquierda)
      , dibujarPutInfo m        -- Panel informativo (derecha)
      ]

  -- Pantalla de victoria
  Victoria rid ->
    Pictures
      [ imagenVictoria m
      , dibujarPutInfo m
      , Translate (-240) 135 $
          Scale 0.27 0.27 $
          Color black $
          Text ("Alumno " ++ show rid ++ " es el ganador")
      ]

-- ================================
-- PUTINFO (esquina superior derecha)
-- ================================
dibujarPutInfo :: MundoGloss -> Picture
dibujarPutInfo m =
  let w = worldState m
      vivos = length [ r | r <- robots w, healthR r > 0 ]
      total = length (robots w)
      proyectilesActivos = length (projectiles w)
      exps = length (explosiones m)

      infoLines =
        [ "INFORMACION"
        , "Alumnos vivos: " ++ show vivos
        , "Chicles activos: " ++ show proyectilesActivos
        , "Explosiones: " ++ show exps
        ]

      fondo = Color (makeColor 0 0 0 0.4) $
                Translate 220 235 $
                  rectangleSolid 250 100

      linePictures =
        [ Translate 130 (260 - fromIntegral i * 25)
            $ Scale 0.15 0.15
            $ Color white
            $ Text line
        | (i, line) <- zip [0..] infoLines
        ]
  in Pictures (fondo : linePictures)

-- ================================
-- Eventos
-- ================================
manejarEvento :: Event -> MundoGloss -> MundoGloss
-- ✅ Mantiene el clic en el botón de inicio
manejarEvento (EventKey (MouseButton LeftButton) Down _ pos) m
  | modo m == Inicio, dentroBoton pos = m { modo = Jugando }
  | otherwise = m

-- ✅ Mantiene la tecla Espacio para disparar
manejarEvento (EventKey (SpecialKey KeySpace) Down _ _) m
  | modo m == Jugando = m { worldState = dispararTodos (worldState m) }

-- 🚫 Quitamos movimiento con flechas (no se mueven más)
manejarEvento (EventKey (SpecialKey KeyUp) Down _ _) m = m
manejarEvento (EventKey (SpecialKey KeyDown) Down _ _) m = m
manejarEvento (EventKey (SpecialKey KeyLeft) Down _ _) m = m
manejarEvento (EventKey (SpecialKey KeyRight) Down _ _) m = m

-- 🔹 Cualquier otro evento se ignora
manejarEvento _ m = m

moverRobots :: Float -> Float -> MundoGloss -> MundoGloss
moverRobots dx dy m = m { worldState = w' }
  where
    w = worldState m
    w' = w { robots = map mover (robots w) }
    mover r = r { commonR = (commonR r) { position = (x + dx, y + dy) } }
      where (x, y) = position (commonR r)

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
          , damageP = damageR r
          , rangeP = 1000
          }
      | (i, r) <- zip [0..] (robots w)
      , healthR r > 0  -- ✅ solo disparan los vivos
      , let (x, y) = position (commonR r)
            vx     = if even i then velChicleVel else -velChicleVel
            offset = if even i then 20 else -20
      ]

-- ================================
-- Actualización + Daño
-- ================================
-- Limita una posición dentro de la ventana (tamaño aprox. del niño: 40x50)
clampDentro :: (Float,Float) -> (Float,Float)
clampDentro (x,y) =
  let halfW = ancho/2
      halfH = alto/2
      -- margen para que no se corten en el borde (mitades del sprite)
      x' = max (-halfW + 20) (min (halfW - 20) x)
      y' = max (-halfH + 25) (min (halfH - 25) y)
  in (x', y')

-- Comportamiento por robot (muy simple):
-- 1: quieto (dispara en pasoShooting)
-- 2: camina a la derecha (dispara en pasoShooting)
-- 3: camina lento a la derecha (no dispara)
-- 4: quieto
comportamientoNino :: Float -> Robot -> Robot
comportamientoNino _dt r =
  case idR r of
    1 -> r
    2 -> mover ( 1.0, 0) r
    3 -> mover ( 0.5, 0) r
    4 -> r
    _ -> r
  where
    mover (vx,vy) rob =
      let (x,y) = position (commonR rob)
          pos'  = clampDentro (x+vx, y+vy)
      in rob { commonR = (commonR rob) { position = pos' } }

-- Qué robots disparan automáticamente 
robotQuiereDisparar :: Robot -> Bool
robotQuiereDisparar r = idR r == 1 || idR r == 2

-- Disparo automático con cooldown.
-- Si shoot <= 0, el robot está vivo y quiere disparar: crea proyectil horizontal (según vectorT) y reinicia cooldown.
-- Devuelve (robotsActualizados, proyectilesNuevos).
pasoShooting :: Float -> [Robot] -> ([Robot], [Projectile])
pasoShooting dt rs = loop rs [] []
  where
    cooldown = 0.6  -- ~un disparo cada 0.6 s

    loop [] accR accP = (reverse accR, reverse accP)
    loop (r:xs) accR accP =
      let t0  = shoot (turret r)
          t1  = max 0 (t0 - dt)
          rCD = r { turret = (turret r) { shoot = t1 } }
      in
        if healthR rCD <= 0 || not (robotQuiereDisparar rCD)
          then loop xs (rCD:accR) accP                         -- no dispara (muerto o no quiere)
          else if t1 > 0
            then loop xs (rCD:accR) accP                       -- en cooldown
            else
              -- Disparo horizontal según vectorT de la torreta
              let (x,y)   = position (commonR rCD)
                  (vx,vy) = vectorT (turret rCD)
                  vproj   = (vx * velChicleVel, vy * velChicleVel)
                  offX    = if vx >= 0 then 20 else -20        -- frente a la "boca"
                  p = Projectile
                        { idP     = idR rCD
                        , commonP = (commonP (projectileT (turret rCD)))
                                      { position = (x + offX, y + 28)
                                      , velocity = vproj
                                      }
                        , damageP = damageR rCD
                        , rangeP  = 1000
                        }
                  r' = rCD { turret = (turret rCD) { shoot = cooldown } }
              in loop xs (r':accR) (p:accP)


actualizar :: Float -> MundoGloss -> MundoGloss
actualizar dt m
  | modo m == Inicio = m
  | otherwise =
      let w = worldState m

          -- movimiento autónomo y límites (todos)
          rs0 = robots w
          rs1 = map (comportamientoNino dt) rs0

          -- disparo automático con cooldown (solo vivos disparan internamente)
          (rs2, nuevosProj) = pasoShooting dt rs1

          -- (proyectiles antes de mover
          ps0 = projectiles w ++ nuevosProj

          -- Mover proyectiles
          psMovidos =
            [ p { commonP = (commonP p)
                    { position = (x + vx * dt, y + vy * dt) } }
            | p <- ps0
            , let (x, y) = position (commonP p)
                  (vx, vy) = velocity (commonP p)
            , x > -ancho/2 && x < ancho/2 && y > -alto/2 && y < alto/2
            ]

          -- 💥 Detectar colisiones solo con los alumnos vivos
          impactos =
            [ (idR r, idP p, damageP p, position (commonP p))
            | r <- rs2
            , healthR r > 0                   
            , p <- psMovidos
            , idR r /= idP p
            , circleAABB (position (commonP p)) chicleRadius (ninoBox r)
            ]


          -- Reducir vida de los niños golpeados
          rsDanyados =
            [ if any (\(idr, _, _, _) -> idr == idR r) impactos
              then let totalDaño = sum [ d | (idr, _, d, _) <- impactos, idr == idR r ]
                   in r { healthR = max 0 (healthR r - totalDaño) }
              else r
            | r <- rs2
            ]

          -- Crear explosiones visuales
          nuevasExplosiones =
            [ Explosion pos (30, 0) 0.6
                (RobotHitByProjectile rid pid dmg pos)
            | (rid, pid, dmg, pos) <- impactos
            ]

          -- 💥 Crear burbujas para los alumnos muertos (duración 5 s)
          nuevasBurbujas =
            [ BurbujaMuerte (position (commonR r)) 5.0
            | r <- rsDanyados
            , healthR r <= 0
            , not (any (\b -> posBurbuja b == position (commonR r)) (burbujas m))
            ]

          -- Eliminar proyectiles que impactaron
          psRestantes =
            [ p
            | p <- psMovidos
            , not (any (\(_, pid, _, _) -> pid == idP p) impactos)
            ]

          -- Actualizar explosiones
          expsAct =
            [ Explosion pos size (ttl - dt) src
            | Explosion pos size ttl src <- explosiones m ++ nuevasExplosiones
            , ttl - dt > 0
            ]

          -- Actualizar burbujas (desaparecen tras 5 s)
          burbAct =
            [ BurbujaMuerte pos (ttl - dt)
            | BurbujaMuerte pos ttl <- burbujas m ++ nuevasBurbujas
            , ttl - dt > 0
            ]

          -- 💡 Mantenemos también los eliminados para que su barra siga visible
          w' = w { robots = rsDanyados, projectiles = psRestantes }

          -- Pero solo cuentan como "vivos" los que tienen vida > 0
          vivos = [ r | r <- rsDanyados, healthR r > 0 ]

      in case vivos of
          [ultimo] -> m { worldState = w', explosiones = expsAct, modo = Victoria (idR ultimo) }
          []       -> m { worldState = w', explosiones = expsAct, modo = Victoria 0 }
          _        -> m { worldState = w', explosiones = expsAct }


