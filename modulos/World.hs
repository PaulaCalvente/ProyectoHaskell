module World where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Types
import Utils

-- Estado que usa Gloss
data MundoGloss = MundoGloss
  { worldState   :: World
  , modo         :: Modo
  , imagenInicio :: Picture
  , fondoJuego   :: Picture
  , imagenVictoria :: Picture
  , explosiones  :: [Explosion]
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
              , damageR = 10
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
              , damageR = 10
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
  Inicio  -> Pictures [imagenInicio m, dibujarBoton]

  Jugando ->
    let w = worldState m
    in Pictures
      [ fondoJuego m
      , dibujarProfe (0, 160)
      , dibujarHUD (robots w)
      , Pictures (map dibujarNino (robots w))
      , Pictures (map dibujarChicle (projectiles w))
      , Pictures (map dibujarExplosion (explosiones m))
      ]

  Victoria rid ->
    Pictures
      [ imagenVictoria m
      , Translate (-240) (135) $
          Scale 0.27 0.27 $
          Color black $
          Text ( "Alumno " ++ show rid ++ " es el ganador")
      ]


-- ================================
-- Eventos
-- ================================

manejarEvento :: Event -> MundoGloss -> MundoGloss
manejarEvento (EventKey (MouseButton LeftButton) Down _ pos) m
  | modo m == Inicio, dentroBoton pos = m { modo = Jugando }
  | otherwise = m

manejarEvento (EventKey (SpecialKey KeySpace) Down _ _) m
  | modo m == Jugando = m { worldState = dispararTodos (worldState m) }
manejarEvento (EventKey (SpecialKey KeyUp) Down _ _) m
  | modo m == Jugando = moverRobots 0 10 m
manejarEvento (EventKey (SpecialKey KeyDown) Down _ _) m
  | modo m == Jugando = moverRobots 0 (-10) m
manejarEvento (EventKey (SpecialKey KeyLeft) Down _ _) m
  | modo m == Jugando = moverRobots (-10) 0 m
manejarEvento (EventKey (SpecialKey KeyRight) Down _ _) m
  | modo m == Jugando = moverRobots 10 0 m
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
                      { position = (x + offset, y + 10)
                      , velocity = (vx, 0)
                      }
          , damageP = damageR r
          , rangeP = 1000
          }
      | (i, r) <- zip [0..] (robots w)
      , let (x, y) = position (commonR r)
            vx = if even i then velChicleVel else -velChicleVel
            offset = if even i then 20 else -20
      ]

-- ================================
-- Actualización
-- ================================

-- ================================
-- Actualización + Daño
-- ================================

actualizar :: Float -> MundoGloss -> MundoGloss
actualizar dt m
  | modo m == Inicio = m
  | otherwise =
      let w = worldState m
          rs = robots w
          ps = projectiles w

          -- Mover proyectiles
          psMovidos =
            [ p { commonP = (commonP p)
                    { position = (x + vx * dt, y + vy * dt) } }
            | p <- ps
            , let (x, y) = position (commonP p)
                  (vx, vy) = velocity (commonP p)
            , x > -ancho/2 && x < ancho/2 && y > -alto/2 && y < alto/2
            ]

          -- Detectar colisiones entre proyectiles y robots
          impactos =
            [ (idR r, idP p, damageP p, position (commonP p))
            | r <- rs
            , p <- psMovidos
            , idR r /= idP p
            , circleAABB (position (commonP p)) chicleRadius (ninoBox r)
            ]

          -- Reducir la vida de los niños golpeados
          rsDanyados =
            [ if any (\(idr, _, _, _) -> idr == idR r) impactos
              then let totalDaño = sum [ d | (idr, _, d, _) <- impactos, idr == idR r ]
                   in r { healthR = max 0 (healthR r - totalDaño) }
              else r
            | r <- rs
            ]

          -- Crear explosiones visuales en los impactos
          nuevasExplosiones =
            [ Explosion pos (30, 0) 0.6
                (RobotHitByProjectile rid pid dmg pos)
            | (rid, pid, dmg, pos) <- impactos
            ]

          -- Eliminar proyectiles que impactaron
          psRestantes =
            [ p
            | p <- psMovidos
            , not (any (\(_, pid, _, _) -> pid == idP p) impactos)
            ]

          -- Actualizar explosiones activas (reducir duración)
          expsAct =
            [ Explosion pos size (ttl - dt) src
            | Explosion pos size ttl src <- explosiones m ++ nuevasExplosiones
            , ttl - dt > 0
            ]

          w' = w { robots = rsDanyados, projectiles = psRestantes }

          -- Contar robots vivos
          vivos = [ r | r <- rsDanyados, healthR r > 0 ]

      in case vivos of
          [ultimo] -> m { worldState = w', explosiones = expsAct, modo = Victoria (idR ultimo) }
          _        -> m { worldState = w', explosiones = expsAct }

