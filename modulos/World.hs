module World where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Types
import Utils

-- Estado que usa Gloss
data MundoGloss = MundoGloss
  { worldState   :: World
  , modo         :: ModoJuego
  , imagenInicio :: Picture
  , fondoJuego   :: Picture
  , explosiones  :: [Explosion]
  }

-- ================================
-- Estado inicial
-- ================================

estadoInicial :: Picture -> Picture -> MundoGloss
estadoInicial inicio fondo = MundoGloss
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
  Jugando -> Pictures
    [ fondoJuego m
    , dibujarProfe (0, 160)
    , Pictures (map dibujarNino (robots (worldState m)))
    , Pictures (map dibujarChicle (projectiles (worldState m)))
    , Pictures (map dibujarExplosion (explosiones m))
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

actualizar :: Float -> MundoGloss -> MundoGloss
actualizar dt m
  | modo m == Inicio = m
  | otherwise =
      let w = worldState m
          rs = robots w
          ps = projectiles w
          -- Avanzar proyectiles
          ps' = [ p { commonP = (commonP p) { position = (x + vx*dt, y + vy*dt) } }
                | p <- ps
                , let (x, y) = position (commonP p)
                      (vx, vy) = velocity (commonP p)
                      dentro = x > -ancho/2 && x < ancho/2
                , dentro
                ]
          -- Detectar impactos
          (psFinales, nuevasExplosiones) = foldr (procesarProyectil rs) ([], []) ps'
          -- Actualizar explosiones existentes
          expsActualizadas = 
            [ Explosion pos size (ttl - dt) hit
            | Explosion pos size ttl hit <- explosiones m ++ nuevasExplosiones
            , ttl - dt > 0
            ]
      in m { worldState = w { projectiles = psFinales }, explosiones = expsActualizadas }
  where
    procesarProyectil :: [Robot] -> Projectile -> ([Projectile], [Explosion]) -> ([Projectile], [Explosion])
    procesarProyectil rs p (accP, accE) =
      let pos = position (commonP p)
          hit = any (\r -> idR r /= idP p && circleAABB pos chicleRadius (ninoBox r)) rs
      in if hit
           then (accP, Explosion pos (30, 0) 0.6 (RobotHitByProjectile (idP p) (idP p) (damageP p) pos) : accE)
           else (p : accP, accE)