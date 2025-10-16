module Main where

-- import Types
-- import Movement
-- import BotAction
-- import Collision

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import World
import Utils

{-
myWorld :: World
myWorld = World
  { robots =
      [ Robot
          { idR = 1
          , commonR = CommonData 1 10 (0, 0) (0, 0) (10, 10) [(0,0)]
          , healthR = 100
          , radarRange = 100
          , turret = Turret 1 (0, 0) 0 (Projectile 1 (CommonData 1 5 (0,0) (0,0) (5,5) [(0,0)]) 10 50) 0 0
          , haveExploded = False
          , damageR = 5
          }
      , Robot
          { idR = 2
          , commonR = CommonData 2 10 (30, 0) (0, 0) (10, 10) [(0,0)]
          , healthR = 100
          , radarRange = 100
          , turret = Turret 2 (0, 0) 0 (Projectile 2 (CommonData 2 5 (0,0) (0,0) (5,5) [(0,0)]) 10 50) 0 0
          , haveExploded = False
          , damageR = 5
          }
      ]
  , projectiles = []
  , turrets = []
  , robotHits = []
  }

-- Bot de ejemplo: recibe el mundo y su propio Id, devuelve un comando
exampleBot :: World -> Id -> BotCmd -- Funcion que si detecta un robot en su zona le dispara, si no patruya unos puntos definidos
exampleBot world myId
  | not (null nearbyEnemies) = ShootAt (TRobot (idR (head nearbyEnemies))) -- Cogemos al primer robot de la lista de cercanos, con idR
                                                                           -- sacamos su ID de robot, con TRobot lo apuntamos (esta
                                                                           -- definido en BotAction y con ShootAt le disparamos)
  | otherwise                = Patrol [(0,0), (50,0), (50,50), (0,50)]
  where
    myRobot = head [r | r <- robots world, idR r == myId]
    nearbyEnemies = [ r | r <- robots world -- Busco los robots del mundo
                        , idR r /= myId -- No soy yo
                        , isRobotAlive r -- Está vivo
                        , detectedAgent myRobot r ] -- Lo detecto con mi radar
-}

main :: IO ()
main = do
  inicio <- loadBMP "inicio.bmp"
  clase  <- loadBMP "clase.bmp"
  play
    (InWindow "Niños y Chicles" (round ancho, round alto) (100, 100))
    white
    60
    (estadoInicial inicio clase)
    dibujar
    manejarEvento
    actualizar