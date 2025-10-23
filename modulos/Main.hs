module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
--import GlossJuicy.Graphics.Gloss.Juicy
import Config.World
import Config.Dibujar

import Data.Mundo

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
  inicio <- loadBMP "imagenes/inicio.bmp"
  clase  <- loadBMP "imagenes/clase.bmp"
  victoria <- loadBMP "imagenes/victoria.bmp"
  derrota <- loadBMP "imagenes/derrota.bmp"
  robot1 <- loadBMP "imagenes/Robot1.bmp"
  --robot2 <- loadBMP "imagenes/robot2.bmp"
  --robot3 <- loadBMP "imagenes/robot3.bmp"
  --robot4 <- loadBMP "imagenes/robot4.bmp"
  torreta <- loadBMP "imagenes/torreta.bmp"
  profesor <- loadBMP "imagenes/profe.bmp"
  play
    (InWindow "Niños y Chicles" (round ancho, round alto) (100, 100))
    white
    60
    (estadoInicial inicio clase victoria derrota robot1 torreta profesor)
    dibujar
    manejarEvento
    actualizar