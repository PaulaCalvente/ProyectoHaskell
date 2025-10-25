module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Config.World
import Config.Dibujar
import Config.Sonido (iniciarMusica, detenerMusica)
import Control.Concurrent (forkIO)
import Control.Monad (void)
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

-- Main.hs (ESTRUCTURA CORREGIDA)



main :: IO ()
main = do
    -- Reproducir música en segundo plano
    void $ forkIO $ iniciarMusica "musica.mp3"

    inicio <- loadBMP "imagenes/imagenesBMP/inicio.bmp"
    clase <- loadBMP "imagenes/imagenesBMP/clase.bmp"
    victoria <- loadBMP "imagenes/imagenesBMP/victoria.bmp"
    derrota <- loadBMP "imagenes/imagenesBMP/derrota.bmp"

    maybeTorreta <- loadJuicyPNG "imagenes/imagenesPNG/torreta.png"
    case maybeTorreta of 
      Nothing -> putStrLn "Advertencia: No se pudo cargar Torreta.png. Se usará un marcador de posición."
      Just _ -> return () 

    profesor <- loadBMP "imagenes/imagenesBMP/profe.bmp"

    maybeRobot1 <- loadJuicyPNG "imagenes/imagenesPNG/Robot1.png"
    case maybeRobot1 of
      Nothing -> putStrLn "Advertencia: No se pudo cargar Robot1.png. Se usará un marcador de posición."
      Just _ -> return () 

    maybeRobot2 <- loadJuicyPNG "imagenes/imagenesPNG/Robot2.png"
    case maybeRobot2 of
      Nothing -> putStrLn "Advertencia: No se pudo cargar Robot2.png. Se usará un marcador de posición."
      Just _ -> return () 

    maybeRobot3 <- loadJuicyPNG "imagenes/imagenesPNG/Robot3.png"
    case maybeRobot3 of
      Nothing -> putStrLn "Advertencia: No se pudo cargar Robot3.png. Se usará un marcador de posición."
      Just _ -> return () 

    maybeRobot4 <- loadJuicyPNG "imagenes/imagenesPNG/Robot4.png"
    case maybeRobot4 of
      Nothing -> putStrLn "Advertencia: No se pudo cargar Robot4.png. Se usará un marcador de posición."
      Just _ -> return ()

    maybeProfe <- loadJuicyPNG "imagenes/imagenesPNG/profe.png"
    case maybeProfe of
      Nothing -> putStrLn "Advertencia: No se pudo cargar profe.png. Se usará un marcador de posición."
      Just _ -> return () 

    maybeProyectil <- loadJuicyPNG "imagenes/imagenesPNG/chicle.png"
    case maybeProyectil of
      Nothing -> putStrLn "Advertencia: No se pudo cargar chicle.png. Se usará un marcador de posición."
      Just _ -> return ()

    maybeExplosion1 <- loadJuicyPNG "imagenes/imagenesPNG/explosion1.png"
    case maybeExplosion1 of
      Nothing -> putStrLn "Advertencia: No se pudo cargar explosion1.png. Se usará un marcador de posición."
      Just _ -> return ()

    maybeExplosion2 <- loadJuicyPNG "imagenes/imagenesPNG/explosion2.png"
    case maybeExplosion2 of
      Nothing -> putStrLn "Advertencia: No se pudo cargar explosion2.png. Se usará un marcador de posición."
      Just _ -> return ()

    maybeExplosion3 <- loadJuicyPNG "imagenes/imagenesPNG/explosion3.png"
    case maybeExplosion3 of
      Nothing -> putStrLn "Advertencia: No se pudo cargar explosion3.png. Se usará un marcador de posición."
      Just _ -> return ()

    play
      (InWindow "Niños y Chicles" (round ancho, round alto) (100, 100))
      white
      60
      (estadoInicial inicio clase victoria derrota maybeRobot1 maybeRobot2 maybeRobot3 maybeRobot4 maybeTorreta maybeProfe maybeProyectil maybeExplosion1 maybeExplosion2 maybeExplosion3) 
      dibujar
      manejarEvento
      actualizar

    detenerMusica
