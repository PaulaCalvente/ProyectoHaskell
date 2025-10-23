module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
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

-- Main.hs (ESTRUCTURA CORREGIDA)

main :: IO ()
main = do
  inicio <- loadBMP "imagenes/imagenesBMP/inicio.bmp"
  clase <- loadBMP "imagenes/imagenesBMP/clase.bmp"
  victoria <- loadBMP "imagenes/imagenesBMP/victoria.bmp"
  derrota <- loadBMP "imagenes/imagenesBMP/derrota.bmp"
  maybeTorreta <- loadJuicyPNG "imagenes/imagenesPNG/torreta.png"
  case maybeTorreta of
    Nothing -> putStrLn "Advertencia: No se pudo cargar Torreta.png. Se usará un marcador de posición."
    Just _ -> return () -- No hace nada, solo continúa con el flujo IO.
  profesor <- loadBMP "imagenes/imagenesBMP/profe.bmp"

  -- 1. Carga el PNG: el resultado es IO (Maybe Picture)
  maybeRobot1 <- loadJuicyPNG "imagenes/imagenesPNG/Robot1.png"

  -- 2. No se usa un 'case' para controlar el flujo principal (la llamada a 'play').
  --    La función 'play' DEBE ejecutarse siempre, por lo que el 'case' debe ser más pequeño.
  --    Para seguir el flujo recomendado, usa la variable Maybe Picture en estadoInicial,
  --    y solo usa el 'case' para manejar el error de carga de forma simple.
  
  case maybeRobot1 of
    Nothing -> putStrLn "Advertencia: No se pudo cargar Robot1.png. Se usará un marcador de posición."
    Just _ -> return () -- No hace nada, solo continúa con el flujo IO.
  
  -- 3. La llamada a 'play' se realiza fuera del 'case' principal, asegurando su ejecución.
  play
    (InWindow "Niños y Chicles" (round ancho, round alto) (100, 100))
    white
    60
    -- La función estadoInicial debe estar corregida para aceptar Maybe Picture aquí.
    (estadoInicial inicio clase victoria derrota maybeRobot1 maybeTorreta profesor) 
    dibujar
    manejarEvento
    actualizar