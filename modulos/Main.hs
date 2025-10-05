module Main where

import Types
import Collision
import World
import BotAction
import Movement

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


main :: IO ()
main = putStrLn "Módulo principal cargado correctamente"