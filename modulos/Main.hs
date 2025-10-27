module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

import Config.World
import Config.Dibujar
import Data.Mundo
import Utils

-- 🔽 AÑADIDOS
import Data.Mundo (ancho, alto)  -- para usar el tamaño real del área de juego
import Control.Monad (replicateM)
import Test.QuickCheck (Gen, generate, choose, suchThat)

--------------------------------------------------------------------------------
-- Generadores QuickCheck para posiciones iniciales
--------------------------------------------------------------------------------

-- Genera una posición dentro del área jugable (centrada en 0,0)
genPosicion :: Gen (Float, Float)
genPosicion = do
  x <- choose (-ancho/2, ancho/2)
  y <- choose (-alto/2,  alto/2)
  pure (x, y)


-- Distancia mínima para evitar solapamiento inicial entre robots
distMin :: Float
distMin = 80

-- ✅ Genera una posición que no esté demasiado cerca de las existentes
genPosicionUnica :: [(Float, Float)] -> Gen (Float, Float)
genPosicionUnica existentes = do
  p <- genPosicion
  if all (\q -> distanceBetween p q > distMin) existentes
     then return p
     else genPosicionUnica existentes

-- Genera N posiciones válidas (acotadas al área y separadas entre sí)
generarPosiciones :: Int -> Gen [(Float, Float)]
generarPosiciones n = go n []
  where
    go 0 acc = pure (reverse acc)
    go k acc = do
      p <- genPosicionUnica acc
      go (k - 1) (p : acc)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    -- Cargar imágenes BMP
    inicio   <- loadBMP "imagenes/imagenesBMP/inicio.bmp"
    clase    <- loadBMP "imagenes/imagenesBMP/clase.bmp"
    victoria <- loadBMP "imagenes/imagenesBMP/victoria.bmp"
    derrota  <- loadBMP "imagenes/imagenesBMP/derrota.bmp"

    -- Cargar imágenes PNG con chequeo
    maybeTorreta <- loadJuicyPNG "imagenes/imagenesPNG/torreta.png"
    case maybeTorreta of 
      Nothing -> putStrLn "Advertencia: No se pudo cargar torreta.png. Se usará un marcador de posición."
      Just _  -> pure () 

    profesor <- loadBMP "imagenes/imagenesBMP/profe.bmp"

    maybeRobot1 <- loadJuicyPNG "imagenes/imagenesPNG/Robot1.png"
    case maybeRobot1 of
      Nothing -> putStrLn "Advertencia: No se pudo cargar Robot1.png. Se usará un marcador de posición."
      Just _  -> pure () 

    maybeRobot2 <- loadJuicyPNG "imagenes/imagenesPNG/Robot2.png"
    case maybeRobot2 of
      Nothing -> putStrLn "Advertencia: No se pudo cargar Robot2.png. Se usará un marcador de posición."
      Just _  -> pure () 

    maybeRobot3 <- loadJuicyPNG "imagenes/imagenesPNG/Robot3.png"
    case maybeRobot3 of
      Nothing -> putStrLn "Advertencia: No se pudo cargar Robot3.png. Se usará un marcador de posición."
      Just _  -> pure ()

    maybeRobot4 <- loadJuicyPNG "imagenes/imagenesPNG/Robot4.png"
    case maybeRobot4 of
      Nothing -> putStrLn "Advertencia: No se pudo cargar Robot4.png. Se usará un marcador de posición."
      Just _  -> pure () 

    maybeProfe <- loadJuicyPNG "imagenes/imagenesPNG/profe.png"
    case maybeProfe of
      Nothing -> putStrLn "Advertencia: No se pudo cargar profe.png. Se usará un marcador de posición."
      Just _  -> pure () 

    maybeProyectil <- loadJuicyPNG "imagenes/imagenesPNG/chicle.png"
    case maybeProyectil of
      Nothing -> putStrLn "Advertencia: No se pudo cargar chicle.png. Se usará un marcador de posición."
      Just _  -> pure ()

    maybeExplosion1 <- loadJuicyPNG "imagenes/imagenesPNG/explosion1.png"
    case maybeExplosion1 of
      Nothing -> putStrLn "Advertencia: No se pudo cargar explosion1.png. Se usará un marcador de posición."
      Just _  -> pure ()

    maybeExplosion2 <- loadJuicyPNG "imagenes/imagenesPNG/explosion2.png"
    case maybeExplosion2 of
      Nothing -> putStrLn "Advertencia: No se pudo cargar explosion2.png. Se usará un marcador de posición."
      Just _  -> pure ()

    maybeExplosion3 <- loadJuicyPNG "imagenes/imagenesPNG/explosion3.png"
    case maybeExplosion3 of
      Nothing -> putStrLn "Advertencia: No se pudo cargar explosion3.png. Se usará un marcador de posición."
      Just _  -> pure ()
    
    maybeExplosionMuerte <- loadJuicyPNG "imagenes/imagenesPNG/chicleMuerte.png"
    case maybeExplosionMuerte of
      Nothing -> putStrLn "Advertencia: No se pudo cargar chicleMuerte.png. Se usará un marcador de posición."
      Just _  -> pure ()


    -- ⬇️ Generar posiciones con QuickCheck, acotadas y separadas
    [pos1, pos2, pos3, pos4] <- generate (generarPosiciones 4)

    -- Crear mundo inicial con posiciones generadas
    let mundo = estadoInicial inicio clase victoria derrota
                           maybeRobot1 maybeRobot2 maybeRobot3 maybeRobot4
                           maybeTorreta maybeProfe maybeProyectil
                           maybeExplosion1 maybeExplosion2 maybeExplosion3 maybeExplosionMuerte
                           pos1 pos2 pos3 pos4

    -- Ejecutar el juego. OJO: usamos (round ancho, round alto) del juego real
    play
      (InWindow "Niños y Chicles" (round ancho, round alto) (100, 100))
      white
      60
      mundo
      dibujar
      manejarEvento
      actualizar
