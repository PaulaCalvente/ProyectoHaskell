module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

import Config.World
import Config.Dibujar
import Data.Mundo
import Utils

-- AÑADIDOS
import Data.Mundo (ancho, alto)  -- para usar el tamaño real del área de juego
import Control.Monad (replicateM)
import Test.QuickCheck (Gen, generate, choose, suchThat)

--------------------------------------------------------------------------------
-- Generadores QuickCheck para posiciones iniciales
--------------------------------------------------------------------------------

genPosicion :: Gen (Float, Float)
genPosicion = do
  x <- choose (-ancho/2, ancho/2)
  y <- choose (-alto/2,  alto/2)
  pure (x, y)

distMin :: Float
distMin = 80

genPosicionUnica :: [(Float, Float)] -> Gen (Float, Float)
genPosicionUnica existentes = do
  p <- genPosicion
  if all (\q -> distanceBetween p q > distMin) existentes
     && not (estaDentroDeEscritorio p)  -- CONDICIÓN: No puede estar dentro de un escritorio
     then return p
     else genPosicionUnica existentes

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

    --  NUEVO: cargar profesor enfadado
    maybeProfeEnfadado <- loadJuicyPNG "imagenes/imagenesPNG/profesor_enfadado.png"
    case maybeProfeEnfadado of
      Nothing -> putStrLn "Advertencia: No se pudo cargar profesor_enfadado.png. Se usará profe normal."
      Just _  -> pure ()

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

    maybeExplosionComida <- loadJuicyPNG "imagenes/imagenesPNG/explosionComida.png"
    case maybeExplosionComida of
      Nothing -> putStrLn "Advertencia: No se pudo cargar explosionComida.png. Se usará fallback."
      Just _  -> pure ()

    maybeExplosionProfesor <- loadJuicyPNG "imagenes/imagenesPNG/explosionProfesor.png"
    case maybeExplosionProfesor of
      Nothing -> putStrLn "Advertencia: No se pudo cargar explosionProfesor.png. Se usará un marcador de posición."
      Just _  -> pure ()

    maybeEscritorio <- loadJuicyPNG "imagenes/imagenesPNG/escritorio.png"
    case maybeEscritorio of
      Nothing -> putStrLn "Advertencia: No se pudo cargar escritorio.png. Se usará un marcador de posición."
      Just _  -> pure ()

    maybeSandwich <- loadJuicyPNG "imagenes/imagenesPNG/sandwich.png"
    case maybeSandwich of
      Nothing -> putStrLn "Advertencia: No se pudo cargar sandwich.png. Se usará un marcador de posición."
      Just _  -> pure ()

    maybeZumo <- loadJuicyPNG "imagenes/imagenesPNG/zumo.png"
    case maybeZumo of
      Nothing -> putStrLn "Advertencia: No se pudo cargar zumo.png. Se usará un marcador de posición."
      Just _  -> pure ()

    maybePlatano <- loadJuicyPNG "imagenes/imagenesPNG/platano.png"
    case maybePlatano of
      Nothing -> putStrLn "Advertencia: No se pudo cargar platano.png. Se usará un marcador de posición."
      Just _  -> pure ()

    --  Generar posiciones con QuickCheck, acotadas y separadas
    [pos1, pos2, pos3, pos4, posSandwich1, posSandwich2, posZumo1, posZumo2, posPlatano1, posPlatano2] <- generate (generarPosiciones 10)

    -- Crear mundo inicial con posiciones generadas
    let mundo = estadoInicial inicio clase victoria derrota
                      maybeRobot1 maybeRobot2 maybeRobot3 maybeRobot4
                      maybeTorreta maybeProfe maybeProfeEnfadado maybeProyectil 
                      maybeExplosion1 maybeExplosion2 maybeExplosion3 maybeExplosionMuerte maybeEscritorio
                      maybeSandwich maybeZumo maybePlatano maybeExplosionComida maybeExplosionProfesor
                      pos1 pos2 pos3 pos4
                      posSandwich1 posSandwich2
                      posZumo1 posZumo2
                      posPlatano1 posPlatano2 

    -- Ejecutar el juego
    play
      (InWindow "Niños y Chicles" (round ancho, round alto) (100, 100))
      white
      60
      mundo
      dibujar
      manejarEvento
      actualizar
      
