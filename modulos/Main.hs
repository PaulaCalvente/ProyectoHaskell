module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

import Config.World
import Config.Dibujar
import Data.Mundo
import Utils
import Torneos
import Config.Generacion

-- AÑADIDOS
import Data.Mundo (ancho, alto)
import Control.Monad (replicateM)
import Test.QuickCheck (Gen, generate, choose)
import System.IO


--------------------------------------------------------------------------------
-- FUNCIÓN PRINCIPAL
--------------------------------------------------------------------------------

main :: IO ()
main = do
    ------------------------------------------------------------
    -- 1️º Leer configuración desde config.txt
    ------------------------------------------------------------
    cfg <- leerConfig "config.txt"
    putStrLn "Archivo de configuración cargado correctamente."
    putStrLn $ "Bots: " ++ show (bots cfg)
    putStrLn $ "Área: " ++ show (area cfg)
    putStrLn $ "Duración: " ++ show (duracion cfg) ++ "s"
    putStrLn $ "Torneos consecutivos: " ++ show (numTorneos cfg)

    ------------------------------------------------------------
    -- 2º Cargar imágenes del juego
    ------------------------------------------------------------
    inicio   <- loadBMP "imagenes/imagenesBMP/inicio.bmp"
    clase    <- loadBMP "imagenes/imagenesBMP/clase.bmp"
    victoria <- loadBMP "imagenes/imagenesBMP/victoria.bmp"
    derrota  <- loadBMP "imagenes/imagenesBMP/derrota.bmp"
    imagenCarga <- loadBMP "imagenes/imagenesBMP/imagenTorneo.bmp"


    let cargarPNG ruta = do
          img <- loadJuicyPNG ruta
          case img of
            Nothing -> putStrLn ("Advertencia: No se pudo cargar " ++ ruta)
            Just _  -> pure ()
          return img

    maybeTorreta <- cargarPNG "imagenes/imagenesPNG/torreta.png"
    maybeProfe <- cargarPNG "imagenes/imagenesPNG/profe.png"
    maybeProfeEnfadado <- cargarPNG "imagenes/imagenesPNG/profesor_enfadado.png"
    maybeRobot1 <- cargarPNG "imagenes/imagenesPNG/Robot1.png"
    maybeRobot2 <- cargarPNG "imagenes/imagenesPNG/Robot2.png"
    maybeRobot3 <- cargarPNG "imagenes/imagenesPNG/Robot3.png"
    maybeRobot4 <- cargarPNG "imagenes/imagenesPNG/Robot4.png"
    maybeProyectil <- cargarPNG "imagenes/imagenesPNG/chicle.png"
    maybeExplosion1 <- cargarPNG "imagenes/imagenesPNG/explosion1.png"
    maybeExplosion2 <- cargarPNG "imagenes/imagenesPNG/explosion2.png"
    maybeExplosion3 <- cargarPNG "imagenes/imagenesPNG/explosion3.png"
    maybeExplosionMuerte <- cargarPNG "imagenes/imagenesPNG/chicleMuerte.png"
    maybeExplosionComida <- cargarPNG "imagenes/imagenesPNG/explosionComida.png"
    maybeExplosionProfesor <- cargarPNG "imagenes/imagenesPNG/explosionProfesor.png"
    maybeEscritorio <- cargarPNG "imagenes/imagenesPNG/escritorio.png"
    maybeSandwich <- cargarPNG "imagenes/imagenesPNG/sandwich.png"
    maybeZumo <- cargarPNG "imagenes/imagenesPNG/zumo.png"
    maybePlatano <- cargarPNG "imagenes/imagenesPNG/platano.png"
    maybeExplosionRobot <- cargarPNG "imagenes/imagenesPNG/explosionRobots.png"


    ------------------------------------------------------------
    -- 3️º Generar posiciones iniciales
    ------------------------------------------------------------
    [pos1, pos2, pos3, pos4,
     posSandwich1, posSandwich2,
     posZumo1, posZumo2,
     posPlatano1, posPlatano2] <- generate (generarPosiciones 10)

------------------------------------------------------------
-- 4️º Crear el mundo inicial con el número de torneos y duración desde config.txt
------------------------------------------------------------
    let numT = numTorneos cfg
        duracionCfg = duracion cfg  -- 👈 LEER DURACIÓN DEL CONFIG
        listaBots = bots cfg  -- 👈 la lista del config
        mundoInicial = (estadoInicial inicio clase victoria derrota imagenCarga
                          maybeRobot1 maybeRobot2 maybeRobot3 maybeRobot4
                          maybeTorreta maybeProfe maybeProfeEnfadado maybeProyectil 
                          maybeExplosion1 maybeExplosion2 maybeExplosion3 maybeExplosionMuerte maybeEscritorio
                          maybeSandwich maybeZumo maybePlatano maybeExplosionComida maybeExplosionProfesor maybeExplosionRobot
                          listaBots  -- 👈 PASAR LA LISTA
                          pos1 pos2 pos3 pos4
                          posSandwich1 posSandwich2
                          posZumo1 posZumo2
                          posPlatano1 posPlatano2)
                          { torneosRestantes = numT
                          , duracionMaxima = duracionCfg  -- 👈 ASIGNAR DURACIÓN
                          }

    ------------------------------------------------------------
    -- 5️º Lanzar torneos consecutivos (automático)
    ------------------------------------------------------------
    lanzarTorneos cfg
    putStrLn "Iniciando juego..."

    ------------------------------------------------------------
    -- 6️º Ejecutar Gloss (el juego)
    ------------------------------------------------------------
    play
      (InWindow "Niños y Chicles" (round ancho, round alto) (100, 100))
      white
      60
      mundoInicial
      dibujar
      manejarEvento
      actualizar