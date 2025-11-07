module Torneos where

import System.IO
import Data.List
import Data.Char (isSpace)

------------------------------------------------------------
-- Estructura de configuración
------------------------------------------------------------
data ConfigTorneo = ConfigTorneo
  { bots       :: [String]
  , area       :: (Int, Int)
  , duracion   :: Float
  , numTorneos :: Int
  } deriving (Show)

------------------------------------------------------------
-- Función para leer y parsear el archivo config.txt
------------------------------------------------------------
leerConfig :: FilePath -> IO ConfigTorneo
leerConfig ruta = do
  contenido <- readFile ruta
  let lineas = lines contenido
      lookupClave clave =
        case find (isPrefixOf clave) lineas of
          Just l  -> dropWhile isSpace $ drop (length clave + 1) l
          Nothing -> error $ "No se encontró la clave " ++ clave

      botsStr   = lookupClave "bots"
      areaStr   = lookupClave "area"
      duracionS = lookupClave "duracion"
      torneosS  = lookupClave "torneos"

      listaBots = map (dropWhile isSpace) $ splitBy ',' botsStr
      areaTuple = case splitBy 'x' areaStr of
                    [w,h] -> (read w, read h)
                    _     -> error "Formato del área inválido, usa ancho x alto"
      duracionF = read duracionS :: Float
      torneosN  = read torneosS  :: Int

  return ConfigTorneo
    { bots = listaBots
    , area = areaTuple
    , duracion = duracionF
    , numTorneos = torneosN
    }

------------------------------------------------------------
-- Función auxiliar para dividir cadenas por un delimitador
------------------------------------------------------------
splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy c s =
  let (x, rest) = break (== c) s
  in x : case rest of
           [] -> []
           (_:xs) -> splitBy c xs

------------------------------------------------------------
-- Ejemplo: iniciar varios torneos seguidos
------------------------------------------------------------
lanzarTorneos :: ConfigTorneo -> IO ()
lanzarTorneos cfg = do
  putStrLn $ "🔧 Configuración del torneo:"
  putStrLn $ "  Bots: " ++ show (bots cfg)
  putStrLn $ "  Área: " ++ show (area cfg)
  putStrLn $ "  Duración máxima: " ++ show (duracion cfg) ++ " segundos"
  putStrLn $ "  Número de torneos: " ++ show (numTorneos cfg)
  putStrLn "---------------------------------------------"
  ejecutarSecuencia (numTorneos cfg) 1

------------------------------------------------------------
-- Simulación o lanzamiento real
-- (sustituye esta parte por tu ejecución real del juego)
------------------------------------------------------------
ejecutarSecuencia :: Int -> Int -> IO ()
ejecutarSecuencia total actual
  | actual > total = putStrLn "✅ Todos los torneos han terminado."
  | otherwise = do
      putStrLn $ "🏁 Iniciando torneo " ++ show actual ++ " de " ++ show total
      -- Aquí podrías llamar a tu main del juego real:
      -- main
      putStrLn "🎮 Ejecutando torneo..."
      putStrLn "💥 Finalizado."
      ejecutarSecuencia total (actual + 1)
