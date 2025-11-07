module Estadisticas (escribirEstadisticas) where

import Data.Mundo (ResultadoTorneo(..))
import System.IO

escribirEstadisticas :: [ResultadoTorneo] -> IO ()
escribirEstadisticas resultados = withFile "estadisticas.txt" WriteMode $ \h -> do
  -- Por torneo
  mapM_ (escribirTorneo h) (zip [1..] resultados)

  -- Agregado: medias y máximos
  hPutStrLn h "\n=== ESTADÍSTICAS AGREGADAS (TODOS LOS TORNEOS) ===\n"

  let todosImpactos = concatMap (map snd . numImpactosPorBot) resultados
      todosPorcentajes = concatMap (map snd . porcentajeVidaPorBot) resultados

  -- Medias
  let mediaImpactos = 
        if null todosImpactos 
           then 0 
           else fromIntegral (sum todosImpactos) / fromIntegral (length todosImpactos)
      mediaVida = 
        if null todosPorcentajes 
           then 0 
           else sum todosPorcentajes / fromIntegral (length todosPorcentajes)

  hPutStrLn h $ "Media de impactos por bot por torneo: " ++ show (round mediaImpactos)
  hPutStrLn h $ "Media de porcentaje de vida: " ++ show (round mediaVida) ++ "%"

  -- Máximos
  let maxImpactos = if null todosImpactos then 0 else maximum todosImpactos
      maxVida = if null todosPorcentajes then 0 else maximum todosPorcentajes

  hPutStrLn h $ "Máximo impactos en un torneo (por un bot): " ++ show maxImpactos
  hPutStrLn h $ "Máximo porcentaje de vida: " ++ show (round maxVida) ++ "%"

  hPutStrLn h "\nFin del archivo."

escribirTorneo :: Handle -> (Int, ResultadoTorneo) -> IO ()
escribirTorneo h (n, res) = do
  hPutStrLn h $ "\n=== TORNEO " ++ show n ++ " ==="
  hPutStrLn h "Impactos por bot (proyectiles que impactaron):"
  mapM_ (\(id, hits) -> hPutStrLn h $ "  Bot " ++ show id ++ ": " ++ show hits) (numImpactosPorBot res)
  hPutStrLn h "Porcentaje de tiempo con vida:"
  mapM_ (\(id, pct) -> hPutStrLn h $ "  Bot " ++ show id ++ ": " ++ show (round pct) ++ "%") (porcentajeVidaPorBot res)
  hPutStrLn h $ "Ganador: " ++ case ganadorTorneo res of
    Just i  -> "Bot " ++ show i
    Nothing -> "Ninguno (todos eliminados)"