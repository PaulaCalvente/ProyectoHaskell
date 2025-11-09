module Estadisticas (escribirEstadisticas) where

import Data.Mundo (ResultadoTorneo(..))
import System.IO

escribirEstadisticas :: [ResultadoTorneo] -> IO ()
escribirEstadisticas resultados = withFile "estadisticas.txt" WriteMode $ \h -> do
  -- Por torneo
  mapM_ (escribirTorneo h) (zip [1..] resultados)

  -- Agregado: estadísticas agregadas
  hPutStrLn h "\n=== ESTADÍSTICAS AGREGADAS (TODOS LOS TORNEOS) ===\n"

  let todosImpactos = concatMap (map snd . numImpactosPorBot) resultados
      todosPorcentajes = concatMap (map snd . porcentajeVidaPorBot) resultados

  -- Medias básicas (ya existían)
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

  -- Máximos (ya existían)
  let maxImpactos = if null todosImpactos then 0 else maximum todosImpactos
      maxVida = if null todosPorcentajes then 0 else maximum todosPorcentajes

  hPutStrLn h $ "Máximo impactos en un torneo (por un bot): " ++ show maxImpactos
  hPutStrLn h $ "Máximo porcentaje de vida: " ++ show (round maxVida) ++ "%"

  -- 👇 NUEVO: Promedio de obstáculos recogidos por torneo
  let todosObstaculos = concatMap (map snd . obstaculosPorBot) resultados
      mediaObstaculos = 
        if null todosObstaculos
           then 0
           else sum (map fromIntegral todosObstaculos) / fromIntegral (length todosObstaculos)

  hPutStrLn h $ "Media de obstáculos recogidos por bot por torneo: " ++ show (round mediaObstaculos)

  -- También: total promedio por torneo
  let obstaculosPorTorneo = map (sum . map snd . obstaculosPorBot) resultados
      mediaTotalObstaculos = 
        if null obstaculosPorTorneo
           then 0
           else sum (map fromIntegral obstaculosPorTorneo) / fromIntegral (length obstaculosPorTorneo)
  hPutStrLn h $ "Media total de obstáculos recogidos por torneo: " ++ show (round mediaTotalObstaculos)

  hPutStrLn h "\nFin del archivo."

escribirTorneo :: Handle -> (Int, ResultadoTorneo) -> IO ()
escribirTorneo h (n, res) = do
  hPutStrLn h $ "\n=== TORNEO " ++ show n ++ " ==="
  
  hPutStrLn h "Impactos por bot (proyectiles que impactaron):"
  mapM_ (\(id, hits) -> hPutStrLn h $ "  Bot " ++ show id ++ ": " ++ show hits) (numImpactosPorBot res)
  
  hPutStrLn h "Daño infligido por bot:"
  mapM_ (\(id, dmg) -> hPutStrLn h $ "  Bot " ++ show id ++ ": " ++ show (round dmg)) (danoInfligidoPorBot res)
  
  hPutStrLn h "Daño recibido por bot:"
  mapM_ (\(id, dmg) -> hPutStrLn h $ "  Bot " ++ show id ++ ": " ++ show (round dmg)) (danoRecibidoPorBot res)
  
  hPutStrLn h "Colisiones robot-robot por bot:"
  mapM_ (\(id, col) -> hPutStrLn h $ "  Bot " ++ show id ++ ": " ++ show col) (colisionesPorBot res)
  
  hPutStrLn h "Obstáculos recogidos por bot:"
  mapM_ (\(id, obs) -> hPutStrLn h $ "  Bot " ++ show id ++ ": " ++ show obs) (obstaculosPorBot res)
  
  hPutStrLn h "Porcentaje de tiempo con vida:"
  mapM_ (\(id, pct) -> hPutStrLn h $ "  Bot " ++ show id ++ ": " ++ show (round pct) ++ "%") (porcentajeVidaPorBot res)
  
  hPutStrLn h $ "Ganador: " ++ case ganadorTorneo res of
    Just i  -> "Bot " ++ show i
    Nothing -> "Ninguno"