module Mecanicas.Robot where

import Utils
import Data.Explosion
import Data.Mundo
import Data.Proyectil
import Data.Robot
import Data.Torreta
import Data.DatosComunes
import Config.Memory
import qualified Data.Map as M

import Mecanicas.Torreta
import Mecanicas.Mundo
import Mecanicas.Proyectil

detectedAgent :: Robot -> Robot -> Bool
detectedAgent r1 r2 =
  let (x1,y1) = positionR r1
      (x2,y2) = positionR r2
      r       = radarRange r1
  in distanceBetween (x1, y1) (x2, y2) <= r

isRobotAlive :: Robot -> Bool
isRobotAlive r = healthR r > 0

countActiveRobots :: [Robot] -> Int
countActiveRobots rs = length [r | r <- rs, isRobotAlive r]

updateRobotVelocity :: Robot -> Velocity -> Robot
updateRobotVelocity robot newVel =
  robot { commonR = (commonR robot) { velocity = newVel } }

robotBox :: Robot -> ((Float,Float),(Float,Float))
robotBox r = 
  let (x, y) = position (commonR r)
  in ((x-20,y-25),(x+20,y+25))

-- Leer valor booleano de memoria
memBool :: String -> Robot -> Maybe Bool
memBool key r = case M.lookup key (memory r) of
  Just (MemBool b) -> Just b
  _ -> Nothing

-- Leer valor entero de memoria
memInt :: String -> Robot -> Maybe Int
memInt key r = case M.lookup key (memory r) of
  Just (MemInt x) -> Just (round x)
  _ -> Nothing

-- Leer valor float de memoria
memFloat :: String -> Robot -> Maybe Float
memFloat key r = case M.lookup key (memory r) of
  Just (MemFloat x) -> Just x
  _ -> Nothing

-- Escribir en memoria
rememberBool :: String -> Bool -> Robot -> Robot
rememberBool key val r = r { memory = M.insert key (MemBool val) (memory r) }

rememberFloat :: String -> Float -> Robot -> Robot
rememberFloat key val r = r { memory = M.insert key (MemFloat val) (memory r) }

rememberInt :: String -> Int -> Robot -> Robot
rememberInt key val r = r { memory = M.insert key (MemInt (fromIntegral val)) (memory r) }

forget :: String -> Robot -> Robot
forget key r = r { memory = M.delete key (memory r) }

-- Lógica 1: Robot 3 – Modo Supervivencia (usa memoria)
aplicarModoSupervivencia :: World -> Robot -> Robot
aplicarModoSupervivencia w r
  | idR r /= 3 = r
  | not (isRobotAlive r) = r
  | healthR r <= 30 =
      rememberBool "modo_supervivencia" True r
  | healthR r >= 50 && not (any (detectedAgent r) enemigosVivos) =
      forget "modo_supervivencia" r
  | otherwise = r
  where
    enemigosVivos = [e | e <- robots w, idR e /= 3, isRobotAlive e]

-- Lógica 2: Robot 4 – Modo Furia (usa memoria)
aplicarModoFuria :: Float -> Robot -> Robot -> Robot
aplicarModoFuria dt rAntes rAhora
  | idR rAhora /= 4 = rAhora
  | not (isRobotAlive rAhora) = rAhora
  | healthR rAhora < healthR rAntes =  -- recibió daño
      let veces = case memInt "veces_furia" rAhora of
                    Just n -> n + 1
                    Nothing -> 1
          r1 = rememberBool "modo_furia" True rAhora
          r2 = rememberInt "veces_furia" veces r1
          r3 = r2 { turret = (turret r2) { cooldown = 0 } }
          r4 = rememberFloat "tiempo_furia" 0 r3
      in r4
  | otherwise =
      case memBool "modo_furia" rAhora of
        Just True ->
          let tiempoAnterior = fromMaybe 0 (memFloat "tiempo_furia" rAhora)
              nuevoTiempo = tiempoAnterior + dt
              r1 = rememberFloat "tiempo_furia" nuevoTiempo rAhora
          in if nuevoTiempo >= 5.0
                then forget "modo_furia" (forget "tiempo_furia" r1)
                else r1
        _ -> rAhora

-- Resto del código
comportamientoNino :: Float -> World -> Robot -> Robot
comportamientoNino dt world r
  | not (isRobotAlive r) = r
  | null pts             = r
  | haLlegado pos objetivo = avanzarPatrulla r
  | otherwise            = moverHaciaObjetivo dt r
  where
    pts = points (commonR r)
    pos = position (commonR r)
    objetivo = head pts

haLlegado :: (Float, Float) -> (Float, Float) -> Bool
haLlegado pos objetivo = distanceBetween pos objetivo < 20

avanzarPatrulla :: Robot -> Robot
avanzarPatrulla r =
  let pts = points (commonR r)
      nuevosPts = tail pts ++ [head pts]
  in r { commonR = (commonR r) { points = nuevosPts } }

moverHaciaObjetivo :: Float -> Robot -> Robot
moverHaciaObjetivo dt r =
  let pos = position (commonR r)
      vel = velocidadPorRol (idR r)
      objetivo = head (points (commonR r))
      ang = angleToTarget pos objetivo
      dx = cos ang * vel * dt
      dy = sin ang * vel * dt
      nuevaPos = clampDentro (fst pos + dx, snd pos + dy)
  in r { commonR = (commonR r) { position = nuevaPos } }

robotQuiereDisparar :: World -> Robot -> Bool
robotQuiereDisparar world me =
  any (detectedAgent me) [r | r <- robots world, idR r /= idR me, isRobotAlive r]

clampDentro :: (Float,Float) -> (Float,Float)
clampDentro (x,y) =
  let halfW = ancho/2
      halfH = alto/2
      x' = max (-halfW + 20) (min (halfW - 20) x)
      y' = max (-halfH + 25) (min (halfH - 25) y)
  in (x', y')

actualizarRobots :: Float -> World -> ([Robot], [Projectile])
actualizarRobots dt w =
  let rs0 = robots w
      rs1 = map (comportamientoNino dt w) rs0
      rs2 = map (apuntarTorreta w) rs1
      estadoAnterior = zip rs0 rs2
      rs3 = map (\(rAntes, rAhora) ->
                let rConSupervivencia = aplicarModoSupervivencia w rAhora
                    rConFuria = aplicarModoFuria dt rAntes rConSupervivencia
                in rConFuria
               ) estadoAnterior
      (rs4, nuevosProj) = pasoShootingConLogicas dt w { robots = rs3 }
  in (rs4, nuevosProj)

pasoShootingConLogicas :: Float -> World -> ([Robot], [Projectile])
pasoShootingConLogicas dt world = foldr (procesarRobotConLogicas dt world) ([], []) (robots world)

procesarRobotConLogicas :: Float -> World -> Robot -> ([Robot], [Projectile]) -> ([Robot], [Projectile])
procesarRobotConLogicas dt world r (accR, accP)
  | not (isRobotAlive r) = (r : accR, accP)
  | otherwise =
      let rCD = actualizarCooldown r (nuevoCooldown (turret r) dt)
          enemigosVivos = enemigos rCD world
          -- Robot 3 en modo supervivencia NO dispara
          enModoSupervivencia = memBool "modo_supervivencia" rCD == Just True
          quiereDisparar = 
            if idR rCD == 3 && enModoSupervivencia
               then False
               else not (null enemigosVivos) && robotQuiereDisparar world rCD
          puedeDisparar = cooldown (turret rCD) <= 0 && quiereDisparar
      in if puedeDisparar
           then let objetivo = enemigoMasCercano rCD enemigosVivos
                    (p, r') = generarDisparo dt rCD objetivo
                in (r' : accR, p : accP)
           else (rCD { shooting = quiereDisparar } : accR, accP)

enemigoMasCercano :: Robot -> [Robot] -> Robot
enemigoMasCercano me (r:rs) = fst $ foldl buscar (r, distanceBetween (position (commonR me)) (position (commonR r))) rs
  where
    buscar (minR, minD) r' =
      let d = distanceBetween (position (commonR me)) (position (commonR r'))
      in if d < minD then (r', d) else (minR, minD)

curarSoporte :: Float -> Robot -> Robot
curarSoporte dt r
  | idR r /= 3 = r
  | not (isRobotAlive r) = r
  | memBool "modo_supervivencia" r == Just True = r  -- NO cura en modo supervivencia
  | shooting r = r
  | nuevoTiempo >= 5.0 =
      r { healthR = min 110 (healthR r + 2)
        , turret = (turret r) { cooldown = 0 } }
  | otherwise =
      r { turret = (turret r) { cooldown = nuevoTiempo } }
  where
    tiempoActual = cooldown (turret r)
    nuevoTiempo  = tiempoActual + dt

enemigos :: Robot -> World -> [Robot]
enemigos r w = [ r' | r' <- robots w, idR r' /= idR r, isRobotAlive r' ]

robotsVivos :: [Robot] -> [Robot]
robotsVivos = filter isRobotAlive

filtrarEnemigos :: World -> Robot -> [Robot]
filtrarEnemigos world r =
  [e | e <- robots world, idR e /= idR r, isRobotAlive e]

detectarEnemigos :: Robot -> [Robot] -> [Robot]
detectarEnemigos r = filter (detectedAgent r)

apuntarTorreta :: World -> Robot -> Robot
apuntarTorreta world r
  | estaMuerto = r
  | null detectados = r
  | otherwise = actualizarTorreta r objetivo
  where
    estaMuerto = healthR r <= 0
    enemigosVivos = filtrarEnemigos world r
    detectados = detectarEnemigos r enemigosVivos
    objetivo = head detectados