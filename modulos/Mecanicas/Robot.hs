module Mecanicas.Robot where

import Utils
import Data.Explosion
import Data.Mundo
import Data.Proyectil
import Data.Robot
import Data.Torreta
import Data.DatosComunes

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

ninoBox :: Robot -> ((Float,Float),(Float,Float))
ninoBox r = 
  let (x, y) = position (commonR r)
  in ((x-20,y-25),(x+20,y+25))

comportamientoNino :: Float -> Robot -> Robot
comportamientoNino dt r
  | not (isRobotAlive r) = r
  | null pts     = r
  | haLlegado pos objetivo = avanzarPatrulla r
  | otherwise    = moverHaciaObjetivo dt r
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
      rs1 = map (comportamientoNino dt) rs0
      rs2 = map (apuntarTorreta w) rs1
      (rs3, nuevosProj) = pasoShooting dt w { robots = rs2 }
      rs4 = map (curarSoporte dt) rs3
  in (rs4, nuevosProj)

procesarRobot :: Float -> World -> Robot -> ([Robot], [Projectile]) -> ([Robot], [Projectile])
procesarRobot dt world r (accR, accP)
  | not (isRobotAlive r)      = (r : accR, accP)
  | not puedeDisparar = (rCD : accR, accP)
  | otherwise         = (r' : accR, p : accP)
  where
    rCD = actualizarCooldown r (nuevoCooldown (turret r) dt)
    enemigosVivos = enemigos rCD world
    puedeDisparar = cooldown (turret rCD) <= 0
                 && not (null enemigosVivos)
                 && robotQuiereDisparar world rCD
    objetivo = enemigoMasCercano rCD enemigosVivos
    (p, r') = generarDisparo dt rCD objetivo

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
  | nuevoTiempo >= 10 =
      r { healthR = vidaNueva
        , turret = (turret r) { cooldown = 0 }
        }
  | otherwise =
      r { turret = (turret r) { cooldown = nuevoTiempo }
        }
  where
    tiempoActual = cooldown (turret r)
    nuevoTiempo  = tiempoActual + dt
    vidaNueva    = min 110 (healthR r + 2)

enemigos :: Robot -> World -> [Robot]
enemigos r w = [ r' | r' <- robots w, idR r' /= idR r, isRobotAlive r' ]

robotsVivos :: [Robot] -> [Robot]
robotsVivos = filter ((> 0) . healthR)

filtrarEnemigos :: World -> Robot -> [Robot]
filtrarEnemigos world r =
  [e | e <- robots world, idR e /= idR r, isRobotAlive e]

detectarEnemigos :: Robot -> [Robot] -> [Robot]
detectarEnemigos r = filter (detectedAgent r)

apuntarTorreta :: World -> Robot -> Robot
apuntarTorreta world r
  | est8Muerto = r
  | null detectados = r
  | otherwise = actualizarTorreta r objetivo
  where
    est8Muerto = healthR r <= 0
    enemigosVivos = filtrarEnemigos world r
    detectados = detectarEnemigos r enemigosVivos
    objetivo = head detectados

pasoShooting :: Float -> World -> ([Robot], [Projectile])
pasoShooting dt world = foldr (procesarRobot dt world) ([], []) (robots world)