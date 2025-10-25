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

robotBox :: Robot -> ((Float,Float),(Float,Float))
robotBox r = 
  let (x, y) = position (commonR r)
  in ((x-20,y-25),(x+20,y+25))

comportamientoNino :: Float -> World -> Robot -> Robot
comportamientoNino dt world r
  | not (isRobotAlive r) = r
  | shooting r           = r  -- Si está disparando, se detiene
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
      rs3 = map (curarSoporte dt) rs2      -- 🔁 mover esto antes del disparo
      (rs4, nuevosProj) = pasoShooting dt w { robots = rs3 }
  in (rs4, nuevosProj)


procesarRobot :: Float -> World -> Robot -> ([Robot], [Projectile]) -> ([Robot], [Projectile])
procesarRobot dt world r (accR, accP)
  | not (isRobotAlive r) = (r : accR, accP)
  | otherwise =
      let rCD = actualizarCooldown r (nuevoCooldown (turret r) dt)
          enemigosVivos = enemigos rCD world
          quiereDisparar = not (null enemigosVivos) && robotQuiereDisparar world rCD
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

cooldownSoporte :: Robot -> Float
cooldownSoporte r
  | idR r /= 3 = 0
  | otherwise  = 5

curarSoporte :: Float -> Robot -> Robot
curarSoporte dt r
  | idR r /= 3 = r  -- Solo el robot soporte (id 3) se cura
  | not (isRobotAlive r) = r
  | nuevoTiempo >= cdSoporte =
      r { healthR = vidaNueva
        , turret = (turret r) { cooldown = 0 }  -- reinicia cooldown
        }
  | otherwise =
      r { turret = (turret r) { cooldown = nuevoTiempo } }
  where
    cdSoporte    = cooldownSoporte r       -- cooldown específico de soporte
    tiempoActual = cooldown (turret r)
    nuevoTiempo  = tiempoActual + dt
    vidaNueva    = min 110 (healthR r + 2) -- curación por tick


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

pasoShooting :: Float -> World -> ([Robot], [Projectile])
pasoShooting dt world = foldr (procesarRobot dt world) ([], []) (robots world)