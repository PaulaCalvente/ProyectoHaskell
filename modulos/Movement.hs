module Movement where

import Utils
import Types

-- Detectar si un robot detecta a otro
detectedAgent :: Robot -> Robot -> Bool
detectedAgent Robot{ positionR = (x1,y1), radarRange = r }
              Robot{ positionR = (x2,y2) } =
  distanceBetween (x1, y1) (x2, y2) <= r

-- Saber si un robot está vivo
isRobotAlive :: Robot -> Bool
isRobotAlive Robot{ healthR = h } = h > 0

-- Contar robots vivos
countActiveRobots :: [Robot] -> Int
countActiveRobots rs = length [r | r <- rs, isRobotAlive r]

-- Actualizar velocidad de un robot
updateRobotVelocity :: Robot -> Velocity -> Robot
updateRobotVelocity robot newVel = robot { velocityR = newVel }

-- Velocidad basada en acción
updateVelocity :: Action -> Velocity
updateVelocity action =
  case action of
    MoveUp    -> (0, baseSpeed)
    MoveDown  -> (0, -baseSpeed)
    MoveLeft  -> (-baseSpeed, 0)
    MoveRight -> (baseSpeed, 0)
    Stop      -> (0, 0)

-- Actualizar posición en función de dt y velocidad
updatePosition :: Float -> Position -> Velocity -> Position
updatePosition dt (px, py) (vx, vy) = (px + vx * dt, py + vy * dt)

-- Multiplicación de puntos
mul :: Point -> Point -> Point
mul (w, h) (sw, sh) = (w * sw, h * sh)
