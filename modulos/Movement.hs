module Movement where

import Utils
import Data.Explosion
import Data.Mundo
import Data.Proyectil
import Data.Robot
import Data.Torreta
import Data.DatosComunes

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

{--
updateVelocity :: Action -> Velocity
updateVelocity action =
  case action of
    MoveUp    -> (0, baseSpeed)
    MoveDown  -> (0, -baseSpeed)
    MoveLeft  -> (-baseSpeed, 0)
    MoveRight -> (baseSpeed, 0)
    Stop      -> (0, 0)
--}

updatePosition :: Float -> Position -> Velocity -> Position
updatePosition dt (px, py) (vx, vy) =
  (px + vx * dt, py + vy * dt)

mul :: Point -> Point -> Point
mul (w, h) (sw, sh) = (w * sw, h * sh)


