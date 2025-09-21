-- DECLARACION DE TIPOS INICIALES
type Point = (Float, Float) 
type Vector = (Int, Int)
type Angle = Float
type Distance = Float
type Position = (Float, Float)
type Size = (Float, Float)

-- 1. Analiza el funcionamiento del juego y piensa en los tipos que son necesarios. Realiza una lista durante el análisis visual y posteriormente implementa dichos TADs.

type Id = Int -- Ids de los elementos
type Health = Float -- Salud del robot
type Velocity = (Float, Float) -- Velocidad del robot y/o proyectil en x e y
type Damage = Float -- Daño que realiza el proyectil 
-- type HaveExploded = Bool 
type Shoot = Float -- Consideramos esto como el cooldown en segundos
type TurretAction = Float -- Consideramos que es el angulo, en grados, en el que gira la torreta para apuntar
type Duration = Float

-- "Objetos" del mundo
data Projectile = Projectile { idP :: Id
                         , positionP :: Position
                         , velocityP :: Velocity
                         , damageP :: Damage
                         , rangeP :: Distance
                         } deriving (Show, Eq)

data Turret = Turret { idT :: Id -- Que coincide con su robot asociado 
                   , vectorT :: Vector
                   , angleT :: Angle
                   , projectileT :: Projectile
                   , turretAction :: TurretAction
                    , shoot :: Shoot
                   } deriving (Show, Eq)

data Action = MoveUp | MoveDown | MoveLeft | MoveRight | Stop deriving (Show, Eq)

data Robot = Robot { idR :: Id
                   , positionR :: Position
                   , velocityR :: Velocity
                   , healthR :: Health
                   , radarRange :: Distance
                   , sizeR :: Size -- Podría ponerse en World ya que en principio todos tienen el mismo tamaño
                   , turret :: Turret
                   } deriving (Show, Eq)


data World = World { robots :: [Robot]
                   , projectiles :: [Projectile]
                    , turrets :: [Turret]
                    } deriving (Show, Eq)

data Explosion = Explosion { positionE :: Position
                           , sizeE :: Size
                           , durationE :: Duration
                           } deriving (Show, Eq)

-- Lo usamos como velocidad base para cualquier robot, a la hora de hacer un cambio de dirección (updateVelocity)             
baseSpeed :: Float
baseSpeed = 5.0  


-- 2. Refactoriza las funciones implementadas hasta ahora para usar pattern matching, listas por comprensión y cláusulas where, if-then-else
  -- , guardas o case-of cuando proceda.

distanceBetween :: Position -> Position -> Distance
distanceBetween (x1, y1) (x2, y2) = sqrt (dx^2 + dy^2)
  where
    dx = x2 - x1
    dy = y2 - y1

angleToTarget :: Position -> Position -> Angle
angleToTarget (x1, y1) (x2, y2) = rad2deg ang
  where
    ang = atan2 (y2 - y1) (x2 - x1)

deg2rad :: Angle -> Angle
deg2rad a = a * pi / 180

rad2deg :: Angle -> Angle
rad2deg a = a * 180 / pi

-- subVec' :: Vector -> Vector -> Vector
-- subVec r1 r2 = (x1- x2, y1-y2)
  -- where
    -- r1 = (x1, y1)
    -- r2 = (x2, y2)

subVec :: Vector -> Vector -> Vector
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

getVertices :: (Point, Point, Point, Point, Angle) -> [Point]
getVertices (p1, p2, p3, p4, a) = 
  [ (x * cos rad - y * sin rad, x * sin rad + y * cos rad) 
  | (x, y) <- [p1, p2, p3, p4] ]
  where
    rad = deg2rad a

dot :: Point -> Point -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

sub :: Point -> Point -> Point
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

perp :: Vector -> Vector
perp (x, y) = (-y, x)

isInBounds :: Point -> Size -> Bool
isInBounds (x, y) (w, h) =
  case (w >= 0, h >= 0) of
    (True, True)   -> (x >= 0 && x <= w) && (y >= 0 && y <= h)
    (True, False)  -> (x >= 0 && x <= w) && (y <= 0 && y >= h)
    (False, True)  -> (x <= 0 && x >= w) && (y >= 0 && y <= h)
    (False, False) -> (x <= 0 && x >= w) && (y <= 0 && y >= h)

-- 3. Implementa las siguientes funciones usando pattern matching con los TADs definidos anteriormente:

--   - detectedAgent: Determinar si un agente ha detectado a otro en caso de encontrarse dentro del rango de su radar~
detectedAgent :: Robot -> Robot -> Bool
detectedAgent Robot{ positionR = (x1,y1), radarRange = r }
              Robot{ positionR = (x2,y2) } 
  | dist <= r = True
  | otherwise  = False
  where
    dist = distanceBetween (x1, y1) (x2, y2)

--  - isRobotAlive: True si la energía del robot es mayor a 0
isRobotAlive :: Robot -> Bool
isRobotAlive Robot{ healthR = h}
 | h > 0 = True
 | otherwise = False

--  - countActiveRobots: Contar los robots que están vivos

countActiveRobots :: [Robot] -> Int
countActiveRobots ss = length [s | s <- ss, isRobotAlive s]

--  - updateRobotVelocity: Actualiza la velocidad de un robot con una velocidad dada

updateRobotVelocity :: Robot -> Velocity -> Robot
updateRobotVelocity robot newVel = robot { velocityR = newVel } 

--  - updateVelocity: Actualizar velocidad basada en la acción de movimiento

updateVelocity :: Action -> Velocity
updateVelocity action =
  case action of
    MoveUp    -> (0, baseSpeed)
    MoveDown  -> (0, -baseSpeed)
    MoveLeft  -> (-baseSpeed, 0)
    MoveRight -> (baseSpeed, 0)
    Stop      -> (0, 0)

--  - updatePosition: Actualizar una posición en función de la velocidad y el incremento de tiempo
updatePosition :: Float -> Position -> Velocity -> Position
updatePosition dt (px, py) (vx, vy) = (px + vx * dt, py + vy * dt)

--  - mul: tal que (w,h) `mul` (sw,sh) = (wsw, hsh)
mul :: Point -> Point -> Point
mul (w, h) (sw, sh) = (w * sw, h * sh)

-- Test de las funciones implementadas