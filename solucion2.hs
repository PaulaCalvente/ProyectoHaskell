-- DECLARACION DE TIPOS INICIALES
type Point = (Float, Float) 
type Vector = (Int, Int)
type Angle = Float
type Distance = Float
type Position = (Float, Float)
type Size = (Float, Float)


-- 1. Analiza el funcionamiento del juego y piensa en los tipos que son necesarios. Realiza una lista durante el análisis visual y posteriormente implementa dichos TADs.

type Health = Float -- Salud del robot
type IsAlive = Bool -- Indica si el robot está vivo
type HaveDetected = Bool -- Indica si un robot ha detectado a otro
type NRobots = Int -- Número de robots en juego en el mundo
type NProjectiles = Int -- Número de proyectiles en juego en el mundo
type NExplosions = Int -- Número de explosiones en juego en el mundo
type Velocity = (Float, Float) -- Velocidad del robot y/o proyectil en x e y

-- EXTRAS
type Damage = Float -- Daño que realiza el proyectil 

-- "Objetos" del mundo
data Proyectile = Projectile { positionP :: Position
                         , velocityP :: Velocity
                         , damageP :: Damage
                         , rangeP :: Distance
                         } deriving (Show, Eq)

data Turret = Turret { vectorT :: Vector
                   , angleT :: Angle
                   , proyectileT :: Proyectile
                   } deriving (Show, Eq)

data Action = MoveUp | MoveDown | MoveLeft | MoveRight | Stop deriving (Show, Eq)

data Robot = Robot { positionR :: Position
                   , velocityR :: Velocity
                   , health :: Health
                   , radarRange :: Distance
                   , sizeR :: Size
                   , turret :: Turret
                   } deriving (Show, Eq)

data World = World { robots :: [Robot]
                   , projectiles :: [Proyectile]
                    , turrets :: [Turret]
                    } deriving (Show, Eq)
                  
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

detectedAgent :: Position -> Position -> Distance -> HaveDetected
detectedAgent p1 p2 r = distanceBetween p1 p2 <= r

--  - isRobotAlive: True si la energía del robot es mayor a 0

isRobotAlive :: Health -> IsAlive
isRobotAlive s = s > 0

--  - countActiveRobots: Contar los robots que están vivos

countActiveRobots :: [Health] -> NRobots
countActiveRobots ss = length [s | s <- ss, isRobotAlive s]

--  - updateRobotVelocity: Actualiza la velocidad de un robot con una velocidad dada

updateRobotVelocity :: Robot -> Velocity -> Robot
updateRobotVelocity robot newVel = robot { velocityR = newVel }

--  - updateVelocity: Actualizar velocidad basada en la acción de movimiento

updateVelocity :: Velocity -> Action -> Velocity
updateVelocity _ action =
  case action of
    MoveUp    -> (0, baseSpeed)
    MoveDown  -> (0, -baseSpeed)
    MoveLeft  -> (-baseSpeed, 0)
    MoveRight -> (baseSpeed, 0)
    Stop      -> (0, 0)

--  - updatePosition: Actualizar una posición en función de la velocidad y el incremento de tiempo

updatePosition :: Position -> Vector -> Float -> Position
updatePosition (px, py) (vx, vy) dt = (px + vxf * dt, py + vyf * dt)
  where
    vxf = fromIntegral vx
    vyf = fromIntegral vy

--  - mul: tal que (w,h) `mul` (sw,sh) = (wsw, hsh)
mul :: Point -> Point -> Point
mul (w, h) (sw, sh) = (w * sw, h * sh)


-- Tests

robot1 = Robot (10, 20) (0, 0) 100 50 (10, 10) turret1 
robot2 = Robot (30, 40) (0, 0) 0 50 (10, 10) turret2
robot3 = Robot (15, 25) (0, 0) 50 50 (10, 10) turret3

turret1 = Turret (1, 0) 0 proyectile1
turret2 = Turret (0, 1) 90 proyectile1
turret3 = Turret (1, 1) 45 proyectile1

proyectile1 = Projectile (5, 5) (1, 1) 10 100

world = World [robot1, robot2, robot3] [proyectile1] [turret1, turret2, turret3]