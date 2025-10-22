module Data.Robot where
import Data.DatosComunes
import Data.Torreta

data Action
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | Stop
  deriving (Show, Eq)

-- Datos del robot

data Robot = Robot
  { idR          :: Id
  , commonR      :: CommonData Float
  , healthR      :: Health
  , maxHealthR   :: Health
  , radarRange   :: Distance
  , turret       :: Turret
  , haveExploded :: HaveExploded
  } deriving (Show, Eq)

-- Registro de impactos o colisiones
data RobotHit
  = RobotHitByProjectile
      { idRobot      :: Id
      , idProjectile :: Id
      , damageHit    :: Damage
      , hitAt        :: Position
      }
  | RobotCollidedWithRobot
      { idRobot1    :: Id
      , idRobot2    :: Id
      , damageHit1  :: Damage
      , damageHit2  :: Damage
      , hitAt       :: Position
      }
  deriving (Show, Eq)

velNino :: Float
velNino = 150

-- Extracción de tipos comunes del robot
damageR :: Robot -> Damage
damageR = damage . commonR

positionR :: Robot -> Position
positionR = position . commonR

velocityR :: Robot -> Velocity
velocityR = velocity . commonR

sizeR :: Robot -> Size
sizeR = size . commonR

pointsR :: Robot -> [Point]
pointsR = points . commonR

velocidadPorRol :: Id -> Float
velocidadPorRol 1 = 75
velocidadPorRol 2 = 10
velocidadPorRol 3 = 25
velocidadPorRol 4 = 30
velocidadPorRol _ = 25