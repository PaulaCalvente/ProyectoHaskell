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