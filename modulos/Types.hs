module Types where

import Utils

-- Tipos básicos del juego
type Id = Int
type Health = Float
type Velocity = (Float, Float)
type Damage = Float
type HaveExploded = Bool
type Shoot = Float
type TurretAction = Float
type Duration = Float

-- Tipo base con datos comunes a objetos con forma y movimiento
data CommonData = CommonData
  { position :: Position
  , velocity :: Velocity
  , size     :: Size
  , points   :: [Point]
  } deriving (Show, Eq)

-- Objetos del mundo
data Projectile = Projectile
  { idP        :: Id
  , common     :: CommonData   -- ← contiene position, velocity, size, points
  , damageP    :: Damage
  , rangeP     :: Distance
  } deriving (Show, Eq)

data Turret = Turret
  { idT          :: Id
  , vectorT      :: Vector
  , angleT       :: Angle
  , projectileT  :: Projectile
  , turretAction :: TurretAction
  , shoot        :: Shoot
  } deriving (Show, Eq)

data Action = MoveUp | MoveDown | MoveLeft | MoveRight | Stop
  deriving (Show, Eq)

data Robot = Robot
  { idR          :: Id
  , common       :: CommonData   -- ← contiene position, velocity, size, points
  , healthR      :: Health
  , radarRange   :: Distance
  , turret       :: Turret
  , haveExploded :: HaveExploded
  , damageR      :: Damage
  } deriving (Show, Eq)

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

data World = World
  { robots      :: [Robot]
  , projectiles :: [Projectile]
  , turrets     :: [Turret]
  , robotHits   :: [RobotHit]  -- eventos de colisión detectados
  } deriving (Show, Eq)

data Explosion = Explosion
  { positionE :: Position
  , sizeE     :: Size
  , durationE :: Duration
  } deriving (Show, Eq)

-- Constantes
baseSpeed :: Float
baseSpeed = 5.0