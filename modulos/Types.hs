module Types where 

import Utils

type Id = Int
type Health = Float
type Velocity = (Float, Float)
type Damage = Float
type HaveExploded = Bool
type Shoot = Float
type TurretAction = Float
type Duration = Float

data CommonData a = CommonData
  { id       :: Id
  , damage   :: Damage
  , position :: a
  , velocity :: a
  , size     :: Size
  , points   :: [a]
  } deriving (Show, Eq)

instance Functor CommonData where
  fmap f (CommonData i d p v s pts) =
    CommonData i d (f p) (f v) s (fmap f pts)

instance Applicative CommonData where
  pure x = CommonData 0 0 x x (0, 0) [x]
  (CommonData i d fp fv s fpts) <*> (CommonData _ _ p v _ pts) =
    CommonData i d (fp p) (fv v) s (zipWith ($) fpts pts)

data Projectile = Projectile
  { idP        :: Id
  , commonP    :: CommonData Float
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
  , commonR      :: CommonData Float
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
  , robotHits   :: [RobotHit]
  } deriving (Show, Eq)

data Explosion = Explosion
  { positionE :: Position
  , sizeE     :: Size
  , durationE :: Duration
  } deriving (Show, Eq)

baseSpeed :: Float
baseSpeed = 5.0
