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

-- | Información común de cualquier entidad con posición, velocidad y forma.
-- Parametrizado en 'a' para poder aplicar Functor y Applicative.
data CommonData a = CommonData
  { id       :: Id
  , damage   :: Damage
  , position :: (a, a)   -- posición como coordenadas (x, y)
  , velocity :: (a, a)   -- velocidad también como vector (vx, vy)
  , size     :: Size
  , points   :: [(a, a)] -- lista de puntos
  } deriving (Show, Eq)

-- | FUNCTOR: aplica una función a todos los campos numéricos.
instance Functor CommonData where
  fmap f (CommonData i d (px, py) (vx, vy) s pts) =
    CommonData i d (f px, f py) (f vx, f vy) s (map (\(x, y) -> (f x, f y)) pts)

instance Applicative CommonData where
  pure x = CommonData 0 0 (x, x) (x, x) (0, 0) [(x, x)]
  (CommonData _ _ (fx, fy) (fvx, fvy) _ _) <*> (CommonData i d (px, py) (vx, vy) s pts) =
    CommonData i d (fx px, fy py) (fvx vx, fvy vy) s pts


-- | Datos del proyectil
data Projectile = Projectile
  { idP        :: Id
  , commonP    :: CommonData Float
  , damageP    :: Damage
  , rangeP     :: Distance
  } deriving (Show, Eq)

-- | Datos de la torreta
data Turret = Turret
  { idT          :: Id
  , vectorT      :: Vector
  , angleT       :: Angle
  , projectileT  :: Projectile
  , turretAction :: TurretAction
  , shoot        :: Shoot
  } deriving (Show, Eq)

-- | Acciones posibles de un robot
data Action
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | Stop
  deriving (Show, Eq)

-- | Datos del robot
data Robot = Robot
  { idR          :: Id
  , commonR      :: CommonData Float
  , healthR      :: Health
  , radarRange   :: Distance
  , turret       :: Turret
  , haveExploded :: HaveExploded
  , damageR      :: Damage
  } deriving (Show, Eq)

-- | Registro de impactos o colisiones
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

-- | Estado global del mundo
data World = World
  { robots      :: [Robot]
  , projectiles :: [Projectile]
  , turrets     :: [Turret]
  , robotHits   :: [RobotHit]
  } deriving (Show, Eq)

-- | Información de una explosión
data Explosion = Explosion
  { positionE :: Position
  , sizeE     :: Size
  , durationE :: Duration
  } deriving (Show, Eq)

-- | Velocidad base por defecto
baseSpeed :: Float
baseSpeed = 5.0

