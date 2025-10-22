module Types where
import Graphics.Gloss hiding (Vector, Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector, Point)

type Point = (Float, Float)
type Vector = (Float, Float)
type Angle = Float
type Distance = Float
type Position = (Float, Float)
type Size = (Float, Float)

type Id = Int
type Health = Float
type Velocity = (Float, Float)
type Damage = Float
type HaveExploded = Bool
type Cooldown = Float
type TurretAction = Float
type Duration = Float

data MundoGloss = MundoGloss
  { worldState     :: World
  , modo           :: Modo
  , imagenInicio   :: Picture
  , fondoJuego     :: Picture
  , imagenVictoria :: Picture
  , imagenDerrota  :: Picture
  , imagenRobot1   :: Picture
  --, imagenRobot2   :: Picture
  --, imagenRobot3   :: Picture
  --, imagenRobot4   :: Picture
  , imagenTorreta  :: Picture
  , explosiones    :: [Explosion]
  }

data Modo = Inicio | Jugando | Victoria Int | Derrota deriving (Eq, Show)

-- Información común de cualquier entidad con posición, velocidad y forma.
-- Parametrizado en 'a' para poder aplicar Functor y Applicative.
data CommonData a = CommonData
  { id       :: Id
  , damage   :: Damage
  , position :: (a, a)   -- posición como coordenadas (x, y)
  , velocity :: (a, a)   -- velocidad también como vector (vx, vy)
  , size     :: Size
  , points   :: [(a, a)] -- lista de puntos
  } deriving (Show, Eq)

-- FUNCTOR: aplica una función a todos los campos numéricos.
instance Functor CommonData where
  fmap f (CommonData i d (px, py) (vx, vy) s pts) =
    CommonData i d (f px, f py) (f vx, f vy) s (map (\(x, y) -> (f x, f y)) pts)

instance Applicative CommonData where
  pure x = CommonData 0 0 (x, x) (x, x) (0, 0) [(x, x)]
  (CommonData i d (fx, fy) (fvx, fvy) s _) <*> (CommonData _ _ (px, py) (vx, vy) _ _) =
    CommonData i d (fx px, fy py) (fvx vx, fvy vy) s []


-- Datos del proyectil
data Projectile = Projectile
  { idP        :: Id
  , commonP    :: CommonData Float
  , rangeP     :: Distance
  } deriving (Show, Eq)

-- Datos de la torreta
data Turret = Turret
  { idT          :: Id
  , vectorT      :: Vector
  , angleT       :: Angle
  , projectileT  :: Projectile
  , cooldown        :: Cooldown -- Cooldown
  } deriving (Show, Eq)

-- Acciones posibles de un robot
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

-- Estado global del mundo
data World = World
  { robots      :: [Robot]
  , projectiles :: [Projectile]
  , turrets     :: [Turret]
  , robotHits   :: [RobotHit]
  } deriving (Show, Eq)

-- Información de una explosión
data Explosion = Explosion
  { positionE   :: Position
  , sizeE       :: Size
  , durationE   :: Duration
  , source     :: RobotHit -- Indicamos de donde ha venido la explosion
  } deriving (Show, Eq)


-- Velocidad base por defecto
baseSpeed :: Float
baseSpeed = 5.0

