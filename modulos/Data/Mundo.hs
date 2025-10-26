module Data.Mundo where
import Graphics.Gloss hiding (Vector, Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector, Point)
import Data.DatosComunes
import Data.Explosion
import Data.Robot
import Data.Proyectil
import Data.Torreta

data MundoGloss = MundoGloss
  { worldState     :: World
  , modo           :: Modo
  , imagenInicio   :: Picture
  , fondoJuego     :: Picture
  , imagenVictoria :: Picture
  , imagenDerrota  :: Picture
  , imagenRobot1   :: Maybe Picture
  , imagenRobot2   :: Maybe Picture
  , imagenRobot3   :: Maybe Picture
  , imagenRobot4   :: Maybe Picture
  , imagenTorreta  :: Maybe Picture
  , imagenProfe    :: Maybe Picture
  , imagenProyectil  :: Maybe Picture
  , imagenExplosion1 :: Maybe Picture
  , imagenExplosion2 :: Maybe Picture
  , imagenExplosion3 :: Maybe Picture
  , imagenExplosionMuerte :: Maybe Picture
  , explosiones    :: [Explosion]
  }

data Modo = Inicio | Jugando | Victoria Int | Derrota deriving (Eq, Show)

-- Estado global del mundo
data World = World
  { robots      :: [Robot]
  , projectiles :: [Projectile]
  , turrets     :: [Turret]
  , robotHits   :: [RobotHit]
  } deriving (Show, Eq)

ancho, alto :: Float
ancho = 600
alto  = 600