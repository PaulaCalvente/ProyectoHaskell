module Data.Mundo where
import Graphics.Gloss hiding (Vector, Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector, Point)
import Data.DatosComunes
import Data.Explosion
import Data.Robot
import Data.Proyectil
import Data.Torreta

-- Data/Mundo.hs
data MundoGloss = MundoGloss
  {worldState     :: World
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
  , imagenEscritorio :: Maybe Picture
  , imagenSandwich :: Maybe Picture
  , imagenZumo     :: Maybe Picture
  , imagenPlatano  :: Maybe Picture
  , imagenExplosionComida :: Maybe Picture  -- << NUEVO
  , posSandwich1 :: (Float, Float)
  , posSandwich2 :: (Float, Float)
  , posZumo1     :: (Float, Float)
  , posZumo2     :: (Float, Float)
  , posPlatano1  :: (Float, Float)
  , posPlatano2  :: (Float, Float)
  -- >>> NUEVOS: estado de actividad
  , sandwich1Activo :: Bool
  , sandwich2Activo :: Bool
  , zumo1Activo     :: Bool
  , zumo2Activo     :: Bool
  , platano1Activo  :: Bool
  , platano2Activo  :: Bool
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