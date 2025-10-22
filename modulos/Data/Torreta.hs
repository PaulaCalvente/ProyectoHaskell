module Data.Torreta where
import Data.DatosComunes
import Data.Proyectil

-- Datos de la torreta
data Turret = Turret
  { idT          :: Id
  , vectorT      :: Vector
  , angleT       :: Angle
  , projectileT  :: Projectile
  , cooldown        :: Cooldown -- Cooldown
  } deriving (Show, Eq)