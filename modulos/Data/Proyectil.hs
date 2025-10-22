module Data.Proyectil where
import Data.DatosComunes
-- Datos del proyectil
data Projectile = Projectile
  { idP        :: Id
  , commonP    :: CommonData Float
  , rangeP     :: Distance
  } deriving (Show, Eq)
