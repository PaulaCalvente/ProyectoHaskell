module Data.Proyectil where
import Data.DatosComunes
-- Datos del proyectil
data Projectile = Projectile
  { idP        :: Id
  , commonP    :: CommonData Float
  , rangeP     :: Distance
  } deriving (Show, Eq)

velProjectile :: Float
velProjectile = 300

projectileRadius :: Float
projectileRadius = 10

-- Extracción de tipos comunes del proyectil
damageP :: Projectile -> Damage
damageP = damage . commonP

positionP :: Projectile -> Position
positionP = position . commonP

velocityP :: Projectile -> Velocity
velocityP = velocity . commonP

sizeP :: Projectile -> Size
sizeP = size . commonP

pointsP :: Projectile -> [Point]
pointsP = points . commonP

proyectilBase :: Id -> Projectile
proyectilBase i = Projectile
  { idP = i
  , commonP = CommonData i 10 (0, 0) (0, 0) (projectileRadius*2, projectileRadius*2) []
  , rangeP = 1000
  }