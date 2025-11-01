module Data.Explosion where
import Data.DatosComunes  
import Data.Robot   


-- Información de una explosión
data Explosion = Explosion
  { positionE   :: Position
  , sizeE       :: Size
  , durationE   :: Duration
  , source     :: RobotHit -- Indicamos de donde ha venido la explosion
  , idA   :: Id  -- Si la explosion es de un alien, guardamos su id
  , idB   :: Id  -- Si la explosion es de un robot, guardamos su id
  } deriving (Show, Eq)