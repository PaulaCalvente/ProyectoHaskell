module Data.Explosion where
import Data.DatosComunes  
import Data.Robot   


-- Información de una explosión
data Explosion = Explosion
  { positionE   :: Position
  , sizeE       :: Size
  , durationE   :: Duration
  , source     :: RobotHit -- Indicamos de donde ha venido la explosion
  } deriving (Show, Eq)