
module Game.Memory
  ( MemoryValue(..)
  , Memory
  , emptyMemory
  , setMemory
  , getMemory
  ) where

import Game.Entities (Point)
import qualified Data.Map as M
import GHC.Generics (Generic)

-- Valores que un agente puede recordar
data MemoryValue
  = memInt    Int
  | memFloat  Float
  | memString String
  | memPoint  Point
  | memBool   Bool
  deriving (Show, Eq, Generic)

-- Diccionario de memoria
type Memory = M.Map String MemoryValue
